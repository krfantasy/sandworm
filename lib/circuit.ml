(* Circuit elaboration: netlist -> flat simulation-ready circuit.
   Pipeline: parse -> prescan .GLOBAL -> .INCLUDE splice -> extract .SUBCKT ->
   expand X instances -> validate -> elaborate typed devices with final
   .PARAM/.MODEL/.OPTIONS tables. Raises Elab_error with a clear message. *)

open Ast

exception Elab_error of string

let elab_errorf fmt = Printf.ksprintf (fun s -> raise (Elab_error s)) fmt

(* ------------------------------------------------------------------ *)
(* Transient source waveforms                                          *)
(* ------------------------------------------------------------------ *)

type tran_wave =
  | Pulse of {
      v1 : float;
      v2 : float;
      td : float;
      tr : float;
      tf : float;
      pw : float;
      per : float;
    }
  | Sine of {
      voff : float;
      vamp : float;
      freq : float;
      td : float;
      theta : float;
      phase : float;
    }
  | Pwl of (float * float) list
  | Exp of {
      v1 : float;
      v2 : float;
      td1 : float;
      tau1 : float;
      td2 : float;
      tau2 : float;
    }

type source_ac = {
  mag : float;
  phase_deg : float;
}

(* ------------------------------------------------------------------ *)
(* Flat elements (integer node ids; -1 is ground)                       *)
(* ------------------------------------------------------------------ *)

type element =
  | R of {
      name : string;
      a : int;
      b : int;
      r : float;
    }
  | C of {
      name : string;
      a : int;
      b : int;
      c : float;
    }
  | L of {
      name : string;
      a : int;
      b : int;
      l : float;
      br : int;
    }
  | Vsrc of {
      name : string;
      a : int;
      b : int;
      dc : float;
      ac : source_ac;
      wave : tran_wave option;
      br : int;
    }
  | Isrc of {
      name : string;
      a : int;
      b : int;
      dc : float;
      ac : source_ac;
      wave : tran_wave option;
    }
  | Vcvs of {
      name : string;
      ap : int;
      an : int;
      cp : int;
      cn : int;
      gain : float;
      br : int;
    }
  | Vccs of {
      name : string;
      ap : int;
      an : int;
      cp : int;
      cn : int;
      gain : float;
    }
  | Cccs of {
      name : string;
      a : int;
      b : int;
      ctrl : int;
      gain : float;
    }
  | Ccvs of {
      name : string;
      a : int;
      b : int;
      ctrl : int;
      gain : float;
      br : int;
    }
  | Dio of {
      name : string;
      a : int;
      b : int;
      model : Models.diode_model;
      area : float;
    }
  | Bjt of {
      name : string;
      c : int;
      b : int;
      e : int;
      model : Models.bjt_model;
      area : float;
      pnp : bool;
    }
  | Mos of {
      name : string;
      d : int;
      g : int;
      s : int;
      bulk : int;
      model : Models.mos_model;
      w : float;
      l : float;
    }
  | Jfet of {
      name : string;
      d : int;
      g : int;
      s : int;
      model : Models.jfet_model;
      area : float;
      pchannel : bool;
    }
  | Bv of {
      name : string;
      a : int;
      b : int;
      expr : expr;
      br : int;
    }
  | Bi of {
      name : string;
      a : int;
      b : int;
      expr : expr;
    }
  | Vsw of {
      name : string;
      a : int;
      b : int;
      ca : int;
      cb : int;
      model : Models.switch_model;
    }
  | Isw of {
      name : string;
      a : int;
      b : int;
      ctrl : int;
      model : Models.switch_model;
    }

type dc_sweep = {
  src : string;
  start : float;
  stop : float;
  step : float;
}

type analysis =
  | Op
  | Dc of dc_sweep list
  | Ac of {
      sweep : ac_sweep;
      npts : float;
      fstart : float;
      fstop : float;
    }
  | Tran of {
      tstep : float;
      tstop : float;
      tstart : float;
      uic : bool;
    }

type circuit = {
  title : string;
  elements : element list;
  n_nodes : int;
  node_names : string array;
  n_branches : int;
  branch_names : string array;
  inductor_l : (int * float) list;
  couplings : (int * int * float) list;
  params : (string * float) list;
  models : Models.model_table;
  options : Models.sim_options;
  analyses : analysis list;
  save : string list;
}

(* ------------------------------------------------------------------ *)
(* Small helpers                                                       *)
(* ------------------------------------------------------------------ *)

type subckt_def = {
  ports : string list;
  body : statement list;
}

let norm_node s = String.lowercase_ascii s
let is_ground s = s = "0"
let norm_name s = String.uppercase_ascii s
let is_x_name n = String.length n > 0 && Char.lowercase_ascii n.[0] = 'x'

let const_eval params what e =
  try Eval.eval_const params e
  with Eval.Eval_error msg -> elab_errorf "%s: %s" what msg

(* Substitute subcircuit actual-parameter exprs for formal EVar names. *)
let rec subst_expr subst = function
  | EVar v as e -> (
      match
        List.find_opt
          (fun (k, _) -> String.uppercase_ascii k = String.uppercase_ascii v)
          subst
      with
      | Some (_, repl) -> repl
      | None -> e)
  | ENum _ as e -> e
  | EStr _ as e -> e
  | ECall (f, args) -> ECall (f, List.map (subst_expr subst) args)
  | EUnary (op, e) -> EUnary (op, subst_expr subst e)
  | EBinop (op, a, b) -> EBinop (op, subst_expr subst a, subst_expr subst b)
  | ETernary (c, t, f) -> ETernary (c, subst_expr subst t, subst_expr subst f)
  | EIndex (a, i) -> EIndex (subst_expr subst a, subst_expr subst i)

let subst_arg subst = function
  | KwParam (k, e) -> KwParam (k, subst_expr subst e)
  | PosFunc (f, args) -> PosFunc (f, List.map (subst_expr subst) args)
  | other -> other

(* ------------------------------------------------------------------ *)
(* Generic-stage scope mapping (pre-validation)                        *)
(* ------------------------------------------------------------------ *)

(* Map one node reference through the current instance scope. *)
let map_node ~port_map ~is_global ~pfx n =
  if is_ground (norm_node n) || is_global (norm_node n) then n
  else
    match List.find_opt (fun (p, _) -> norm_node p = norm_node n) port_map with
    | Some (_, actual) -> actual
    | None -> pfx ^ n

(* Remap V()/I() references inside B-source expressions. *)
let rec map_b_expr ~map_node ~map_dev = function
  | ECall (f, args) when String.lowercase_ascii f = "v" ->
      ECall
        ( f,
          List.map
            (function
              | EVar n -> EVar (map_node n)
              | ENum _ as e -> e
              | e -> map_b_expr ~map_node ~map_dev e)
            args )
  | ECall (f, args)
    when String.lowercase_ascii f = "i" || String.lowercase_ascii f = "ib" ->
      ECall
        ( f,
          List.map
            (function
              | EVar n -> EVar (map_dev n)
              | e -> map_b_expr ~map_node ~map_dev e)
            args )
  | ECall (f, args) -> ECall (f, List.map (map_b_expr ~map_node ~map_dev) args)
  | EUnary (op, e) -> EUnary (op, map_b_expr ~map_node ~map_dev e)
  | EBinop (op, a, b) -> EBinop (op, map_b_expr ~map_node ~map_dev a, map_b_expr ~map_node ~map_dev b)
  | ETernary (c, t, e) ->
      ETernary (map_b_expr ~map_node ~map_dev c, map_b_expr ~map_node ~map_dev t, map_b_expr ~map_node ~map_dev e)
  | EIndex (a, i) -> EIndex (map_b_expr ~map_node ~map_dev a, map_b_expr ~map_node ~map_dev i)
  | e -> e

(* Per-device positional mapping: which PosId/PosNum positionals are nodes
   (remap), which are device references (prefix if local), which are model or
   keyword names (leave alone). Mirrors lib/device.ml's positional layout. *)
let remap_device ~pfx ~dpath ~port_map ~is_global ~local_devs (d : device) : device =
  let map_node = map_node ~port_map ~is_global ~pfx in
  (* Device names take a SUFFIX so the first letter (which encodes the device
     type for validation) is preserved: R1 inside X1 becomes "R1:X1". *)
  let suffix n = if dpath = "" then n else n ^ ":" ^ dpath in
  let map_dev n =
    if List.exists (fun dev -> norm_name dev = norm_name n) local_devs then suffix n else n
  in
  let is_node_arg = function
    | PosId _ -> true
    | PosNum n -> (
        match Device.arg_to_node (PosNum n) with
        | Some _ -> true
        | None -> false)
    | _ -> false
  in
  (* Split positionals into leading nodes (count determined per device kind),
     with special handling for kinds with trailing node/model layouts. *)
  let pos, kw =
    List.partition (function KwParam _ -> true | _ -> false) d.args
    |> fun (k, p) -> (p, k)
  in
  let kind = if String.length d.name > 0 then Char.lowercase_ascii d.name.[0] else '?' in
  let map_first_n n args =
    let rec aux i = function
      | [] -> []
      | a :: rest ->
          if i < n && is_node_arg a then
            (match a with
            | PosId s -> PosId (map_node s)
            | PosNum _ as x -> (
                match Device.arg_to_node x with
                | Some s ->
                    let mapped = map_node s in
                    if mapped = s then x else PosId mapped
                | None -> x)
            | x -> x)
            :: aux (i + 1) rest
          else a :: aux i rest
    in
    aux 0 args
  in
  let pos =
    match kind with
    | 'r' | 'c' | 'l' | 'd' | 'v' | 'i' | 'b' -> map_first_n 2 pos
    | 'q' -> (
        (* 3 nodes + optional substrate/temp nodes before the model name *)
        let mapped = map_first_n 3 pos in
        (* find last PosId (= model); map node-like args before it *)
        let last_id =
          let ids =
            List.filter_map (function PosId s -> Some s | _ -> None) mapped
          in
          match List.rev ids with
          | m :: _ -> Some m
          | [] -> None
        in
        (match last_id with
        | None -> mapped
        | Some m ->
            List.map
              (function
                | PosId s when norm_name s <> norm_name m && is_node_arg (PosId s) ->
                    (* substrate/temp node only if it precedes the model *)
                    PosId s (* position check below *)
                | a -> a)
              mapped
            |> fun l ->
            (* walk up to (excluding) the model PosId, mapping nodes *)
            let rec aux seen_model = function
              | [] -> []
              | (PosId s as a) :: rest when norm_name s = norm_name m ->
                  a :: aux true rest
              | a :: rest when not seen_model && is_node_arg a -> (
                  (match a with
                  | PosId s -> PosId (map_node s)
                  | PosNum _ as x -> (
                      match Device.arg_to_node x with
                      | Some s ->
                          let mapped = map_node s in
                          if mapped = s then x else PosId mapped
                      | None -> x)
                  | x -> x)
                  :: aux false rest)
              | a :: rest -> a :: aux seen_model rest
            in
            aux false l))
    | 'j' | 'z' -> map_first_n 3 pos
    | 'm' -> map_first_n 4 pos
    | 'e' | 'g' -> map_first_n 4 pos
    | 'f' | 'h' -> (
        let mapped = map_first_n 2 pos in
        let rec aux done_ctrl = function
          | [] -> []
          | (PosId s as a) :: rest when not done_ctrl ->
              let skip =
                String.lowercase_ascii s = "dc" || String.lowercase_ascii s = "ac"
              in
              if skip then a :: aux false rest else PosId (map_dev s) :: aux true rest
          | a :: rest -> a :: aux done_ctrl rest
        in
        aux false mapped)
    | 's' -> map_first_n 4 pos
    | 'w' -> (
        let mapped = map_first_n 2 pos in
        let rec aux done_ctrl = function
          | [] -> []
          | (PosId s) :: rest when not done_ctrl -> PosId (map_dev s) :: aux true rest
          | a :: rest -> a :: aux done_ctrl rest
        in
        aux false mapped)
    | 't' -> map_first_n 4 pos
    | 'k' ->
        List.map
          (function
            | PosId s -> PosId (map_dev s)
            | a -> a)
          pos
    | 'x' -> pos (* handled by instance expansion *)
    | _ -> pos
  in
  let kw =
    List.map
      (function
        | KwParam (k, e)
          when kind = 'b' && (String.uppercase_ascii k = "V" || String.uppercase_ascii k = "I") ->
            KwParam (k, map_b_expr ~map_node ~map_dev e)
        | a -> a)
      kw
  in
  { d with name = (if dpath = "" then d.name else d.name ^ ":" ^ dpath); args = pos @ kw }

(* ------------------------------------------------------------------ *)
(* Statement-level passes: include splice, subckt extract + expand      *)
(* ------------------------------------------------------------------ *)

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* Splice .INCLUDE files (generic stage). *)
let rec splice_includes ~basedir ~depth (stmts : statement list) : statement list =
  if depth > 8 then raise (Elab_error ".INCLUDE nesting too deep");
  List.concat_map
    (function
      | DotCmd (Include f) -> (
          let path =
            if Filename.is_relative f then Filename.concat basedir f else f
          in
          let content =
            try read_file path
            with Sys_error msg -> elab_errorf ".INCLUDE '%s': %s" f msg
          in
          let sub =
            try Netlist.parse_string content
            with Netlist.Parse_error msg -> elab_errorf ".INCLUDE '%s': %s" f msg
          in
          splice_includes ~basedir:(Filename.dirname path) ~depth:(depth + 1) sub)
      | s -> [ s ])
    stmts

(* Extract .SUBCKT..ENDS bodies; stop at .END. *)
let extract_subckts (stmts : statement list) : (string * subckt_def) list * statement list =
  let defs = ref [] in
  let top = ref [] in
  let cur = ref None in
  let stop = ref false in
  List.iter
    (fun s ->
      if !stop then ()
      else
        match s with
        | DotCmd End -> stop := true
        | DotCmd (Subckt (name, ports)) -> (
            match !cur with
            | Some _ -> raise (Elab_error "nested .SUBCKT definitions are not supported")
            | None -> cur := Some (name, ports, ref []))
        | DotCmd (Ends _) -> (
            match !cur with
            | None -> () (* stray .ENDS at top level: ignore *)
            | Some (name, ports, body) ->
                defs := (norm_name name, { ports; body = List.rev !body }) :: !defs;
                cur := None)
        | _ -> (
            match !cur with
            | Some (_, _, body) -> body := s :: !body
            | None -> top := s :: !top))
    stmts;
  (match !cur with
  | Some (name, _, _) -> elab_errorf "unterminated .SUBCKT %s: missing .ENDS" name
  | None -> ());
  (!defs, List.rev !top)

(* Prescan all statements (including bodies) for .GLOBAL nodes. *)
let prescan_globals defs top =
  let g = ref [ "0" ] in
  let scan = function
    | DotCmd (Global ids) ->
        List.iter (fun n -> g := norm_node n :: !g) ids
    | _ -> ()
  in
  List.iter scan top;
  List.iter (fun (_, d) -> List.iter scan d.body) defs;
  let tbl = Hashtbl.create 8 in
  List.iter (fun n -> Hashtbl.replace tbl n ()) !g;
  Hashtbl.mem tbl

(* Expand X instances. *)
let expand_subckts defs ~is_global (stmts : statement list) : statement list =
  let max_depth = 16 in
  let rec expand_list ~depth ~stack ~pfx ~dpath ~port_map ~subst (stmts : statement list) =
    if depth > max_depth then
      raise (Elab_error "subcircuit expansion too deep (recursive .SUBCKT?)");
    let local_devs = List.filter_map (function Device d -> Some d.name | _ -> None) stmts in
    List.concat_map (expand_stmt ~depth ~stack ~pfx ~dpath ~port_map ~subst ~local_devs) stmts
  and expand_stmt ~depth ~stack ~pfx ~dpath ~port_map ~subst ~local_devs = function
    | DotCmd (Param plist) ->
        (* scoped .PARAM: keep textual, append instance subst *)
        let plist = List.map (fun (k, e) -> (k, subst_expr subst e)) plist in
        [ DotCmd (Param plist) ]
    | DotCmd (Model _) when depth > 0 ->
        raise (Elab_error ".MODEL inside .SUBCKT is not supported; move it to top level")
    | DotCmd (Options _) when depth > 0 ->
        raise (Elab_error ".OPTIONS inside .SUBCKT is not supported; move it to top level")
    | DotCmd (Op | Ac _ | Tran _ | Dc _) when depth > 0 -> [] (* analyses inside subckts ignored *)
    | DotCmd (DotRaw _) as s -> [ s ]
    | DotCmd _ as s -> [ s ]
    | Device d ->
        let d = { d with args = List.map (subst_arg subst) d.args } in
        if is_x_name d.name then expand_instance ~depth ~stack ~pfx ~dpath ~port_map ~subst d
        else [ Device (remap_device ~pfx ~dpath ~port_map ~is_global ~local_devs d) ]
  and expand_instance ~depth ~stack ~pfx ~dpath ~port_map ~subst (d : device) =
    let pos = List.filter_map (function KwParam _ -> None | a -> Some a) d.args in
    let kws =
      List.filter_map (function KwParam (k, e) -> Some (k, e) | _ -> None) d.args
    in
    (* Last PosId positional is the subcircuit name. *)
    let rev_pos = List.rev pos in
    let sub, node_args_rev =
      match rev_pos with
      | PosId sub :: rest -> (sub, rest)
      | _ -> elab_errorf "%s: X instance needs nodes + subcircuit name" d.name
    in
    let key = norm_name sub in
    if List.mem key stack then elab_errorf "recursive subcircuit instantiation of %s" sub;
    let def =
      match List.find_opt (fun (k, _) -> k = key) defs with
      | Some (_, d) -> d
      | None -> elab_errorf "%s: unknown subcircuit '%s'" d.name sub
    in
    (* Actual node args are in current-scope naming: map them first. *)
    let map_node = map_node ~port_map ~is_global ~pfx in
    let actual_of = function
      | PosId s -> map_node s
      | PosNum n -> (
          match Device.arg_to_node (PosNum n) with
          | Some s -> map_node s
          | None -> elab_errorf "%s: bad node value" d.name)
      | _ -> elab_errorf "%s: subcircuit actual must be a node" d.name
    in
    let actuals = List.rev_map actual_of node_args_rev in
    if List.length actuals <> List.length def.ports then
      elab_errorf "%s: subcircuit %s expects %d nodes, got %d" d.name sub
        (List.length def.ports) (List.length actuals);
    let child_ports = List.combine def.ports actuals in
    let child_pfx = pfx ^ d.name ^ ":" in
    let child_dpath = if dpath = "" then d.name else d.name ^ ":" ^ dpath in
    (* Instance actuals are already scope-mapped; keep textual for subst. *)
    let child_subst = kws @ subst in
    expand_list ~depth:(depth + 1) ~stack:(key :: stack) ~pfx:child_pfx ~dpath:child_dpath
      ~port_map:child_ports ~subst:child_subst def.body
  in
  expand_list ~depth:0 ~stack:[] ~pfx:"" ~dpath:"" ~port_map:[] ~subst:[] stmts

(* ------------------------------------------------------------------ *)
(* Elaboration environment                                             *)
(* ------------------------------------------------------------------ *)

type env = {
  params : (string * float) list;
  models : Models.model_table;
  options : Models.sim_options;
  node_tbl : (string, int) Hashtbl.t;
  mutable next_node : int;
  node_names : string list ref;
  branches : (string, int) Hashtbl.t;
  mutable next_branch : int;
  branch_names : string list ref;
  inductors : (int * float) list ref;
  raw_k : (string * string list * float option) list ref;
  save : string list ref;
  analyses : analysis list ref;
}

let node_id env name =
  let n = norm_node name in
  if is_ground n then -1
  else
    match Hashtbl.find_opt env.node_tbl n with
    | Some i -> i
    | None ->
        let i = env.next_node in
        env.next_node <- i + 1;
        Hashtbl.add env.node_tbl n i;
        env.node_names := n :: !(env.node_names);
        i

let branch_id env name =
  let key = norm_name name in
  match Hashtbl.find_opt env.branches key with
  | Some i -> i
  | None ->
      let i = env.next_branch in
      env.next_branch <- i + 1;
      Hashtbl.add env.branches key i;
      env.branch_names := key :: !(env.branch_names);
      i

let const_ env what e = const_eval env.params what e
let const_opt env what = function
  | None -> None
  | Some e -> Some (const_ env what e)

let value_or_param env what value_opt params keys model_opt =
  match value_opt with
  | Some e -> const_ env what e
  | None -> (
      let found =
        List.find_map
          (fun k ->
            List.find_map
              (fun (pk, pe) ->
                if String.uppercase_ascii pk = k then Some (const_ env what pe) else None)
              params)
          keys
      in
      match found with
      | Some v -> v
      | None -> (
          (* Bare .PARAM name in the value slot parses as a model reference;
             prefer a real model, else fall back to the parameter. *)
          match model_opt with
          | Some m when Models.find_opt env.models m <> None ->
              elab_errorf "%s: missing value" what
          | Some m -> (
              match List.find_opt (fun (k, _) -> norm_name k = norm_name m) env.params with
              | Some (_, v) -> v
              | None -> elab_errorf "%s: missing value" what)
          | None -> elab_errorf "%s: missing value" what))

let find_model env what name =
  match Models.find_opt env.models name with
  | Some m -> m
  | None -> elab_errorf "%s: unknown model '%s'" what name

(* Parse transient function spec from a validated source. *)
let parse_wave env what = function
  | None -> None
  | Some (fname, exprs) ->
      let args = List.map (const_ env (what ^ ": transient args")) exprs in
      let nth i default = try List.nth args i with Failure _ | Invalid_argument _ -> default in
      let wave =
        match String.lowercase_ascii fname with
        | "pulse" ->
            if List.length args < 2 then elab_errorf "%s: PULSE needs at least V1 V2" what;
            Pulse
              {
                v1 = nth 0 0.0;
                v2 = nth 1 0.0;
                td = nth 2 0.0;
                tr = nth 3 0.0;
                tf = nth 4 0.0;
                pw = nth 5 Float.infinity;
                per = nth 6 Float.infinity;
              }
        | "sin" ->
            if List.length args < 2 then elab_errorf "%s: SIN needs at least VOFF VAMPL" what;
            Sine
              {
                voff = nth 0 0.0;
                vamp = nth 1 0.0;
                freq = nth 2 0.0;
                td = nth 3 0.0;
                theta = nth 4 0.0;
                phase = nth 5 0.0;
              }
        | "pwl" ->
            if List.length args < 2 || List.length args mod 2 <> 0 then
              elab_errorf "%s: PWL needs even time/value pairs" what;
            let rec pairs = function
              | t :: v :: rest -> (t, v) :: pairs rest
              | [] -> []
              | _ -> assert false
            in
            Pwl (pairs args)
        | "exp" ->
            if List.length args < 2 then elab_errorf "%s: EXP needs at least V1 V2" what;
            Exp
              {
                v1 = nth 0 0.0;
                v2 = nth 1 0.0;
                td1 = nth 2 0.0;
                tau1 = nth 3 0.0;
                td2 = nth 4 0.0;
                tau2 = nth 5 0.0;
              }
        | other -> elab_errorf "%s: unsupported transient function '%s'" what other
      in
      Some wave

let source_ac_of env what (s : Device.source) =
  let mag =
    match s.ac_mag with
    | None -> 0.0
    | Some e -> const_ env (what ^ ": AC magnitude") e
  in
  let phase =
    match s.ac_phase with
    | None -> 0.0
    | Some e -> const_ env (what ^ ": AC phase") e
  in
  { mag; phase_deg = phase }

let is_transient_call = function
  | ECall (f, _) -> List.mem (String.lowercase_ascii f) [ "pulse"; "sin"; "pwl"; "exp"; "sffm" ]
  | _ -> false

let source_dc_of env what (s : Device.source) =
  match s.dc with
  | None -> 0.0
  (* A bare transient function ("V1 n+ n- PULSE(...)") carries no DC value;
     SPICE uses 0 for the operating point. *)
  | Some e when is_transient_call e -> 0.0
  | Some e -> const_ env (what ^ ": DC value") e

(* ------------------------------------------------------------------ *)
(* Per-device elaboration                                              *)
(* ------------------------------------------------------------------ *)

let elab_gain env what = function
  | None -> elab_errorf "%s: missing gain" what
  | Some e -> const_ env (what ^ ": gain") e

let elaborate_device env : Device.typed_device -> element = function
  | Device.Resistor r ->
      let v = value_or_param env r.name r.value r.params [ "R"; "RES"; "VALUE" ] r.model in
      if v = 0.0 then elab_errorf "%s: resistance is zero" r.name;
      R { name = r.name; a = node_id env r.pos_node; b = node_id env r.neg_node; r = v }
  | Device.Capacitor c ->
      let v = value_or_param env c.name c.value c.params [ "C"; "CAP"; "VALUE" ] c.model in
      C { name = c.name; a = node_id env c.pos_node; b = node_id env c.neg_node; c = v }
  | Device.Inductor l ->
      let v = value_or_param env l.name l.value l.params [ "L"; "IND"; "VALUE" ] l.model in
      let br = branch_id env l.name in
      env.inductors := (br, v) :: !(env.inductors);
      L { name = l.name; a = node_id env l.pos_node; b = node_id env l.neg_node; l = v; br }
  | Device.Voltage_source s ->
      let br = branch_id env s.name in
      Vsrc
        {
          name = s.name;
          a = node_id env s.pos_node;
          b = node_id env s.neg_node;
          dc = source_dc_of env s.name s;
          ac = source_ac_of env s.name s;
          wave = parse_wave env s.name s.transient;
          br;
        }
  | Device.Current_source s ->
      Isrc
        {
          name = s.name;
          a = node_id env s.pos_node;
          b = node_id env s.neg_node;
          dc = source_dc_of env s.name s;
          ac = source_ac_of env s.name s;
          wave = parse_wave env s.name s.transient;
        }
  | Device.Vcvs v ->
      Vcvs
        {
          name = v.name;
          ap = node_id env v.out_pos;
          an = node_id env v.out_neg;
          cp = node_id env v.in_pos;
          cn = node_id env v.in_neg;
          gain = elab_gain env v.name v.gain;
          br = branch_id env v.name;
        }
  | Device.Vccs v ->
      Vccs
        {
          name = v.name;
          ap = node_id env v.out_pos;
          an = node_id env v.out_neg;
          cp = node_id env v.in_pos;
          cn = node_id env v.in_neg;
          gain = elab_gain env v.name v.gain;
        }
  | Device.Cccs c ->
      let ctrl =
        match Hashtbl.find_opt env.branches (norm_name c.controlling_source) with
        | Some b -> b
        | None -> elab_errorf "%s: unknown controlling source '%s'" c.name c.controlling_source
      in
      Cccs
        {
          name = c.name;
          a = node_id env c.pos_node;
          b = node_id env c.neg_node;
          ctrl;
          gain = elab_gain env c.name c.gain;
        }
  | Device.Ccvs c ->
      let ctrl =
        match Hashtbl.find_opt env.branches (norm_name c.controlling_source) with
        | Some b -> b
        | None -> elab_errorf "%s: unknown controlling source '%s'" c.name c.controlling_source
      in
      Ccvs
        {
          name = c.name;
          a = node_id env c.pos_node;
          b = node_id env c.neg_node;
          ctrl;
          gain = elab_gain env c.name c.gain;
          br = branch_id env c.name;
        }
  | Device.Diode d -> (
      let area =
        match d.area with
        | None -> 1.0
        | Some e -> const_ env (d.name ^ ": area") e
      in
      match find_model env d.name d.model with
      | Models.DModel m ->
          Dio
            {
              name = d.name;
              a = node_id env d.pos_node;
              b = node_id env d.neg_node;
              model = m;
              area;
            }
      | Models.OtherModel t -> elab_errorf "%s: model '%s' is type %s, not D" d.name d.model t
      | _ -> elab_errorf "%s: model '%s' is not a diode model" d.name d.model)
  | Device.Bjt q -> (
      let area =
        match q.area with
        | None -> 1.0
        | Some e -> const_ env (q.name ^ ": area") e
      in
      match find_model env q.name q.model with
      | Models.BjtModel m ->
          Bjt
            {
              name = q.name;
              c = node_id env q.collector;
              b = node_id env q.base;
              e = node_id env q.emitter;
              model = m;
              area;
              pnp = m.Models.b_pnp;
            }
      | Models.OtherModel t -> elab_errorf "%s: model '%s' is type %s, not NPN/PNP" q.name q.model t
      | _ -> elab_errorf "%s: model '%s' is not a BJT model" q.name q.model)
  | Device.Jfet j -> (
      let area =
        match j.area with
        | None -> 1.0
        | Some e -> const_ env (j.name ^ ": area") e
      in
      match find_model env j.name j.model with
      | Models.JfetModel m ->
          Jfet
            {
              name = j.name;
              d = node_id env j.drain;
              g = node_id env j.gate;
              s = node_id env j.source;
              model = m;
              area;
              pchannel = m.Models.j_pchan;
            }
      | Models.OtherModel t -> elab_errorf "%s: model '%s' is type %s, not NJF/PJF" j.name j.model t
      | _ -> elab_errorf "%s: model '%s' is not a JFET model" j.name j.model)
  | Device.Mesfet j -> (
      let area =
        match j.area with
        | None -> 1.0
        | Some e -> const_ env (j.name ^ ": area") e
      in
      match find_model env j.name j.model with
      | Models.JfetModel m ->
          Jfet
            {
              name = j.name;
              d = node_id env j.drain;
              g = node_id env j.gate;
              s = node_id env j.source;
              model = m;
              area;
              pchannel = false;
            }
      | Models.OtherModel t -> elab_errorf "%s: model '%s' is type %s, not a FET model" j.name j.model t
      | _ -> elab_errorf "%s: model '%s' is not a FET model" j.name j.model)
  | Device.Mosfet m -> (
      let w =
        match List.find_opt (fun (k, _) -> String.uppercase_ascii k = "W") m.params with
        | Some (_, e) -> const_ env (m.name ^ ": W") e
        | None -> env.options.Models.defw
      in
      let l =
        match List.find_opt (fun (k, _) -> String.uppercase_ascii k = "L") m.params with
        | Some (_, e) -> const_ env (m.name ^ ": L") e
        | None -> env.options.Models.defl
      in
      match find_model env m.name m.model with
      | Models.MosModel mo ->
          Mos
            {
              name = m.name;
              d = node_id env m.drain;
              g = node_id env m.gate;
              s = node_id env m.source;
              bulk = node_id env m.bulk;
              model = mo;
              w;
              l;
            }
      | Models.OtherModel t ->
          elab_errorf "%s: model '%s' is type %s, not NMOS/PMOS" m.name m.model t
      | _ -> elab_errorf "%s: model '%s' is not a MOSFET model" m.name m.model)
  | Device.Bsource b ->
      let a = node_id env b.pos_node in
      let bb = node_id env b.neg_node in
      (match (b.v_expr, b.i_expr) with
      | Some e, None -> Bv { name = b.name; a; b = bb; expr = e; br = branch_id env b.name }
      | None, Some e -> Bi { name = b.name; a; b = bb; expr = e }
      | Some _, Some _ -> elab_errorf "%s: B-source needs exactly one of V= / I=" b.name
      | None, None -> elab_errorf "%s: B-source needs V=<expr> or I=<expr>" b.name)
  | Device.Vswitch s -> (
      match find_model env s.name s.model with
      | Models.SwModel m ->
          Vsw
            {
              name = s.name;
              a = node_id env s.pos_node;
              b = node_id env s.neg_node;
              ca = node_id env s.ctrl_pos;
              cb = node_id env s.ctrl_neg;
              model = m;
            }
      | Models.OtherModel t -> elab_errorf "%s: model '%s' is type %s, not SW" s.name s.model t
      | _ -> elab_errorf "%s: model '%s' is not a switch model" s.name s.model)
  | Device.Iswitch s -> (
      let ctrl =
        match Hashtbl.find_opt env.branches (norm_name s.controlling_source) with
        | Some b -> b
        | None -> elab_errorf "%s: unknown controlling source '%s'" s.name s.controlling_source
      in
      match find_model env s.name s.model with
      | Models.SwModel m ->
          Isw
            {
              name = s.name;
              a = node_id env s.pos_node;
              b = node_id env s.neg_node;
              ctrl;
              model = m;
            }
      | Models.OtherModel t ->
          elab_errorf "%s: model '%s' is type %s, not CSW" s.name s.model t
      | _ -> elab_errorf "%s: model '%s' is not a switch model" s.name s.model)
  | Device.Tline t ->
      elab_errorf
        "%s: lossless transmission lines (T) are not supported in this version; use a lumped RLC equivalent"
        t.name
  | Device.Mutual_inductor k ->
      let coupling =
        match k.coupling with
        | None -> elab_errorf "%s: missing coupling coefficient" k.name
        | Some e -> const_ env (k.name ^ ": coupling") e
      in
      (* resolve inductor branches later (all L's may not be elaborated yet) *)
      env.raw_k := (k.name, k.inductors, Some coupling) :: !(env.raw_k);
      (* placeholder element: re-emitted as coupling only; keep an R of 0? No:
         represent K as no element. Return a zero resistor? Instead raise
         impossible: we model K purely via [couplings]. Use a dummy that the
         driver drops. *)
      R { name = k.name; a = -1; b = -1; r = Float.infinity }
  | Device.Subckt_instance x ->
      elab_errorf "%s: unexpanded subcircuit instance '%s' (unknown subcircuit '%s')" x.name
        x.name x.subckt_name
  | Device.Unknown d -> elab_errorf "%s: unsupported device type '%c'" d.name d.name.[0]

(* Parse a DotRaw "tran ..." with trailing UIC flag (parser only accepts the
   2/3-numeric forms, so UIC variants land here). *)
let tran_of_dotraw args =
  let nums =
    List.filter_map (function PosNum n -> Some n | _ -> None) args
  in
  let uic =
    List.exists
      (function
        | PosId s -> String.lowercase_ascii s = "uic"
        | _ -> false)
      args
  in
  match nums with
  | [ tstep; tstop ] -> Some (Tran { tstep; tstop; tstart = 0.0; uic })
  | [ tstep; tstop; tstart ] -> Some (Tran { tstep; tstop; tstart; uic })
  | _ -> None

(* ------------------------------------------------------------------ *)
(* Top-level entry points                                              *)
(* ------------------------------------------------------------------ *)

let elaborate_statements ~title (stmts : statement list) : circuit =
  (* 1. collect top-level .PARAM exprs (textual, in order) *)
  let param_exprs =
    List.filter_map
      (function
        | DotCmd (Param plist) -> Some plist
        | _ -> None)
      stmts
  in
  let params =
    List.fold_left
      (fun acc plist ->
        List.fold_left
          (fun acc (k, e) ->
            let v =
              try Eval.eval_const acc e
              with Eval.Eval_error msg ->
                elab_errorf ".PARAM %s: %s" k msg
            in
            (k, v) :: List.remove_assoc k acc)
          acc plist)
      [] param_exprs
  in
  (* 2. models *)
  let models =
    List.fold_left
      (fun tbl -> function
        | DotCmd (Model (name, mtype, mparams)) ->
            let entry =
              try Models.entry_of_model ~const_params:params ~name ~mtype ~mparams
              with Models.Model_error msg -> elab_errorf ".MODEL %s: %s" name msg
            in
            Models.add tbl name entry
        | _ -> tbl)
      [] stmts
  in
  (* 3. options *)
  let option_cards =
    List.filter_map
      (function
        | DotCmd (Options cards) -> Some cards
        | _ -> None)
      stmts
  in
  let options = Models.options_of_cards ~const_params:params (List.concat option_cards) in
  (* 4. analyses + save *)
  let analyses = ref [] in
  let save = ref [] in
  List.iter
    (function
      | DotCmd Op -> analyses := Op :: !analyses
      | DotCmd (Ac (sw, n, fs, fe)) -> analyses := Ac { sweep = sw; npts = n; fstart = fs; fstop = fe } :: !analyses
      | DotCmd (Tran (a, b, c)) ->
          analyses := Tran { tstep = a; tstop = b; tstart = Option.value ~default:0.0 c; uic = false } :: !analyses
      | DotCmd (Dc sweeps) ->
          analyses :=
            Dc
              (List.map
                 (fun (src, st, sp, s) -> { src; start = st; stop = sp; step = s })
                 sweeps)
            :: !analyses
      | DotCmd (Save ids) -> save := !save @ ids
      | DotCmd (DotRaw (cmd, args)) when String.lowercase_ascii cmd = "tran" -> (
          match tran_of_dotraw args with
          | Some a -> analyses := a :: !analyses
          | None -> ())
      | DotCmd (Lib _) -> raise (Elab_error ".LIB is not supported; use .INCLUDE instead")
      | _ -> ())
    stmts;
  if !analyses = [] then analyses := [ Op ];
  let analyses = List.rev !analyses in
  (* 5. elaborate devices *)
  let env =
    {
      params;
      models;
      options;
      node_tbl = Hashtbl.create 32;
      next_node = 0;
      node_names = ref [];
      branches = Hashtbl.create 16;
      next_branch = 0;
      branch_names = ref [];
      inductors = ref [];
      raw_k = ref [];
      save = ref !save;
      analyses = ref analyses;
    }
  in
  let elements =
    List.filter_map
      (function
        | DotCmd _ -> None
        | Device d -> (
            match Device.validate_device d with
            | Error e -> raise (Elab_error (Device.format_error e))
            | Ok td -> Some (elaborate_device env td)))
      stmts
  in
  (* drop K placeholder elements (R with infinite value on ground) *)
  let elements =
    List.filter
      (function
        | R { r; a = -1; b = -1; _ } when r = Float.infinity -> false
        | _ -> true)
      elements
  in
  (* 6. resolve couplings to inductor branches *)
  let l_by_name =
    List.filter_map
      (function
        | L { name; br; l; _ } -> Some (norm_name name, (br, l))
        | Vsrc _ | Isrc _ | Vcvs _ | Ccvs _ -> None
        | _ -> None)
      elements
  in
  let _ =
    List.iter
      (function
        | Vsrc { name; br; _ } -> Hashtbl.replace env.branches (norm_name name) br
        | _ -> ())
      elements
  in
  let couplings =
    List.concat_map
      (fun (kname, names, coupling) ->
        let k = Option.value ~default:1.0 coupling in
        let branches =
          List.map
            (fun n ->
              match List.find_opt (fun (m, _) -> m = norm_name n) l_by_name with
              | Some (_, (br, _)) -> br
              | None -> elab_errorf "%s: unknown inductor '%s'" kname n)
            names
        in
        let rec pairs = function
          | a :: b :: _ -> [ (a, b, k) ]
          | _ -> []
        in
        (* pairwise for 2; chain for more *)
        let rec chain = function
          | a :: (b :: _ as rest) -> (a, b, k) :: chain rest
          | _ -> []
        in
        ignore pairs;
        chain branches)
      !(env.raw_k)
  in
  let n_nodes = env.next_node in
  let node_names =
    Array.init n_nodes (fun i ->
        match List.find_opt (fun n -> Hashtbl.find env.node_tbl n = i) (Hashtbl.fold (fun k _ acc -> k :: acc) env.node_tbl []) with
        | Some n -> n
        | None -> "?")
  in
  let n_branches = env.next_branch in
  let branch_names =
    Array.init n_branches (fun i ->
        match
          List.find_opt
            (fun n -> Hashtbl.find env.branches n = i)
            (Hashtbl.fold (fun k _ acc -> k :: acc) env.branches [])
        with
        | Some n -> n
        | None -> "?")
  in
  {
    title;
    elements;
    n_nodes;
    node_names;
    n_branches;
    branch_names;
    inductor_l = !(env.inductors);
    couplings;
    params;
    models;
    options;
    analyses;
    save = !(env.save);
  }

let elaborate ~title (nl : netlist) : circuit =
  let basedir = "." in
  let spliced = splice_includes ~basedir ~depth:0 nl in
  let defs, top = extract_subckts spliced in
  let is_global = prescan_globals defs top in
  let flat = expand_subckts defs ~is_global top in
  (* scoped .PARAM cards from subckt bodies arrive mixed with top-level ones;
     they were already substituted textually, so plain sequential eval works. *)
  elaborate_statements ~title flat

let of_string ?(title = "sandworm circuit") s =
  let nl =
    try Netlist.parse_string s with Netlist.Parse_error msg -> raise (Elab_error msg)
  in
  elaborate ~title nl

(* Human-readable dump of the elaborated circuit, for debugging netlists. *)
let dump (ckt : circuit) : string =
  let b = Buffer.create 1024 in
  let line fmt = Printf.ksprintf (fun s -> Buffer.add_string b s; Buffer.add_char b '\n') fmt in
  line "title: %s" ckt.title;
  line "nodes(%d): %s" ckt.n_nodes (String.concat ", " (Array.to_list ckt.node_names));
  line "branches(%d): %s" ckt.n_branches (String.concat ", " (Array.to_list ckt.branch_names));
  line "params: %s"
    (String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ string_of_float v) ckt.params));
  let nd i = if i < 0 then "0" else ckt.node_names.(i) in
  List.iter
    (function
      | R { name; a; b; r } -> line "R %s %s %s %g" name (nd a) (nd b) r
      | C { name; a; b; c } -> line "C %s %s %s %g" name (nd a) (nd b) c
      | L { name; a; b; l; br } -> line "L %s %s %s %g br=%d" name (nd a) (nd b) l br
      | Vsrc { name; a; b; dc; ac; wave; br } ->
          line "V %s %s %s dc=%g ac=%g<%g %s br=%d" name (nd a) (nd b) dc ac.mag
            ac.phase_deg
            (match wave with None -> "" | Some _ -> "tran")
          br
      | Isrc { name; a; b; dc; ac; wave; _ } ->
          line "I %s %s %s dc=%g ac=%g<%g %s" name (nd a) (nd b) dc ac.mag ac.phase_deg
            (match wave with None -> "" | Some _ -> "tran")
      | Vcvs { name; gain; br; _ } -> line "E %s gain=%g br=%d" name gain br
      | Vccs { name; gain; _ } -> line "G %s gain=%g" name gain
      | Cccs { name; gain; _ } -> line "F %s gain=%g" name gain
      | Ccvs { name; gain; br; _ } -> line "H %s gain=%g br=%d" name gain br
      | Dio { name; a; b; model; area } ->
          line "D %s %s %s IS=%g N=%g RS=%g area=%g" name (nd a) (nd b) model.Models.d_is
            model.Models.d_n model.Models.d_rs area
      | Bjt { name; model; area; pnp; _ } ->
          line "Q %s IS=%g BF=%g BR=%g NF=%g NR=%g VAF=%g VAR=%g area=%g pnp=%b" name
            model.Models.b_is model.Models.b_bf model.Models.b_br model.Models.b_nf
            model.Models.b_nr model.Models.b_vaf model.Models.b_var area pnp
      | Mos { name; model; w; l; _ } ->
          line "M %s %s VTO=%g KP=%g LAMBDA=%g GAMMA=%g PHI=%g W=%g L=%g" name
            (match model.Models.m_polarity with Models.Nmos -> "NMOS" | Models.Pmos -> "PMOS")
            model.Models.m_vto model.Models.m_kp model.Models.m_lambda model.Models.m_gamma
            model.Models.m_phi w l
      | Jfet { name; model; area; pchannel; _ } ->
          line "J %s VTO=%g BETA=%g LAMBDA=%g area=%g pchan=%b" name model.Models.j_vto
            model.Models.j_beta model.Models.j_lambda area pchannel
      | Bv { name; expr; _ } -> line "Bv %s %s" name (show_expr expr)
      | Bi { name; expr; _ } -> line "Bi %s %s" name (show_expr expr)
      | Vsw { name; model; _ } ->
          line "S %s VT=%g VH=%g RON=%g ROFF=%g" name model.Models.s_vt model.Models.s_vh
            model.Models.s_ron model.Models.s_roff
      | Isw { name; model; _ } ->
          line "W %s IT=%g IH=%g RON=%g ROFF=%g" name model.Models.s_vt model.Models.s_vh
            model.Models.s_ron model.Models.s_roff)
    ckt.elements;
  List.iter
    (fun (b1, b2, k) -> line "K br%d br%d k=%g" b1 b2 k)
    ckt.couplings;
  let show_analysis = function
    | Op -> "OP"
    | Dc sw -> "DC " ^ String.concat "," (List.map (fun s -> s.src) sw)
    | Ac _ -> "AC"
    | Tran _ -> "TRAN"
  in
  line "analyses: %s" (String.concat " " (List.map show_analysis ckt.analyses));
  Buffer.contents b

let of_file path =
  let content =
    try read_file path
    with Sys_error msg -> raise (Elab_error msg)
  in
  let title =
    match String.index_opt content '\n' with
    | Some i -> String.sub content 0 i |> String.trim
    | None -> path
  in
  let nl =
    try Netlist.parse_file path
    with Netlist.Parse_error msg -> raise (Elab_error msg)
  in
  let basedir = Filename.dirname path in
  let spliced = splice_includes ~basedir ~depth:0 nl in
  let defs, top = extract_subckts spliced in
  let is_global = prescan_globals defs top in
  let flat = expand_subckts defs ~is_global top in
  elaborate_statements ~title flat

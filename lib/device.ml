(* Device-specific semantic validation.
   Converts generic Ast.device values into typed device variants.
   The parser grammar stays unchanged — this is a separate pass. *)

open Ast

(* --- Error types --- *)

type error_kind =
  | Unknown_device_type of char
  | Wrong_node_count of string * int  (* expected description, got *)
  | Missing_model
  | Invalid_value of string
  | Duplicate_keyword of string

type validate_error = {
  device_name : string;
  loc : (int * int) option;
  kind : error_kind;
}

type typed_statement =
  | Typed_device of typed_device
  | Typed_cmd of dot_cmd

and typed_netlist = typed_statement list

and typed_device =
  | Resistor of two_terminal_passive
  | Capacitor of two_terminal_passive
  | Inductor of two_terminal_passive
  | Diode of diode
  | Bjt of bjt
  | Jfet of three_terminal_fet
  | Mesfet of three_terminal_fet
  | Mosfet of mosfet
  | Voltage_source of source
  | Current_source of source
  | Vcvs of voltage_controlled
  | Vccs of voltage_controlled
  | Cccs of current_controlled
  | Ccvs of current_controlled
  | Bsource of bsource
  | Vswitch of vswitch
  | Iswitch of iswitch
  | Tline of tline
  | Mutual_inductor of mutual_inductor
  | Subckt_instance of subckt_instance
  | Unknown of device

(* --- Typed device record types --- *)

and two_terminal_passive = {
  name : string;
  pos_node : string;
  neg_node : string;
  model : string option;
  value : expr option;
  params : (string * expr) list;
}

and diode = {
  name : string;
  pos_node : string;
  neg_node : string;
  model : string;
  area : expr option;
  params : (string * expr) list;
}

and bjt = {
  name : string;
  collector : string;
  base : string;
  emitter : string;
  substrate : string option;
  temp : string option;
  model : string;
  area : expr option;
  params : (string * expr) list;
}

and three_terminal_fet = {
  name : string;
  drain : string;
  gate : string;
  source : string;
  model : string;
  area : expr option;
  params : (string * expr) list;
}

and mosfet = {
  name : string;
  drain : string;
  gate : string;
  source : string;
  bulk : string;
  model : string;
  params : (string * expr) list;
}

and source = {
  name : string;
  pos_node : string;
  neg_node : string;
  dc : expr option;
  ac_mag : expr option;
  ac_phase : expr option;
  transient : (string * expr list) option;
}

and voltage_controlled = {
  name : string;
  out_pos : string;
  out_neg : string;
  in_pos : string;
  in_neg : string;
  gain : expr option;
}

and current_controlled = {
  name : string;
  pos_node : string;
  neg_node : string;
  controlling_source : string;
  gain : expr option;
}

and bsource = {
  name : string;
  pos_node : string;
  neg_node : string;
  v_expr : expr option;
  i_expr : expr option;
}

and vswitch = {
  name : string;
  pos_node : string;
  neg_node : string;
  ctrl_pos : string;
  ctrl_neg : string;
  model : string;
  params : (string * expr) list;
}

and iswitch = {
  name : string;
  pos_node : string;
  neg_node : string;
  controlling_source : string;
  model : string;
  params : (string * expr) list;
}

and tline = {
  name : string;
  port1_pos : string;
  port1_neg : string;
  port2_pos : string;
  port2_neg : string;
  params : (string * expr) list;
}

and mutual_inductor = {
  name : string;
  inductors : string list;
  coupling : expr option;
}

and subckt_instance = {
  name : string;
  nodes : string list;
  subckt_name : string;
  params : (string * expr) list;
}

(* --- Helpers --- *)

let split_args (args : device_arg list) :
  device_arg list * (string * expr) list =
  let rec loop acc_pos acc_kw = function
    | [] -> (List.rev acc_pos, List.rev acc_kw)
    | KwParam (k, v) :: rest -> loop acc_pos ((k, v) :: acc_kw) rest
    | x :: rest -> loop (x :: acc_pos) acc_kw rest
  in
  loop [] [] args

let arg_to_node = function
  | PosId s -> Some s
  | PosNum n ->
    let i = int_of_float n in
    if float_of_int i = n then Some (string_of_int i) else None
  | _ -> None

let arg_to_expr = function
  | PosNum n -> Some (ENum n)
  | PosId s -> Some (EVar s)
  | PosFunc (f, a) -> Some (ECall (f, a))
  | _ -> None

let make_error (d : Ast.device) k = Error { device_name = d.name; loc = d.loc; kind = k }

let get_unique_kw kws key =
  let matches = List.filter (fun (k, _) -> String.equal k key) kws in
  match matches with
  | [(_, v)] -> Some v
  | _ :: _ :: _ -> None  (* duplicate *)
  | [] -> None

(* --- Per-device validators --- *)

let validate_two_terminal_passive (d : device) kind =
  let pos, kws = split_args d.args in
  let nodes = List.filter_map arg_to_node pos in
  if List.length nodes < 2 then make_error d (Wrong_node_count ("2+", List.length nodes))
  else begin
    let pos_node = List.nth nodes 0 in
    let neg_node = List.nth nodes 1 in
    (* After 2 nodes: optional model name (PosId), optional value (PosNum/PosFunc) *)
    let rec drop n = function [] -> [] | _ :: t when n > 0 -> drop (n-1) t | l -> l in
    let after_nodes = drop 2 pos in
    let model, value =
      match after_nodes with
      | [] -> (None, None)
      | [PosNum n] -> (None, Some (ENum n))
      | [PosFunc (f, a)] -> (None, Some (ECall (f, a)))
      | [PosId m] -> (Some m, None)
      | PosId m :: PosNum n :: _ -> (Some m, Some (ENum n))
      | PosId m :: PosFunc (f, a) :: _ -> (Some m, Some (ECall (f, a)))
      | PosId m :: _ -> (Some m, None)
      | PosNum n :: _ -> (None, Some (ENum n))
      | _ -> (None, None)
    in
    Ok (kind { name = d.name; pos_node; neg_node; model; value; params = kws })
  end

let validate_diode (d : device) =
  let pos, kws = split_args d.args in
  let nodes = List.filter_map arg_to_node pos in
  if List.length nodes < 2 then make_error d (Wrong_node_count ("2+", List.length nodes))
  else begin
    let pos_node = List.nth nodes 0 in
    let neg_node = List.nth nodes 1 in
    (* After 2 nodes: model name is required, optional area *)
    let rec drop n = function [] -> [] | _ :: t when n > 0 -> drop (n-1) t | l -> l in
    let after_nodes = drop 2 pos in
    let ids = List.filter_map (function PosId s -> Some s | _ -> None) after_nodes in
    match ids with
    | [] -> make_error d Missing_model
    | model :: _ ->
      let area =
        match List.filter_map (function PosNum n -> Some (ENum n) | _ -> None) after_nodes with
        | v :: _ -> Some v
        | [] -> None
      in
      Ok (Diode { name = d.name; pos_node; neg_node; model; area; params = kws })
  end

let validate_bjt (d : device) =
  let pos, kws = split_args d.args in
  let nodes = List.filter_map arg_to_node pos in
  if List.length nodes < 3 then make_error d (Wrong_node_count ("3+", List.length nodes))
  else begin
    let collector = List.nth nodes 0 in
    let base = List.nth nodes 1 in
    let emitter = List.nth nodes 2 in
    (* After 3 nodes: optional substrate/temp nodes, then model (required), optional area.
       Model is the last PosId in the remaining positional args.
       Everything before the last PosId (both PosNum and PosId) is optional nodes. *)
    let rec drop n = function [] -> [] | _ :: t when n > 0 -> drop (n-1) t | l -> l in
    let rest = drop 3 pos in
    let ids = List.filter_map (function PosId s -> Some s | _ -> None) rest in
    let rec split_last = function
      | [x] -> ([], x)
      | x :: xs -> let (pre, last) = split_last xs in (x :: pre, last)
      | [] -> assert false
    in
    match ids with
    | [] -> make_error d Missing_model
    | _ ->
      let (_, model) = split_last ids in
      (* Collect all positional args before the model PosId as optional nodes *)
      let rec take_until_model = function
        | [] -> []
        | PosId s :: _ when String.equal s model -> []
        | x :: t -> x :: take_until_model t
      in
      let opt_args = take_until_model rest in
      let opt_nodes = List.filter_map arg_to_node opt_args in
      let substrate = if List.length opt_nodes >= 1 then Some (List.nth opt_nodes 0) else None in
      let temp = if List.length opt_nodes >= 2 then Some (List.nth opt_nodes 1) else None in
      let area =
        let rec find_num_after_model found = function
          | [] -> None
          | PosId s :: t when String.equal s model -> find_num_after_model true t
          | PosNum n :: t when found -> Some (ENum n)
          | _ :: t -> find_num_after_model found t
        in
        find_num_after_model false rest
      in
      Ok (Bjt { name = d.name; collector; base; emitter; substrate; temp;
                model; area; params = kws })
  end

let validate_three_terminal_fet (d : device) kind =
  let pos, kws = split_args d.args in
  let nodes = List.filter_map arg_to_node pos in
  if List.length nodes < 3 then make_error d (Wrong_node_count ("3+", List.length nodes))
  else begin
    let drain = List.nth nodes 0 in
    let gate = List.nth nodes 1 in
    let source = List.nth nodes 2 in
    let rec drop n = function [] -> [] | _ :: t when n > 0 -> drop (n-1) t | l -> l in
    let after_nodes = List.filter_map (function PosId s -> Some s | _ -> None) (drop 3 pos) in
    match after_nodes with
    | [] -> make_error d Missing_model
    | model :: _ ->
      let area =
        let rec find_num = function
          | [] -> None
          | PosNum n :: _ -> Some (ENum n)
          | _ :: t -> find_num t
        in
        find_num (drop 3 pos)
      in
      Ok (kind { name = d.name; drain; gate; source; model; area; params = kws })
  end

let validate_mosfet (d : device) =
  let pos, kws = split_args d.args in
  let nodes = List.filter_map arg_to_node pos in
  if List.length nodes < 4 then make_error d (Wrong_node_count ("4+", List.length nodes))
  else begin
    let drain = List.nth nodes 0 in
    let gate = List.nth nodes 1 in
    let source = List.nth nodes 2 in
    let bulk = List.nth nodes 3 in
    let rec drop n = function [] -> [] | _ :: t when n > 0 -> drop (n-1) t | l -> l in
    let after_nodes = List.filter_map (function PosId s -> Some s | _ -> None) (drop 4 pos) in
    match after_nodes with
    | [] -> make_error d Missing_model
    | model :: _ ->
      Ok (Mosfet { name = d.name; drain; gate; source; bulk; model; params = kws })
  end

let validate_source (d : device) kind =
  let pos, kws = split_args d.args in
  let nodes = List.filter_map arg_to_node pos in
  if List.length nodes < 2 then make_error d (Wrong_node_count ("2+", List.length nodes))
  else begin
    let pos_node = List.nth nodes 0 in
    let neg_node = List.nth nodes 1 in
    (* Parse positional args after 2 nodes for DC/AC/transient.
       Accepted forms (keywords case-insensitive):
         DC <val> | <bare val>            (DC value)
         AC <mag> [<phase>]                (AC value, phase in degrees)
       e.g. "V1 1 0 DC 5", "V1 1 0 5 AC 1", "V1 1 0 AC 1 90",
       "V1 1 0 DC 0 AC 1 0 SIN(...)" *)
    let rec drop n = function [] -> [] | _ :: t when n > 0 -> drop (n-1) t | l -> l in
    let rest_pos = drop 2 pos in
    let is_kw s kw = String.lowercase_ascii s = kw in
    let kw_param_ci kws key =
      match get_unique_kw kws key with
      | Some _ as v -> v
      | None -> (
          match List.find_opt (fun (k, _) -> is_kw k key) kws with
          | Some (_, v) -> Some v
          | None -> None)
    in
    (* Split off the AC spec ("AC" keyword through end of AC numbers) so the
       bare-DC search does not pick up AC numbers. *)
    let rec split_ac acc = function
      | [] -> (List.rev acc, None, [])
      | PosId s :: t when is_kw s "ac" -> (List.rev acc, Some t, t)
      | a :: t -> split_ac (a :: acc) t
    in
    let pre_ac, _, post_ac_full = split_ac [] rest_pos in
    let ac_mag, ac_phase =
      match kw_param_ci kws "ac" with
      | Some v -> (Some v, None)
      | None -> (
          let num_or_var = function
            | PosNum n -> Some (ENum n)
            | PosId s -> Some (EVar s)
            | _ -> None
          in
          match post_ac_full with
          | mag_arg :: rest -> (
              let mag = num_or_var mag_arg in
              let phase = match List.filter_map num_or_var rest with
                | ph :: _ -> Some ph
                | [] -> None
              in
              (* only treat as AC spec if something followed the AC keyword *)
              (match mag with Some _ -> (mag, phase) | None -> (None, None)))
          | [] -> (None, None))
    in
    (* AC spec present? (keyword seen positionally or via KwParam) *)
    let _ = ac_phase in
    let dc = match kw_param_ci kws "dc" with
      | Some v -> Some v
      | None ->
        (* Positional "DC <val>" anywhere before the AC spec *)
        let rec find_dc = function
          | PosId s :: PosNum n :: _ when is_kw s "dc" -> Some (ENum n)
          | PosId s :: ((PosFunc _ as pf) :: _) when is_kw s "dc" -> arg_to_expr pf
          | PosId s :: PosId v :: _ when is_kw s "dc" -> Some (EVar v)
          | _ :: t -> find_dc t
          | [] -> None
        in
        (match find_dc pre_ac with
         | Some _ as d -> d
         | None ->
           (* Bare positional value (first number/func before any AC spec) *)
           (match List.filter_map arg_to_expr pre_ac with
            | [] -> None
            | v :: _ -> Some v))
    in
    let transient =
      let rec find_func = function
        | PosFunc (f, args) :: _ -> Some (f, args)
        | _ :: t -> find_func t
        | [] -> None
      in
      find_func rest_pos
    in
    Ok (kind { name = d.name; pos_node; neg_node; dc; ac_mag; ac_phase; transient })
  end

let validate_voltage_controlled (d : device) kind =
  let pos, kws = split_args d.args in
  let nodes = List.filter_map arg_to_node pos in
  if List.length nodes < 4 then make_error d (Wrong_node_count ("4+", List.length nodes))
  else begin
    let out_pos = List.nth nodes 0 in
    let out_neg = List.nth nodes 1 in
    let in_pos = List.nth nodes 2 in
    let in_neg = List.nth nodes 3 in
    let rec drop n = function [] -> [] | _ :: t when n > 0 -> drop (n-1) t | l -> l in
    let gain =
      match List.filter_map arg_to_expr (drop 4 pos) with
      | v :: _ -> Some v
      | [] -> (match get_unique_kw kws "gain" with Some v -> Some v | None -> None)
    in
    Ok (kind { name = d.name; out_pos; out_neg; in_pos; in_neg; gain })
  end

let validate_current_controlled (d : device) kind =
  let pos, kws = split_args d.args in
  let nodes = List.filter_map arg_to_node pos in
  if List.length nodes < 2 then make_error d (Wrong_node_count ("2+", List.length nodes))
  else begin
    let pos_node = List.nth nodes 0 in
    let neg_node = List.nth nodes 1 in
    let rec drop n = function [] -> [] | _ :: t when n > 0 -> drop (n-1) t | l -> l in
    let remaining = List.filter_map (function PosId s -> Some s | _ -> None) (drop 2 pos) in
    match remaining with
    | [] -> make_error d (Invalid_value "missing controlling source")
    | vname :: _ ->
      let gain =
        let rec find_num = function
          | [] -> None
          | PosNum n :: _ -> Some (ENum n)
          | _ :: t -> find_num t
        in
        let g = find_num (drop 2 pos) in
        match g with
        | Some v -> Some v
        | None -> (match get_unique_kw kws "gain" with Some v -> Some v | None -> None)
      in
      Ok (kind { name = d.name; pos_node; neg_node;
                 controlling_source = vname; gain })
  end

let validate_bsource (d : device) =
  let _pos, kws = split_args d.args in
  let nodes = List.filter_map arg_to_node d.args in
  if List.length nodes < 2 then make_error d (Wrong_node_count ("2+", List.length nodes))
  else begin
    let pos_node = List.nth nodes 0 in
    let neg_node = List.nth nodes 1 in
    let v_expr = get_unique_kw kws "V" in
    let i_expr = get_unique_kw kws "I" in
    Ok (Bsource { name = d.name; pos_node; neg_node; v_expr; i_expr })
  end

let validate_vswitch (d : device) =
  let pos, kws = split_args d.args in
  let nodes = List.filter_map arg_to_node pos in
  if List.length nodes < 4 then make_error d (Wrong_node_count ("4+", List.length nodes))
  else begin
    let pos_node = List.nth nodes 0 in
    let neg_node = List.nth nodes 1 in
    let ctrl_pos = List.nth nodes 2 in
    let ctrl_neg = List.nth nodes 3 in
    let rec drop n = function [] -> [] | _ :: t when n > 0 -> drop (n-1) t | l -> l in
    let remaining = List.filter_map (function PosId s -> Some s | _ -> None) (drop 4 pos) in
    match remaining with
    | [] -> make_error d Missing_model
    | model :: _ ->
      Ok (Vswitch { name = d.name; pos_node; neg_node; ctrl_pos; ctrl_neg; model; params = kws })
  end

let validate_iswitch (d : device) =
  let pos, kws = split_args d.args in
  let nodes = List.filter_map arg_to_node pos in
  if List.length nodes < 2 then make_error d (Wrong_node_count ("2+", List.length nodes))
  else begin
    let pos_node = List.nth nodes 0 in
    let neg_node = List.nth nodes 1 in
    let rec drop n = function [] -> [] | _ :: t when n > 0 -> drop (n-1) t | l -> l in
    let remaining = List.filter_map (function PosId s -> Some s | _ -> None) (drop 2 pos) in
    match remaining with
    | [] -> make_error d (Invalid_value "missing controlling source")
    | vname :: _ ->
      let model =
        let ids = List.filter_map (function PosId s -> Some s | _ -> None) (drop 2 pos) in
        match ids with
        | _ :: m :: _ -> Some m
        | _ -> None
      in
      match model with
      | None -> make_error d Missing_model
      | Some m ->
        Ok (Iswitch { name = d.name; pos_node; neg_node;
                      controlling_source = vname; model = m; params = kws })
  end

let validate_tline (d : device) =
  let pos, kws = split_args d.args in
  let nodes = List.filter_map arg_to_node pos in
  if List.length nodes < 4 then make_error d (Wrong_node_count ("4+", List.length nodes))
  else begin
    let port1_pos = List.nth nodes 0 in
    let port1_neg = List.nth nodes 1 in
    let port2_pos = List.nth nodes 2 in
    let port2_neg = List.nth nodes 3 in
    Ok (Tline { name = d.name; port1_pos; port1_neg; port2_pos; port2_neg; params = kws })
  end

let validate_mutual_inductor (d : device) =
  let pos, kws = split_args d.args in
  let inductors = List.filter_map (function PosId s -> Some s | _ -> None) pos in
  if List.length inductors < 2 then make_error d (Wrong_node_count ("2+ inductors", List.length inductors))
  else begin
    let coupling =
      match List.filter_map arg_to_expr pos with
      | _ :: _ :: v :: _ -> Some v
      | [] -> None
      | _ -> None
    in
    Ok (Mutual_inductor { name = d.name; inductors; coupling })
  end

let validate_subckt_instance (d : device) =
  let pos, kws = split_args d.args in
  let all_ids = List.filter_map (function PosId s -> Some s | _ -> None) pos in
  match all_ids with
  | [] -> make_error d (Invalid_value "missing subcircuit name")
  | _ ->
    (* Last positional ID is the subcircuit name, the rest are nodes *)
    let rec split_last = function
      | [x] -> ([], x)
      | x :: xs -> let (nodes, last) = split_last xs in (x :: nodes, last)
      | [] -> assert false
    in
    let (nodes, subckt_name) = split_last all_ids in
    Ok (Subckt_instance { name = d.name; nodes; subckt_name; params = kws })

(* --- Dispatch --- *)

let validate_device (d : device) : (typed_device, validate_error) result =
  match Char.lowercase_ascii d.name.[0] with
  | 'r' -> validate_two_terminal_passive d (fun x -> Resistor x)
  | 'c' -> validate_two_terminal_passive d (fun x -> Capacitor x)
  | 'l' -> validate_two_terminal_passive d (fun x -> Inductor x)
  | 'd' -> validate_diode d
  | 'q' -> validate_bjt d
  | 'j' -> validate_three_terminal_fet d (fun x -> Jfet x)
  | 'z' -> validate_three_terminal_fet d (fun x -> Mesfet x)
  | 'm' -> validate_mosfet d
  | 'v' -> validate_source d (fun x -> Voltage_source x)
  | 'i' -> validate_source d (fun x -> Current_source x)
  | 'e' -> validate_voltage_controlled d (fun x -> Vcvs x)
  | 'g' -> validate_voltage_controlled d (fun x -> Vccs x)
  | 'f' -> validate_current_controlled d (fun x -> Cccs x)
  | 'h' -> validate_current_controlled d (fun x -> Ccvs x)
  | 'b' -> validate_bsource d
  | 's' -> validate_vswitch d
  | 'w' -> validate_iswitch d
  | 't' -> validate_tline d
  | 'k' -> validate_mutual_inductor d
  | 'x' -> validate_subckt_instance d
  | c -> Ok (Unknown d)

(* --- Netlist validation --- *)

let validate_netlist (nl : netlist) : (typed_netlist, validate_error list) result =
  let errors = ref [] in
  let typed = List.map (function
      | DotCmd cmd -> Typed_cmd cmd
      | Device d ->
        match validate_device d with
        | Ok td -> Typed_device td
        | Error e -> errors := e :: !errors; Typed_device (Unknown d)
    ) nl in
  match !errors with
  | [] -> Ok typed
  | e -> Error (List.rev e)

let format_error (e : validate_error) =
  let loc_str = match e.loc with
    | Some (line, col) -> Printf.sprintf " (line %d, col %d)" line col
    | None -> ""
  in
  let kind_str = match e.kind with
    | Unknown_device_type c -> Printf.sprintf "unknown device type '%c'" c
    | Wrong_node_count (expected, got) ->
      Printf.sprintf "wrong node count: expected %s, got %d" expected got
    | Missing_model -> "missing model reference"
    | Invalid_value s -> Printf.sprintf "invalid value: %s" s
    | Duplicate_keyword k -> Printf.sprintf "duplicate keyword '%s'" k
  in
  Printf.sprintf "device '%s'%s: %s" e.device_name loc_str kind_str

(* SPICE-like analog simulator: MNA + Newton-Raphson + transient companion
   models + complex AC. Pure OCaml (stdlib only). Raises Sim_error on
   netlist/semantic problems and No_convergence when Newton/GMIN stepping
   cannot find an operating point. *)

open Ast
open Circuit

exception Sim_error of string
exception No_convergence of string

let sim_errorf fmt = Printf.ksprintf (fun s -> raise (Sim_error s)) fmt
let no_conv_f fmt = Printf.ksprintf (fun s -> raise (No_convergence s)) fmt

(* Overflow-safe exp with linear continuation past xmax (SPICE DEXP-style):
   value and derivative stay a consistent pair, so Newton can never stick
   in a flat-but-stiff spurious fixed point. *)
let exp_xmax = 80.0
let exp_emax = 5.54062238439351e34 (* exp(80) *)

let exp_lim x =
  if x > exp_xmax then exp_emax *. (1.0 +. x -. exp_xmax)
  else if x < -80.0 then 0.0
  else Float.exp x

let dexp_lim x =
  if x > exp_xmax then exp_emax else if x < -80.0 then 0.0 else Float.exp x

let debug_enabled () =
  match Sys.getenv_opt "SANDWORM_DEBUG" with
  | Some s when s <> "" && s <> "0" -> true
  | _ -> false

(* ------------------------------------------------------------------ *)
(* Simulator state                                                     *)
(* ------------------------------------------------------------------ *)

type stamp_mode =
  | M_dc
  | M_tran of {
      t : float;
      dt : float;
      use_trap : bool;
      first_be : bool;
    }

(* Small-signal linearization captured at the operating point, for AC. *)
type lin =
  | Lin_none
  | Lin_dio of float (* g *)
  | Lin_bjt of {
      dc_db : float;
      dc_dc : float;
      dc_de : float;
      db_db : float;
      db_dc : float;
      db_de : float;
    }
  | Lin_mos of {
      gm : float;
      gds : float;
      gmb : float;
    }
  | Lin_jfet of {
      gm : float;
      gds : float;
    }
  | Lin_b of float array (* dV-or-dI/dx, length n *)
  | Lin_r of float (* switch on/off resistance *)

type st = {
  ckt : circuit;
  n : int;
  elems : element array;
  nelems : int;
  node_of_name : (string, int) Hashtbl.t;
  branch_of_name : (string, int) Hashtbl.t;
  lmat : float array array;
  lvec_idx_of_br : (int, int) Hashtbl.t;
  cap_vc : float array;
  cap_ic : float array;
  ind_vprev : float array;
  sw_on : bool array;
  lin : lin array;
  mutable op_valid : bool;
}

(* Solution-side junction damping (SPICE-style): scale the Newton step so
   that no exponential junction moves too far in one iteration. This keeps
   the solver out of overflow-stiff regions where micro-corrections would
   falsely look converged. *)
let v_of x a = if a < 0 then 0.0 else x.(a)

let junction_damp s x_old x_new =
  let worst = ref 1.0 in
  let consider vold vnew =
    let dv = Float.abs (vnew -. vold) in
    if dv > 1e-300 then (
      let maxdv = if vnew > vold then 0.25 else 2.0 in
      if dv > maxdv then worst := Float.min !worst (maxdv /. dv))
  in
  Array.iter
    (function
      | Dio { a; b; _ } ->
          consider (v_of x_old a -. v_of x_old b) (v_of x_new a -. v_of x_new b)
      | Bjt { c; b; e; pnp; _ } ->
          let sgn = if pnp then -1.0 else 1.0 in
          consider
            (sgn *. (v_of x_old b -. v_of x_old e))
            (sgn *. (v_of x_new b -. v_of x_new e));
          consider
            (sgn *. (v_of x_old b -. v_of x_old c))
            (sgn *. (v_of x_new b -. v_of x_new c))
      | _ -> ())
    s.elems;
  !worst

let find_branch st name =
  match Hashtbl.find_opt st.branch_of_name (String.uppercase_ascii name) with
  | Some b -> b
  | None -> sim_errorf "unknown voltage-source/branch '%s'" name

let init (ckt : circuit) : st =
  let elems = Array.of_list ckt.elements in
  let nelems = Array.length elems in
  let n = ckt.n_nodes + ckt.n_branches in
  let node_of_name = Hashtbl.create (max 8 ckt.n_nodes) in
  Array.iteri (fun i nm -> Hashtbl.add node_of_name nm i) ckt.node_names;
  let branch_of_name = Hashtbl.create (max 8 ckt.n_branches) in
  Array.iteri (fun i nm -> Hashtbl.add branch_of_name nm i) ckt.branch_names;
  (* Inductor branch ordering for the L matrix. *)
  let l_brs =
    List.map fst ckt.inductor_l |> List.sort_uniq Int.compare |> Array.of_list
  in
  let nl = Array.length l_brs in
  let lmat = Array.make_matrix nl nl 0.0 in
  let l_of_br = Hashtbl.create nl in
  List.iter
    (fun (br, l) ->
      if l <= 0.0 then sim_errorf "inductance must be positive";
      Hashtbl.replace l_of_br br l)
    ckt.inductor_l;
  Array.iteri
    (fun i br ->
      let li = Hashtbl.find l_of_br br in
      lmat.(i).(i) <- li)
    l_brs;
  List.iter
    (fun (b1, b2, k) ->
      let i1 = ref (-1) and i2 = ref (-1) in
      Array.iteri
        (fun i br ->
          if br = b1 then i1 := i;
          if br = b2 then i2 := i)
        l_brs;
      if !i1 < 0 || !i2 < 0 then
        sim_errorf "coupling references unknown inductor branch";
      let l1 = Hashtbl.find l_of_br b1 and l2 = Hashtbl.find l_of_br b2 in
      if Float.abs k >= 1.0 then sim_errorf "coupling coefficient |k| must be < 1";
      let m = k *. Float.sqrt (l1 *. l2) in
      lmat.(!i1).(!i2) <- lmat.(!i1).(!i2) +. m;
      lmat.(!i2).(!i1) <- lmat.(!i2).(!i1) +. m)
    ckt.couplings;
  let lvec_idx_of_br = Hashtbl.create nl in
  Array.iteri (fun i br -> Hashtbl.add lvec_idx_of_br br i) l_brs;
  {
    ckt;
    n;
    elems;
    nelems;
    node_of_name;
    branch_of_name;
    lmat;
    lvec_idx_of_br;
    cap_vc = Array.make nelems 0.0;
    cap_ic = Array.make nelems 0.0;
    ind_vprev = Array.make nelems 0.0;
    sw_on = Array.make nelems false;
    lin = Array.make nelems Lin_none;
    op_valid = false;
  }

let node_v (x : float array) (ckt : circuit) a =
  if a < 0 then 0.0 else x.(a)

let branch_i (x : float array) (ckt : circuit) br = x.(ckt.n_nodes + br)

(* ------------------------------------------------------------------ *)
(* Source waveforms                                                    *)
(* ------------------------------------------------------------------ *)

let rec wave_value w t =
  match w with
  | Pulse p ->
      if t < p.td then p.v1
      else
        let te = t -. p.td in
        let v =
          if te < p.tr then
            if p.tr <= 0.0 then p.v2 else p.v1 +. (p.v2 -. p.v1) *. (te /. p.tr)
          else if te < p.tr +. p.pw then p.v2
          else if te < p.tr +. p.pw +. p.tf then
            if p.tf <= 0.0 then p.v1
            else p.v2 +. (p.v1 -. p.v2) *. ((te -. p.tr -. p.pw) /. p.tf)
          else p.v1
        in
        if te >= p.per && p.per > 0.0 && p.per < Float.infinity then
          wave_value w (p.td +. Float.rem te p.per)
        else v
  | Sine s ->
      let ph = s.phase *. Float.pi /. 180.0 in
      if t < s.td then s.voff +. s.vamp *. Float.sin ph
      else
        let te = t -. s.td in
        let damp = if s.theta = 0.0 then 1.0 else Float.exp (-.te *. s.theta) in
        s.voff +. s.vamp *. damp *. Float.sin (2.0 *. Float.pi *. s.freq *. te +. ph)
  | Pwl pts -> (
      match pts with
      | [] -> 0.0
      | (t0, v0) :: _ when t <= t0 -> v0
      | _ ->
          let rec aux prev = function
            | [] -> snd prev
            | (t1, v1) :: rest ->
                if t <= t1 then
                  let (t0, v0) = prev in
                  if t1 = t0 then v1 else v0 +. (v1 -. v0) *. ((t -. t0) /. (t1 -. t0))
                else aux (t1, v1) rest
          in
          aux (List.hd pts) (List.tl pts))
  | Exp e ->
      if t < e.td1 then e.v1
      else if t < e.td2 then
        if e.tau1 <= 0.0 then e.v2
        else e.v1 +. (e.v2 -. e.v1) *. (1.0 -. Float.exp (-.(t -. e.td1) /. e.tau1))
      else
        let v_td2 =
          if e.tau1 <= 0.0 then e.v2
          else e.v1 +. (e.v2 -. e.v1) *. (1.0 -. Float.exp (-.(e.td2 -. e.td1) /. e.tau1))
        in
        if e.tau2 <= 0.0 then e.v1 else v_td2 +. (e.v1 -. v_td2) *. (1.0 -. Float.exp (-.(t -. e.td2) /. e.tau2))

(* ------------------------------------------------------------------ *)
(* Real stamping: G x = rhs                                            *)
(* ------------------------------------------------------------------ *)

let stamp_real (s : st) ~(mode : stamp_mode) ~(gmin : float) ~(dc_over : (string * float) list)
    (x : float array) (g : float array array) (rhs : float array) =
  let ckt = s.ckt in
  let add_g i j v =
    if i >= 0 && j >= 0 then g.(i).(j) <- g.(i).(j) +. v
    else if i >= 0 then () (* ground column: no-op *)
    else ()
  in
  let add_gb i br v =
    if i >= 0 then g.(i).(ckt.n_nodes + br) <- g.(i).(ckt.n_nodes + br) +. v
  in
  let add_bg br j v =
    if j >= 0 then g.(ckt.n_nodes + br).(j) <- g.(ckt.n_nodes + br).(j) +. v
  in
  let add_bb b1 b2 v =
    g.(ckt.n_nodes + b1).(ckt.n_nodes + b2) <-
      g.(ckt.n_nodes + b1).(ckt.n_nodes + b2) +. v
  in
  let add_i i v = if i >= 0 then rhs.(i) <- rhs.(i) +. v in
  let add_ib br v = rhs.(ckt.n_nodes + br) <- rhs.(ckt.n_nodes + br) +. v in
  (* GMIN: conductance from every node to ground. *)
  if gmin > 0.0 then for i = 0 to ckt.n_nodes - 1 do g.(i).(i) <- g.(i).(i) +. gmin done;
  let vdiff a b = node_v x ckt a -. node_v x ckt b in
  (* B-source evaluation context at the current iterate. *)
  let bctx time freq =
    {
      Eval.params = ckt.params;
      time;
      freq;
      v_node = (fun nm -> try x.(Hashtbl.find s.node_of_name (norm_node nm)) with Not_found -> 0.0);
      v_diff =
        (fun (n1, n2) ->
          let v n = try x.(Hashtbl.find s.node_of_name (norm_node n)) with Not_found -> 0.0 in
          v n1 -. v n2);
      i_branch =
        (fun nm ->
          match Hashtbl.find_opt s.branch_of_name (String.uppercase_ascii nm) with
          | Some br -> branch_i x ckt br
          | None -> 0.0);
    }
  in
  let time_of_mode = match mode with M_dc -> 0.0 | M_tran { t; _ } -> t in
  Array.iteri
    (fun ei el ->
      match el with
      | R { name; a; b; r } ->
          let r =
            match List.find_opt (fun (k, _) -> k = norm_name name) dc_over with
            | Some (_, v) ->
                if v = 0.0 then sim_errorf "%s: resistance swept to zero" name;
                v
            | None -> r
          in
          let gg = 1.0 /. r in
          add_g a a gg;
          add_g b b gg;
          add_g a b (-.gg);
          add_g b a (-.gg);
          s.lin.(ei) <- Lin_none
      | C { a; b; c; _ } -> (
          match mode with
          | M_dc -> s.lin.(ei) <- Lin_none (* open *)
          | M_tran { dt; use_trap; first_be; _ } ->
              let trap = use_trap && not first_be in
              if trap then (
                let geq = 2.0 *. c /. dt in
                let ieq = -.(s.cap_ic.(ei) +. geq *. s.cap_vc.(ei)) in
                add_g a a geq;
                add_g b b geq;
                add_g a b (-.geq);
                add_g b a (-.geq);
                add_i a (-.ieq);
                add_i b ieq)
              else (
                let geq = c /. dt in
                let ieq = -.geq *. s.cap_vc.(ei) in
                add_g a a geq;
                add_g b b geq;
                add_g a b (-.geq);
                add_g b a (-.geq);
                add_i a (-.ieq);
                add_i b ieq))
      | L { a; b; br; _ } -> (
          add_gb a br 1.0;
          add_gb b br (-1.0);
          match mode with
          | M_dc ->
              add_bg br a 1.0;
              add_bg br b (-1.0);
              add_ib br 0.0
          | M_tran { dt; use_trap; first_be; _ } ->
              (* Coupled-inductor companion via the L matrix row. *)
              let li = Hashtbl.find s.lvec_idx_of_br br in
              let nl = Array.length s.lmat in
              for j = 0 to nl - 1 do
                (* branch-current columns *)
                let brj = ref (-1) in
                Hashtbl.iter (fun b idx -> if idx = j then brj := b) s.lvec_idx_of_br;
                add_bb br !brj s.lmat.(li).(j)
              done;
              let c1, c2extra =
                if use_trap && not first_be then (
                  (* TRAP: L I - (dt/2) V = L Iprev - (dt/2) Vprev *)
                  let liprev = ref 0.0 in
                  for j = 0 to nl - 1 do
                    let brj = ref (-1) in
                    Hashtbl.iter (fun b idx -> if idx = j then brj := b) s.lvec_idx_of_br;
                    liprev := !liprev +. s.lmat.(li).(j) *. branch_i x ckt !brj
                  done;
                  (* Vprev of THIS inductor: stored per element *)
                  let vprev = s.ind_vprev.(ei) in
                  (-.dt /. 2.0, !liprev -. (dt /. 2.0) *. vprev))
                else (
                  let liprev = ref 0.0 in
                  for j = 0 to nl - 1 do
                    let brj = ref (-1) in
                    Hashtbl.iter (fun b idx -> if idx = j then brj := b) s.lvec_idx_of_br;
                    liprev := !liprev +. s.lmat.(li).(j) *. branch_i x ckt !brj
                  done;
                  (-.dt, !liprev))
              in
              add_bg br a (-.c1);
              add_bg br b c1;
              add_ib br c2extra)
      | Vsrc { name; a; b; dc; wave; br; _ } ->
          let v =
            match List.find_opt (fun (k, _) -> k = norm_name name) dc_over with
            | Some (_, vv) -> vv
            | None -> (
                match mode with
                | M_dc -> dc
                | M_tran { t; _ } -> (
                    match wave with
                    | None -> dc
                    | Some w -> wave_value w t))
          in
          add_gb a br 1.0;
          add_gb b br (-1.0);
          add_bg br a 1.0;
          add_bg br b (-1.0);
          add_ib br v;
          s.lin.(ei) <- Lin_none
      | Isrc { name; a; b; dc; wave; _ } ->
          let v =
            match List.find_opt (fun (k, _) -> k = norm_name name) dc_over with
            | Some (_, vv) -> vv
            | None -> (
                match mode with
                | M_dc -> dc
                | M_tran { t; _ } -> (
                    match wave with
                    | None -> dc
                    | Some w -> wave_value w t))
          in
          add_i a (-.v);
          add_i b v;
          s.lin.(ei) <- Lin_none
      | Vcvs { ap; an; cp; cn; gain; br; _ } ->
          add_gb ap br 1.0;
          add_gb an br (-1.0);
          add_bg br ap 1.0;
          add_bg br an (-1.0);
          add_bg br cp (-.gain);
          add_bg br cn gain;
          add_ib br 0.0;
          s.lin.(ei) <- Lin_none
      | Vccs { ap; an; cp; cn; gain; _ } ->
          add_g ap cp gain;
          add_g ap cn (-.gain);
          add_g an cp (-.gain);
          add_g an cn gain;
          s.lin.(ei) <- Lin_none
      | Cccs { a; b; ctrl; gain; _ } ->
          add_gb a ctrl gain;
          add_gb b ctrl (-.gain);
          s.lin.(ei) <- Lin_none
      | Ccvs { a; b; ctrl; gain; br; _ } ->
          add_gb a br 1.0;
          add_gb b br (-1.0);
          add_bg br a 1.0;
          add_bg br b (-1.0);
          add_bb br ctrl (-.gain);
          add_ib br 0.0;
          s.lin.(ei) <- Lin_none
      | Dio { a; b; model; area; _ } ->
          let vt = Models.thermal_voltage () in
          let is = model.Models.d_is *. area in
          let nv = model.Models.d_n *. vt in
          let vd = vdiff a b in
          let arg = vd /. nv in
          let e = exp_lim arg and de = dexp_lim arg in
          let id = is *. (e -. 1.0) in
          let gg = is /. nv *. de in
          add_g a a gg;
          add_g b b gg;
          add_g a b (-.gg);
          add_g b a (-.gg);
          let ieq = id -. gg *. vd in
          add_i a (-.ieq);
          add_i b ieq;
          s.lin.(ei) <- Lin_dio gg
      | Bjt { c; b; e; model; area; pnp; _ } ->
          let sgn = if pnp then -1.0 else 1.0 in
          let vb = node_v x ckt b and vc = node_v x ckt c and ve = node_v x ckt e in
          let vbe = sgn *. (vb -. ve) and vbc = sgn *. (vb -. vc) in
          let vt = Models.thermal_voltage () in
          let is = model.Models.b_is *. area in
          let arg_be = vbe /. (model.Models.b_nf *. vt) in
          let arg_bc = vbc /. (model.Models.b_nr *. vt) in
          let gif = is /. (model.Models.b_nf *. vt) *. dexp_lim arg_be in
          let gir = is /. (model.Models.b_nr *. vt) *. dexp_lim arg_bc in
          let f_if = is *. (exp_lim arg_be -. 1.0) in
          let f_ir = is *. (exp_lim arg_bc -. 1.0) in
          let q1 =
            1.0
            /. (1.0 -. vbc /. model.Models.b_vaf -. vbe /. model.Models.b_var)
          in
          let ic_n = q1 *. (f_if -. f_ir) -. f_ir /. model.Models.b_br in
          let ib_n = f_if /. model.Models.b_bf +. f_ir /. model.Models.b_br in
          (* partials w.r.t. internal (signed) voltages *)
          let dq_dvbe = q1 *. q1 /. model.Models.b_var in
          let dq_dvbc = q1 *. q1 /. model.Models.b_vaf in
          let dIc_dvbe = q1 *. gif +. (f_if -. f_ir) *. dq_dvbe in
          let dIc_dvbc = -.q1 *. gir +. (f_if -. f_ir) *. dq_dvbc in
          let dIb_dvbe = gif /. model.Models.b_bf in
          let dIb_dvbc = gir /. model.Models.b_br in
          (* map to terminal voltages: d/dVb = sgn*(d/dvbe + d/dvbc) etc. *)
          let gic_b = sgn *. (dIc_dvbe +. dIc_dvbc) in
          let gic_c = sgn *. (-.dIc_dvbc) in
          let gic_e = sgn *. (-.dIc_dvbe) in
          let gib_b = sgn *. (dIb_dvbe +. dIb_dvbc) in
          let gib_c = sgn *. (-.dIb_dvbc) in
          let gib_e = sgn *. (-.dIb_dvbe) in
          let ic = sgn *. ic_n and ib = sgn *. ib_n in
          let ie = -.(ic +. ib) in
          let gie_b = -.(gic_b +. gib_b) in
          let gie_c = -.(gic_c +. gib_c) in
          let gie_e = -.(gic_e +. gib_e) in
          add_g c b gic_b;
          add_g c c gic_c;
          add_g c e gic_e;
          add_g b b gib_b;
          add_g b c gib_c;
          add_g b e gib_e;
          add_g e b gie_b;
          add_g e c gie_c;
          add_g e e gie_e;
          add_i c (-.(ic -. (gic_b *. vb +. gic_c *. vc +. gic_e *. ve)));
          add_i b (-.(ib -. (gib_b *. vb +. gib_c *. vc +. gib_e *. ve)));
          add_i e (-.(ie -. (gie_b *. vb +. gie_c *. vc +. gie_e *. ve)));
          s.lin.(ei) <- Lin_bjt { dc_db = gic_b; dc_dc = gic_c; dc_de = gic_e; db_db = gib_b; db_dc = gib_c; db_de = gib_e }
      | Mos { d; g; s = src; bulk; model; w; l; _ } ->
          let sgn = match model.Models.m_polarity with Models.Nmos -> 1.0 | Models.Pmos -> -1.0 in
          let vd = node_v x ckt d and vg = node_v x ckt g and vs = node_v x ckt src and vb = node_v x ckt bulk in
          let vgs = sgn *. (vg -. vs) and vds = sgn *. (vd -. vs) and vsb = sgn *. (vs -. vb) in
          (* Vth is given in physical (signed) units; map it to the
             NMOS-equivalent frame like the terminal voltages. *)
          let vth =
            (sgn *. model.Models.m_vto)
            +. model.Models.m_gamma
               *. (Float.sqrt (Float.max 0.0 (model.Models.m_phi +. vsb)) -. Float.sqrt model.Models.m_phi)
          in
          let beta = model.Models.m_kp *. (w /. l) in
          let vgst = vgs -. vth in
          let id_n, gm, gds, gmb =
            if vgst <= 0.0 then (0.0, 0.0, 0.0, 0.0)
            else if vds >= vgst then (
              (* saturation *)
              let ids = 0.5 *. beta *. vgst *. vgst *. (1.0 +. model.Models.m_lambda *. vds) in
              let gm = beta *. vgst *. (1.0 +. model.Models.m_lambda *. vds) in
              let gds = 0.5 *. beta *. vgst *. vgst *. model.Models.m_lambda in
              let dvth =
                if model.Models.m_gamma = 0.0 then 0.0
                else model.Models.m_gamma /. (2.0 *. Float.sqrt (Float.max 1e-12 (model.Models.m_phi +. vsb)))
              in
              let gmb = gm *. dvth in
              (ids, gm, gds, gmb))
            else (
              (* linear *)
              let ids = beta *. (vgst *. vds -. 0.5 *. vds *. vds) *. (1.0 +. model.Models.m_lambda *. vds) in
              let gm = beta *. vds *. (1.0 +. model.Models.m_lambda *. vds) in
              let gds =
                beta *. ((vgst -. vds) *. (1.0 +. model.Models.m_lambda *. vds) +. (vgst *. vds -. 0.5 *. vds *. vds) *. model.Models.m_lambda)
              in
              let dvth =
                if model.Models.m_gamma = 0.0 then 0.0
                else model.Models.m_gamma /. (2.0 *. Float.sqrt (Float.max 1e-12 (model.Models.m_phi +. vsb)))
              in
              let gmb = gm *. dvth in
              (ids, gm, gds, gmb))
          in
          (* map to terminals: Id(into drain, actual) = sgn * id_n *)
          let gd_d = sgn *. gds in
          let gd_g = sgn *. gm in
          let gd_s = sgn *. (-.gm -. gds +. gmb) in
          let gd_b = sgn *. (-.gmb) in
          let id = sgn *. id_n in
          add_g d d gd_d;
          add_g d g gd_g;
          add_g d src gd_s;
          add_g d bulk gd_b;
          add_g src d (-.gd_d);
          add_g src g (-.gd_g);
          add_g src src (-.gd_s);
          add_g src bulk (-.gd_b);
          let ieq_d = id -. (gd_d *. vd +. gd_g *. vg +. gd_s *. vs +. gd_b *. vb) in
          add_i d (-.ieq_d);
          add_i src ieq_d;
          s.lin.(ei) <- Lin_mos { gm = sgn *. gm; gds = sgn *. gds; gmb = sgn *. gmb }
      | Jfet { d; g; s = src; model; area; pchannel; _ } ->
          let sgn = if pchannel then -1.0 else 1.0 in
          let vd = node_v x ckt d and vg = node_v x ckt g and vs = node_v x ckt src in
          let vgs = sgn *. (vg -. vs) and vds = sgn *. (vd -. vs) in
          let k1 = 2.0 *. model.Models.j_beta *. area in
          let vgst = vgs -. (sgn *. model.Models.j_vto) in
          let id_n, gm, gds =
            if vgst <= 0.0 then (0.0, 0.0, 0.0)
            else if vds >= vgst then
              let ids = 0.5 *. k1 *. vgst *. vgst *. (1.0 +. model.Models.j_lambda *. vds) in
              let gm = k1 *. vgst *. (1.0 +. model.Models.j_lambda *. vds) in
              let gds = 0.5 *. k1 *. vgst *. vgst *. model.Models.j_lambda in
              (ids, gm, gds)
            else
              let ids = 0.5 *. k1 *. (2.0 *. vgst *. vds -. vds *. vds) *. (1.0 +. model.Models.j_lambda *. vds) in
              let gm = k1 *. vds *. (1.0 +. model.Models.j_lambda *. vds) in
              let gds =
                0.5 *. k1 *. ((2.0 *. vgst -. 2.0 *. vds) *. (1.0 +. model.Models.j_lambda *. vds)
                              +. (2.0 *. vgst *. vds -. vds *. vds) *. model.Models.j_lambda)
              in
              (ids, gm, gds)
          in
          let gd_d = sgn *. gds and gd_g = sgn *. gm and gd_s = sgn *. (-.gm -. gds) in
          let id = sgn *. id_n in
          add_g d d gd_d;
          add_g d g gd_g;
          add_g d src gd_s;
          add_g src d (-.gd_d);
          add_g src g (-.gd_g);
          add_g src src (-.gd_s);
          let ieq_d = id -. (gd_d *. vd +. gd_g *. vg +. gd_s *. vs) in
          add_i d (-.ieq_d);
          add_i src ieq_d;
          s.lin.(ei) <- Lin_jfet { gm = sgn *. gm; gds = sgn *. gds }
      | Bv { name; a; b; expr; br } -> (
          let ctx = bctx time_of_mode 0.0 in
          let vval =
            try Eval.eval ctx expr
            with Eval.Eval_error msg -> sim_errorf "%s: %s" name msg
          in
          (* numeric derivatives w.r.t. all unknowns *)
          let n = s.n in
          let deriv = Array.make n 0.0 in
          let xn = Array.copy x in
          let bv_of xm =
            let ctx =
              {
                ctx with
                v_node =
                  (fun nm ->
                    try xm.(Hashtbl.find s.node_of_name (norm_node nm)) with Not_found -> 0.0);
                v_diff =
                  (fun (n1, n2) ->
                    let vv n = try xm.(Hashtbl.find s.node_of_name (norm_node n)) with Not_found -> 0.0 in
                    vv n1 -. vv n2);
                i_branch =
                  (fun nm ->
                    match Hashtbl.find_opt s.branch_of_name (String.uppercase_ascii nm) with
                    | Some br -> xm.(ckt.n_nodes + br)
                    | None -> 0.0);
              }
            in
            (try Eval.eval ctx expr with Eval.Eval_error _ -> vval)
          in
          for j = 0 to n - 1 do
            let h = 1e-8 *. (1.0 +. Float.abs x.(j)) in
            xn.(j) <- x.(j) +. h;
            let vp = bv_of xn in
            xn.(j) <- x.(j);
            deriv.(j) <- (vp -. vval) /. h
          done;
          (* branch equation: Va - Vb - V(x) = 0 *)
          add_gb a br 1.0;
          add_gb b br (-1.0);
          add_bg br a 1.0;
          add_bg br b (-1.0);
          for j = 0 to ckt.n_nodes - 1 do
            add_bg br j (-.deriv.(j))
          done;
          for bb = 0 to ckt.n_branches - 1 do
            add_bb br bb (-.deriv.(ckt.n_nodes + bb))
          done;
          let const = vval -. Array.fold_left ( +. ) 0.0 (Array.mapi (fun j dd -> dd *. x.(j)) deriv) in
          add_ib br const;
          s.lin.(ei) <- Lin_b (Array.copy deriv))
      | Bi { name; a; b; expr } -> (
          let ctx = bctx time_of_mode 0.0 in
          let ival =
            try Eval.eval ctx expr
            with Eval.Eval_error msg -> sim_errorf "%s: %s" name msg
          in
          let n = s.n in
          let deriv = Array.make n 0.0 in
          let xn = Array.copy x in
          let bi_of xm =
            let ctx =
              {
                ctx with
                v_node =
                  (fun nm ->
                    try xm.(Hashtbl.find s.node_of_name (norm_node nm)) with Not_found -> 0.0);
                v_diff =
                  (fun (n1, n2) ->
                    let vv n = try xm.(Hashtbl.find s.node_of_name (norm_node n)) with Not_found -> 0.0 in
                    vv n1 -. vv n2);
                i_branch =
                  (fun nm ->
                    match Hashtbl.find_opt s.branch_of_name (String.uppercase_ascii nm) with
                    | Some br -> xm.(ckt.n_nodes + br)
                    | None -> 0.0);
              }
            in
            (try Eval.eval ctx expr with Eval.Eval_error _ -> ival)
          in
          for j = 0 to n - 1 do
            let h = 1e-8 *. (1.0 +. Float.abs x.(j)) in
            xn.(j) <- x.(j) +. h;
            let vp = bi_of xn in
            xn.(j) <- x.(j);
            deriv.(j) <- (vp -. ival) /. h
          done;
          let const = ival -. Array.fold_left ( +. ) 0.0 (Array.mapi (fun j dd -> dd *. x.(j)) deriv) in
          (* current ival flowing a -> b *)
          for j = 0 to ckt.n_nodes - 1 do
            add_g a j (-.deriv.(j));
            add_g b j deriv.(j)
          done;
          for bb = 0 to ckt.n_branches - 1 do
            add_gb a bb (-.deriv.(ckt.n_nodes + bb));
            add_gb b bb deriv.(ckt.n_nodes + bb)
          done;
          add_i a (-.const);
          add_i b const;
          s.lin.(ei) <- Lin_b (Array.copy deriv))
      | Vsw { a; b; ca; cb; model; _ } ->
          let vc = vdiff ca cb in
          let on =
            if s.sw_on.(ei) then vc >= model.Models.s_vt -. model.Models.s_vh
            else vc >= model.Models.s_vt +. model.Models.s_vh
          in
          s.sw_on.(ei) <- on;
          let r = if on then model.Models.s_ron else model.Models.s_roff in
          let gg = 1.0 /. r in
          add_g a a gg;
          add_g b b gg;
          add_g a b (-.gg);
          add_g b a (-.gg);
          s.lin.(ei) <- Lin_r r
      | Isw { a; b; ctrl; model; _ } ->
          let ic = branch_i x ckt ctrl in
          let on =
            if s.sw_on.(ei) then ic >= model.Models.s_vt -. model.Models.s_vh
            else ic >= model.Models.s_vt +. model.Models.s_vh
          in
          s.sw_on.(ei) <- on;
          let r = if on then model.Models.s_ron else model.Models.s_roff in
          let gg = 1.0 /. r in
          add_g a a gg;
          add_g b b gg;
          add_g a b (-.gg);
          add_g b a (-.gg);
          s.lin.(ei) <- Lin_r r)
    s.elems

(* ------------------------------------------------------------------ *)
(* Newton-Raphson operating-point solve                                *)
(* ------------------------------------------------------------------ *)

let max_abs xs = Array.fold_left (fun m v -> Float.max m (Float.abs v)) 0.0 xs

let converged opts x y =
  let nn = Array.length x in
  let ok = ref true in
  for i = 0 to nn - 1 do
    let tol = opts.Models.abstol +. opts.Models.reltol *. Float.max (Float.abs x.(i)) (Float.abs y.(i)) in
    if Float.abs (y.(i) -. x.(i)) > tol then ok := false
  done;
  !ok

let newton_solve s ~(mode : stamp_mode) ~(dc_over : (string * float) list) ~(guess : float array)
    : float array =
  let opts = s.ckt.options in
  let n = s.n in
  let solve_with ~gmin ~guess:guess0 =
    let x = Array.copy guess0 in
    let rec iter k =
      if k >= opts.Models.itl1 then `Fail
      else
        let g = Array.make_matrix n n 0.0 in
        let rhs = Array.make n 0.0 in
        stamp_real s ~mode ~gmin ~dc_over x g rhs;
        let y =
          try Solve.solve_real g rhs
          with Solve.Singular msg -> (
            if debug_enabled () then (
              Printf.eprintf "[newton] singular: %s\nG=\n" msg;
              Array.iter
                (fun row ->
                  Printf.eprintf "  [%s]\n"
                    (String.concat " " (List.map (Printf.sprintf "%.3g") (Array.to_list row))))
                g;
              Printf.eprintf "rhs=[%s]\n"
                (String.concat " " (List.map (Printf.sprintf "%.3g") (Array.to_list rhs))))
            else ();
            Array.make n Float.nan)
        in
        if Array.exists Float.is_nan y then `Fail
        else (
          (* step limiting: clamp node-voltage jumps to tame exponentials *)
          let dx_max = ref 0.0 in
          for i = 0 to s.ckt.n_nodes - 1 do
            dx_max := Float.max !dx_max (Float.abs (y.(i) -. x.(i)))
          done;
          let damp = if !dx_max > 2.0 then 2.0 /. !dx_max else 1.0 in
          let x_old = Array.copy x in
          let xn = Array.init n (fun i -> x.(i) +. damp *. (y.(i) -. x.(i))) in
          let jd = junction_damp s x_old xn in
          for i = 0 to n - 1 do
            x.(i) <- x_old.(i) +. jd *. (xn.(i) -. x_old.(i))
          done;
          if debug_enabled () then
            Printf.eprintf "[newton] it=%d dx_max=%.3g damp=%.3g jdamp=%.3g x=[%s]\n" k !dx_max
              damp jd
              (String.concat "," (List.map (Printf.sprintf "%.6g") (Array.to_list x)));
          if converged opts x_old y then `Ok x else iter (k + 1))
    in
    match iter 0 with
    | `Ok sol -> `Ok sol
    | `Fail -> `Fail
  in
  match solve_with ~gmin:opts.Models.gmin ~guess with
  | `Ok sol -> sol
  | `Fail ->
      (* GMIN stepping: solve easier (lossier) problems first, chain guesses. *)
      let rec step k guess_k =
        if k < 0 then `Fail
        else
          let gmin = opts.Models.gmin *. (10.0 ** float_of_int k) in
          match solve_with ~gmin ~guess:guess_k with
          | `Ok sol -> if k = 0 then `Ok sol else step (k - 1) sol
          | `Fail -> `Fail
      in
      (match step 3 guess with
      | `Ok sol -> sol
      | `Fail ->
          no_conv_f "Newton failed to converge (itl1=%d); check topology, floating nodes, or model values"
            opts.Models.itl1)

(* Update dynamic states after a converged TRAN step. [first_be] must match
   the stamp method of the step just taken (the first step is always BE). *)
let update_tran_state s ~(dt : float) ~(use_trap : bool) ~(first_be : bool)
    (x_prev : float array) (x : float array) =
  let trap_now = use_trap && not first_be in
  let ckt = s.ckt in
  Array.iteri
    (fun ei el ->
      match el with
      | C { a; b; c; _ } ->
          let vc = node_v x ckt a -. node_v x ckt b in
          let vc_prev = node_v x_prev ckt a -. node_v x_prev ckt b in
          s.cap_vc.(ei) <- vc;
          if trap_now then (
            let geq = 2.0 *. c /. dt in
            s.cap_ic.(ei) <- geq *. (vc -. vc_prev) -. s.cap_ic.(ei))
          else s.cap_ic.(ei) <- c /. dt *. (vc -. vc_prev)
      | L { a; b; _ } ->
          s.ind_vprev.(ei) <- node_v x ckt a -. node_v x ckt b
      | _ -> ())
    s.elems

(* Seed dynamic states from an OP solution (steady state: Ic=0, Vl=0). *)
let seed_tran_state s (x : float array) =
  let ckt = s.ckt in
  Array.iteri
    (fun ei el ->
      match el with
      | C { a; b; _ } ->
          s.cap_vc.(ei) <- node_v x ckt a -. node_v x ckt b;
          s.cap_ic.(ei) <- 0.0
      | L _ -> s.ind_vprev.(ei) <- 0.0
      | Vsw _ | Isw _ -> s.sw_on.(ei) <- false
      | _ -> ())
    s.elems

(* ------------------------------------------------------------------ *)
(* Public result types + analyses                                      *)
(* ------------------------------------------------------------------ *)

type op_point = {
  x : float array;
}

type op_result = {
  voltages : (string * float) list;
  currents : (string * float) list;
}

type dc_result = {
  sweep_names : string list;
  points : (float list * op_result) list;
}

type ac_result = {
  freqs : float array;
  v_nodes : (string * Complex.t array) list;
  i_branches : (string * Complex.t array) list;
}

type tran_result = {
  times : float array;
  v_nodes : (string * float array) list;
  i_branches : (string * float array) list;
}

type sim_result =
  | Op of op_result
  | Dc of dc_result
  | Ac of ac_result
  | Tran of tran_result

let op_of_x s (x : float array) : op_result =
  let ckt = s.ckt in
  let voltages =
    Array.to_list
      (Array.mapi
         (fun i nm ->
           let _ = i in
           (nm, x.(i)))
         ckt.node_names)
  in
  let currents =
    Array.to_list
      (Array.mapi
         (fun i nm ->
           let _ = i in
           (nm, x.(ckt.n_nodes + i)))
         ckt.branch_names)
  in
  { voltages; currents }

let solve_op ?(dc_over = []) ?(guess = None) s : float array * op_result =
  let n = s.n in
  let guess =
    match guess with
    | Some g -> g
    | None ->
        (* fresh OP: reset switch states *)
        Array.make n 0.0
  in
  (* reset switch states for a fresh OP *)
  Array.fill s.sw_on 0 s.nelems false;
  let x = newton_solve s ~mode:M_dc ~dc_over ~guess in
  (* settle switches: re-solve while any switch flips (bounded) *)
  let rec settle x flips =
    let before = Array.copy s.sw_on in
    let g = Array.make_matrix n n 0.0 in
    let rhs = Array.make n 0.0 in
    stamp_real s ~mode:M_dc ~gmin:s.ckt.options.Models.gmin ~dc_over x g rhs;
    let after = Array.copy s.sw_on in
    if after = before || flips >= 20 then x
    else
      let x = newton_solve s ~mode:M_dc ~dc_over ~guess:x in
      settle x (flips + 1)
  in
  let x = settle x 0 in
  s.op_valid <- dc_over = [];
  (x, op_of_x s x)

let run_dc s (sweeps : dc_sweep list) : dc_result =
  if sweeps = [] then sim_errorf ".DC with no sweeps";
  List.iter
    (fun sw ->
      if sw.step = 0.0 && sw.start <> sw.stop then
        sim_errorf ".DC %s: step is zero" sw.src)
    sweeps;
  (* resolve sweep targets *)
  let targets =
    List.map
      (fun sw ->
        let key = norm_name sw.src in
        let kind =
          if Hashtbl.mem s.branch_of_name key then `Source
          else
            let found =
              Array.exists
                (function
                  | R { name; _ } | C { name; _ } | L { name; _ } ->
                      norm_name name = key
                  | _ -> false)
                s.elems
            in
            if found then `Passive else sim_errorf ".DC: cannot sweep '%s' (only V/I/R/C/L)" sw.src
        in
        (sw, kind))
      sweeps
  in
  let points_of sw =
    if sw.start = sw.stop then [ sw.start ]
    else (
      let pts = ref [] in
      let v = ref sw.start in
      let guard = ref 0 in
      let upwards = sw.stop > sw.start in
      while !guard < 10001 && (if upwards then !v <= sw.stop +. 1e-30 else !v >= sw.stop -. 1e-30) do
        pts := !v :: !pts;
        v := !v +. sw.step;
        incr guard
      done;
      if !guard >= 10001 then sim_errorf ".DC %s: too many points (check step)" sw.src;
      List.rev !pts)
  in
  let grids = List.map (fun (sw, _) -> points_of sw) targets in
  (* cartesian product, outermost sweep slowest *)
  let rec product = function
    | [] -> [ [] ]
    | g :: rest ->
        List.concat_map (fun v -> List.map (fun r -> v :: r) (product rest)) g
  in
  let combos = product grids in
  let names = List.map (fun (sw, _) -> sw.src) targets in
  let guess = ref None in
  let points =
    List.map
      (fun combo ->
        let dc_over =
          List.map2
            (fun (sw, _) v ->
              let _ = sw in
              (norm_name sw.src, v))
            targets combo
        in
        let result =
          try solve_op ~dc_over ~guess:!guess s
          with No_convergence _ ->
            (* retry from zero before giving up on this point *)
            solve_op ~dc_over ~guess:None s
        in
        guess := Some (fst result);
        (combo, snd result))
      combos
  in
  { sweep_names = names; points }

(* Complex AC stamping (linearized around OP). *)
let stamp_ac s ~(freq : float) (gc : Complex.t array array) (rhsc : Complex.t array) =
  let ckt = s.ckt in
  if not s.op_valid then sim_errorf "AC needs a valid .OP (run OP first)";
  let omega = 2.0 *. Float.pi *. freq in
  let jw = { Complex.re = 0.0; im = omega } in
  let c_of_float f = { Complex.re = f; im = 0.0 } in
  let add_g i j v =
    if i >= 0 && j >= 0 then gc.(i).(j) <- Complex.add gc.(i).(j) v
  in
  let add_gb i br v =
    if i >= 0 then gc.(i).(ckt.n_nodes + br) <- Complex.add gc.(i).(ckt.n_nodes + br) v
  in
  let add_bg br j v =
    if j >= 0 then gc.(ckt.n_nodes + br).(j) <- Complex.add gc.(ckt.n_nodes + br).(j) v
  in
  let add_bb b1 b2 v =
    gc.(ckt.n_nodes + b1).(ckt.n_nodes + b2) <-
      Complex.add gc.(ckt.n_nodes + b1).(ckt.n_nodes + b2) v
  in
  let add_i i v = if i >= 0 then rhsc.(i) <- Complex.add rhsc.(i) v in
  let add_ib br v = rhsc.(ckt.n_nodes + br) <- Complex.add rhsc.(ckt.n_nodes + br) v in
  let gmin_c = c_of_float ckt.options.Models.gmin in
  if ckt.options.Models.gmin > 0.0 then
    for i = 0 to ckt.n_nodes - 1 do
      gc.(i).(i) <- Complex.add gc.(i).(i) gmin_c
    done;
  Array.iteri
    (fun ei el ->
      match el with
      | R { a; b; r; _ } ->
          let gg = c_of_float (1.0 /. r) in
          let ng = Complex.neg gg in
          add_g a a gg;
          add_g b b gg;
          add_g a b ng;
          add_g b a ng
      | C { a; b; c; _ } ->
          let y = Complex.mul jw (c_of_float c) in
          let ny = Complex.neg y in
          add_g a a y;
          add_g b b y;
          add_g a b ny;
          add_g b a ny
      | L _ -> () (* handled below via L matrix *)
      | Vsrc { a; b; ac; br; _ } ->
          let mag =
            Complex.polar ac.mag (ac.phase_deg *. Float.pi /. 180.0)
          in
          add_gb a br Complex.one;
          add_gb b br (Complex.neg Complex.one);
          add_bg br a Complex.one;
          add_bg br b (Complex.neg Complex.one);
          add_ib br mag
      | Isrc { a; b; ac; _ } ->
          let mag = Complex.polar ac.mag (ac.phase_deg *. Float.pi /. 180.0) in
          add_i a (Complex.neg mag);
          add_i b mag
      | Vcvs { ap; an; cp; cn; gain; br; _ } ->
          let gg = c_of_float gain in
          add_gb ap br Complex.one;
          add_gb an br (Complex.neg Complex.one);
          add_bg br ap Complex.one;
          add_bg br an (Complex.neg Complex.one);
          add_bg br cp (Complex.neg gg);
          add_bg br cn gg
      | Vccs { ap; an; cp; cn; gain; _ } ->
          let gg = c_of_float gain in
          let ng = Complex.neg gg in
          add_g ap cp gg;
          add_g ap cn ng;
          add_g an cp ng;
          add_g an cn gg
      | Cccs { a; b; ctrl; gain; _ } ->
          let gg = c_of_float gain in
          add_gb a ctrl gg;
          add_gb b ctrl (Complex.neg gg)
      | Ccvs { a; b; ctrl; gain; br; _ } ->
          let gg = c_of_float gain in
          add_gb a br Complex.one;
          add_gb b br (Complex.neg Complex.one);
          add_bg br a Complex.one;
          add_bg br b (Complex.neg Complex.one);
          add_bb br ctrl (Complex.neg gg)
      | Dio _ -> (
          match s.lin.(ei) with
          | Lin_dio gg ->
              let g = c_of_float gg and ng = c_of_float (-.gg) in
              (match s.elems.(ei) with
              | Dio { a; b; _ } ->
                  add_g a a g;
                  add_g b b g;
                  add_g a b ng;
                  add_g b a ng
              | _ -> assert false)
          | _ -> sim_errorf "AC: missing OP data for diode")
      | Bjt _ -> (
          match (s.lin.(ei), s.elems.(ei)) with
          | ( Lin_bjt l,
              Bjt { c; b; e; _ } ) ->
              let terms =
                [ (c, l.dc_db, l.dc_dc, l.dc_de); (b, l.db_db, l.db_dc, l.db_de) ]
              in
              List.iter
                (fun (row, gb, gc_, ge) ->
                  let gie = -.(gb +. gc_ +. ge) in
                  add_g row b (c_of_float gb);
                  add_g row c (c_of_float gc_);
                  add_g row e (c_of_float ge);
                  ignore gie)
                terms;
              (* emitter row = -(collector+base) rows *)
              let gb = -.(l.dc_db +. l.db_db) in
              let gc_ = -.(l.dc_dc +. l.db_dc) in
              let ge = -.(l.dc_de +. l.db_de) in
              add_g e b (c_of_float gb);
              add_g e c (c_of_float gc_);
              add_g e e (c_of_float ge)
          | _ -> sim_errorf "AC: missing OP data for BJT")
      | Mos _ -> (
          match (s.lin.(ei), s.elems.(ei)) with
          | Lin_mos l, Mos { d; g; s = src; bulk; _ } ->
              let gd_s = -.(l.gm +. l.gds +. l.gmb) in
              add_g d d (c_of_float l.gds);
              add_g d g (c_of_float l.gm);
              add_g d src (c_of_float gd_s);
              add_g d bulk (c_of_float l.gmb);
              add_g src d (c_of_float (-.l.gds));
              add_g src g (c_of_float (-.l.gm));
              add_g src src (c_of_float (-.gd_s));
              add_g src bulk (c_of_float (-.l.gmb))
          | _ -> sim_errorf "AC: missing OP data for MOSFET")
      | Jfet _ -> (
          match (s.lin.(ei), s.elems.(ei)) with
          | Lin_jfet l, Jfet { d; g; s = src; _ } ->
              let gd_s = -.(l.gm +. l.gds) in
              add_g d d (c_of_float l.gds);
              add_g d g (c_of_float l.gm);
              add_g d src (c_of_float gd_s);
              add_g src d (c_of_float (-.l.gds));
              add_g src g (c_of_float (-.l.gm));
              add_g src src (c_of_float (-.gd_s))
          | _ -> sim_errorf "AC: missing OP data for JFET")
      | Bv { a; b; br; _ } -> (
          match s.lin.(ei) with
          | Lin_b deriv ->
              add_gb a br Complex.one;
              add_gb b br (Complex.neg Complex.one);
              for j = 0 to ckt.n_nodes - 1 do
                add_bg br j (c_of_float (-.deriv.(j)))
              done;
              for bb = 0 to ckt.n_branches - 1 do
                add_bb br bb (c_of_float (-.deriv.(ckt.n_nodes + bb)))
              done
          | _ -> sim_errorf "AC: missing OP data for B-source")
      | Bi { a; b; _ } -> (
          match s.lin.(ei) with
          | Lin_b deriv ->
              for j = 0 to ckt.n_nodes - 1 do
                let v = c_of_float deriv.(j) in
                add_g a j (Complex.neg v);
                add_g b j v
              done;
              for bb = 0 to ckt.n_branches - 1 do
                let v = c_of_float deriv.(ckt.n_nodes + bb) in
                add_gb a bb (Complex.neg v);
                add_gb b bb v
              done
          | _ -> sim_errorf "AC: missing OP data for B-source")
      | Vsw { a; b; _ } | Isw { a; b; _ } -> (
          match s.lin.(ei) with
          | Lin_r r ->
              let gg = c_of_float (1.0 /. r) in
              let ng = Complex.neg gg in
              add_g a a gg;
              add_g b b gg;
              add_g a b ng;
              add_g b a ng
          | _ -> sim_errorf "AC: missing OP data for switch"))
    s.elems;
  (* Inductors (with coupling) as complex impedances on branch rows. *)
  let nl = Array.length s.lmat in
  (* map branch -> position *)
  Array.iteri
    (fun ei el ->
      match el with
      | L { a; b; br; _ } ->
          add_gb a br Complex.one;
          add_gb b br (Complex.neg Complex.one);
          add_bg br a Complex.one;
          add_bg br b (Complex.neg Complex.one);
          let li = Hashtbl.find s.lvec_idx_of_br br in
          (* branch equation: Va - Vb - jw L I = 0 *)
          for j = 0 to nl - 1 do
            let brj = ref (-1) in
            Hashtbl.iter (fun bb idx -> if idx = j then brj := bb) s.lvec_idx_of_br;
            add_bb br !brj (Complex.neg (Complex.mul jw (c_of_float s.lmat.(li).(j))))
          done
      | _ -> ())
    s.elems

let ac_freqs ~sweep ~npts ~fstart ~fstop =
  if fstart <= 0.0 || fstop <= 0.0 then sim_errorf ".AC frequencies must be positive";
  match sweep with
  | Lin ->
      let n = max 2 (int_of_float npts) in
      Array.init n (fun i -> fstart +. (fstop -. fstart) *. float_of_int i /. float_of_int (n - 1))
  | Dec ->
      let per = max 1 (int_of_float npts) in
      let decades = Float.log10 (fstop /. fstart) in
      if decades <= 0.0 then sim_errorf ".AC DEC needs fstop > fstart";
      let total = max 2 (int_of_float (float_of_int per *. decades) + 1) in
      Array.init total (fun i ->
          fstart *. (10.0 ** (float_of_int i /. float_of_int per)))
  | Oct ->
      let per = max 1 (int_of_float npts) in
      let octaves = Float.log2 (fstop /. fstart) in
      if octaves <= 0.0 then sim_errorf ".AC OCT needs fstop > fstart";
      let total = max 2 (int_of_float (float_of_int per *. octaves) + 1) in
      Array.init total (fun i -> fstart *. (2.0 ** (float_of_int i /. float_of_int per)))

let run_ac s ~sweep ~npts ~fstart ~fstop : ac_result =
  (* OP first for the small-signal model. *)
  let _ = solve_op s in
  let freqs = ac_freqs ~sweep ~npts ~fstart ~fstop in
  let n = s.n in
  let v_acc = Array.init s.ckt.n_nodes (fun _ -> Array.make (Array.length freqs) Complex.zero) in
  let i_acc = Array.init s.ckt.n_branches (fun _ -> Array.make (Array.length freqs) Complex.zero) in
  Array.iteri
    (fun fi f ->
      let gc = Array.make_matrix n n Complex.zero in
      let rhsc = Array.make n Complex.zero in
      stamp_ac s ~freq:f gc rhsc;
      let x =
        try Solve.solve_complex gc rhsc
        with Solve.Singular msg -> sim_errorf ".AC f=%g: %s (check topology)" f msg
      in
      for i = 0 to s.ckt.n_nodes - 1 do
        v_acc.(i).(fi) <- x.(i)
      done;
      for bb = 0 to s.ckt.n_branches - 1 do
        i_acc.(bb).(fi) <- x.(s.ckt.n_nodes + bb)
      done)
    freqs;
  let v_nodes = Array.to_list (Array.mapi (fun i nm -> (nm, v_acc.(i))) s.ckt.node_names) in
  let i_branches =
    Array.to_list (Array.mapi (fun i nm -> (nm, i_acc.(i))) s.ckt.branch_names)
  in
  { freqs; v_nodes; i_branches }

let run_tran s ~tstep ~tstop ~tstart ~uic : tran_result =
  if tstep <= 0.0 then sim_errorf ".TRAN tstep must be positive";
  if tstop <= 0.0 then sim_errorf ".TRAN tstop must be positive";
  let tstart = Float.max 0.0 tstart in
  let use_trap = s.ckt.options.Models.use_trapezoidal in
  (* initial state *)
  let x0 =
    if uic then (
      let x = Array.make s.n 0.0 in
      seed_tran_state s x;
      (* capacitor/inductor histories at zero *)
      Array.fill s.cap_vc 0 s.nelems 0.0;
      Array.fill s.cap_ic 0 s.nelems 0.0;
      Array.fill s.ind_vprev 0 s.nelems 0.0;
      Array.fill s.sw_on 0 s.nelems false;
      x)
    else (
      let x, _ = solve_op s in
      seed_tran_state s x;
      x)
  in
  (* nsteps guards float division noise (e.g. 2e-3/.1e-5 = 200.00000000000003)
     so the output grid lands exactly on multiples of tstep. *)
  let nsteps = max 1 (int_of_float (Float.ceil (tstop /. tstep -. 1e-9))) in
  let dt = tstop /. float_of_int nsteps in
  let times = ref [] in
  let snaps = ref [] in
  let x_prev = ref x0 in
  let t = ref 0.0 in
  if tstart = 0.0 then (
    times := 0.0 :: !times;
    snaps := Array.copy x0 :: !snaps);
  for step = 1 to nsteps do
    t := float_of_int step *. dt;
    let mode = M_tran { t = !t; dt; use_trap; first_be = step = 1 } in
    let x =
      try newton_solve s ~mode ~dc_over:[] ~guess:!x_prev
      with No_convergence _ ->
        no_conv_f ".TRAN failed at t=%g (dt=%g); try a smaller tstep or .OPTIONS METHOD=EULER" !t dt
    in
    update_tran_state s ~dt ~use_trap ~first_be:(step = 1) !x_prev x;
    x_prev := x;
    if !t >= tstart -. dt /. 2.0 then (
      times := !t :: !times;
      snaps := Array.copy x :: !snaps)
  done;
  let times = Array.of_list (List.rev !times) in
  let snaps = Array.of_list (List.rev !snaps) in
  let col_nodes =
    Array.to_list
      (Array.mapi
         (fun i nm -> (nm, Array.init (Array.length times) (fun k -> snaps.(k).(i))))
         s.ckt.node_names)
  in
  let col_br =
    Array.to_list
      (Array.mapi
         (fun i nm ->
           (nm, Array.init (Array.length times) (fun k -> snaps.(k).(s.ckt.n_nodes + i))))
         s.ckt.branch_names)
  in
  { times; v_nodes = col_nodes; i_branches = col_br }

(* ------------------------------------------------------------------ *)
(* Driver                                                              *)
(* ------------------------------------------------------------------ *)

let run s (a : analysis) : sim_result =
  match a with
  | Op ->
      let _, op = solve_op s in
      Op op
  | Dc sweeps -> Dc (run_dc s sweeps)
  | Ac { sweep; npts; fstart; fstop } -> Ac (run_ac s ~sweep ~npts ~fstart ~fstop)
  | Tran { tstep; tstop; tstart; uic } -> Tran (run_tran s ~tstep ~tstop ~tstart ~uic)

let run_all s : (analysis * sim_result) list = List.map (fun a -> (a, run s a)) s.ckt.analyses

(* ------------------------------------------------------------------ *)
(* Text output (used by the CLI)                                       *)
(* ------------------------------------------------------------------ *)

let fmt_f v =
  if v = 0.0 then "0"
  else
    let a = Float.abs v in
    if (a >= 0.001 && a < 100000.0) || v = 0.0 then Printf.sprintf "%.6g" v else Printf.sprintf "%.6e" v

let string_of_op title (op : op_result) =
  let b = Buffer.create 256 in
  Buffer.add_string b ("Operating point: " ^ title ^ "\n");
  List.iter (fun (n, v) -> Buffer.add_string b (Printf.sprintf "  V(%s) = %s V\n" n (fmt_f v))) op.voltages;
  List.iter (fun (n, v) -> Buffer.add_string b (Printf.sprintf "  I(%s) = %s A\n" n (fmt_f v))) op.currents;
  Buffer.contents b

let string_of_dc title (dc : dc_result) =
  let b = Buffer.create 512 in
  Buffer.add_string b ("DC transfer: " ^ title ^ "\n");
  Buffer.add_string b ("  " ^ String.concat "  " dc.sweep_names ^ "\n");
  List.iter
    (fun (combo, op) ->
      Buffer.add_string b ("  [" ^ String.concat ", " (List.map fmt_f combo) ^ "]");
      List.iter (fun (n, v) -> Buffer.add_string b (Printf.sprintf "  V(%s)=%s" n (fmt_f v))) op.voltages;
      Buffer.add_char b '\n')
    dc.points;
  Buffer.contents b

let string_of_ac title (ac : ac_result) =
  let b = Buffer.create 512 in
  Buffer.add_string b ("AC analysis: " ^ title ^ "\n");
  Array.iteri
    (fun fi f ->
      Buffer.add_string b (Printf.sprintf "  f=%s Hz" (fmt_f f));
      List.iter
        (fun (n, v) ->
          let m = Complex.norm v.(fi) and ph = Complex.arg v.(fi) *. 180.0 /. Float.pi in
          Buffer.add_string b (Printf.sprintf "  V(%s)=%s<%sg" n (fmt_f m) (fmt_f ph)))
        ac.v_nodes;
      Buffer.add_char b '\n')
    ac.freqs;
  Buffer.contents b

let string_of_tran title (tr : tran_result) =
  let b = Buffer.create 1024 in
  Buffer.add_string b ("Transient: " ^ title ^ "\n");
  let show solare =
    min solare 2000 (* cap printed rows *)
  in
  let rows = show (Array.length tr.times) in
  Buffer.add_string b "  time";
  List.iter (fun (n, _) -> Buffer.add_string b ("  V(" ^ n ^ ")")) tr.v_nodes;
  Buffer.add_char b '\n';
  for k = 0 to rows - 1 do
    Buffer.add_string b ("  " ^ fmt_f tr.times.(k));
    List.iter (fun (_, arr) -> Buffer.add_string b ("  " ^ fmt_f arr.(k))) tr.v_nodes;
    Buffer.add_char b '\n'
  done;
  if rows < Array.length tr.times then
    Buffer.add_string b (Printf.sprintf "  ... (%d more rows)\n" (Array.length tr.times - rows));
  Buffer.contents b

let string_of_result title = function
  | Op op -> string_of_op title op
  | Dc dc -> string_of_dc title dc
  | Ac ac -> string_of_ac title ac
  | Tran tr -> string_of_tran title tr

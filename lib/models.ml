(* Semiconductor + switch model parameters parsed from .MODEL cards.
   Pure OCaml. Parameter keys are matched case-insensitively. *)

open Ast

exception Model_error of string

let boltzmann = 1.380649e-23
let electron_charge = 1.602176634e-19
let celsius_to_kelvin = 273.15

let thermal_voltage ?(temp_c = 27.0) () =
  boltzmann *. (temp_c +. celsius_to_kelvin) /. electron_charge

type diode_model = {
  d_is : float;
  d_n : float;
  d_rs : float;
}

type bjt_model = {
  b_is : float;
  b_bf : float;
  b_br : float;
  b_nf : float;
  b_nr : float;
  b_vaf : float;
  b_var : float;
  b_pnp : bool;
}

type mos_polarity =
  | Nmos
  | Pmos

type mos_model = {
  m_polarity : mos_polarity;
  m_vto : float;
  m_kp : float;
  m_lambda : float;
  m_gamma : float;
  m_phi : float;
}

type jfet_model = {
  j_vto : float;
  j_beta : float;
  j_lambda : float;
  j_pchan : bool;
}

type switch_model = {
  s_vt : float;
  s_vh : float;
  s_ron : float;
  s_roff : float;
}

type model_entry =
  | DModel of diode_model
  | BjtModel of bjt_model
  | MosModel of mos_model
  | JfetModel of jfet_model
  | SwModel of switch_model
  | OtherModel of string

type model_table = (string * model_entry) list

(* Case-insensitive assoc lookup on evaluated float params. *)
let find_param params key =
  let ukey = String.uppercase_ascii key in
  List.find_opt (fun (k, _) -> String.uppercase_ascii k = ukey) params
  |> Option.map snd

let eval_params const_params expr_params =
  List.map
    (fun (k, e) ->
      try (k, Eval.eval_const const_params e)
      with Eval.Eval_error msg ->
        raise (Model_error ("cannot evaluate model parameter '" ^ k ^ "': " ^ msg)))
    expr_params

let diode_of_params p =
  {
    d_is = Option.value ~default:1e-14 (find_param p "IS");
    d_n = Option.value ~default:1.0 (find_param p "N");
    d_rs = Option.value ~default:0.0 (find_param p "RS");
  }

let bjt_of_params mtype p =
  {
    b_is = Option.value ~default:1e-16 (find_param p "IS");
    b_bf = Option.value ~default:100.0 (find_param p "BF");
    b_br = Option.value ~default:1.0 (find_param p "BR");
    b_nf = Option.value ~default:1.0 (find_param p "NF");
    b_nr = Option.value ~default:1.0 (find_param p "NR");
    b_vaf = Option.value ~default:Float.infinity (find_param p "VAF");
    b_var = Option.value ~default:Float.infinity (find_param p "VAR");
    b_pnp = String.uppercase_ascii mtype = "PNP";
  }

let mos_of_params mtype p =
  let polarity =
    match String.uppercase_ascii mtype with
    | "PMOS" -> Pmos
    | _ -> Nmos
  in
  (* SPICE defaults VTO to 0 and lets the device fall back to depletion-style
     behavior; we default to 1.0/-1.0 V enhancement so a bare model still
     switches. An explicit VTO always wins. *)
  let default_vto =
    match (polarity, find_param p "VTO") with
    | _, Some v -> v
    | Nmos, None -> 1.0
    | Pmos, None -> -1.0
  in
  {
    m_polarity = polarity;
    m_vto = default_vto;
    m_kp = Option.value ~default:2e-5 (find_param p "KP");
    m_lambda = Option.value ~default:0.0 (find_param p "LAMBDA");
    m_gamma = Option.value ~default:0.0 (find_param p "GAMMA");
    m_phi = Option.value ~default:0.6 (find_param p "PHI");
  }

let jfet_of_params mtype p =
  {
    j_vto = Option.value ~default:(-2.0) (find_param p "VTO");
    j_beta = Option.value ~default:1e-4 (find_param p "BETA");
    j_lambda = Option.value ~default:0.0 (find_param p "LAMBDA");
    j_pchan = String.uppercase_ascii mtype = "PJF";
  }

let switch_of_params is_current p =
  if is_current then
    {
      s_vt = Option.value ~default:0.0 (find_param p "IT");
      s_vh = Option.value ~default:0.0 (find_param p "IH");
      s_ron = Option.value ~default:1.0 (find_param p "RON");
      s_roff = Option.value ~default:1e9 (find_param p "ROFF");
    }
  else
    {
      s_vt = Option.value ~default:0.0 (find_param p "VT");
      s_vh = Option.value ~default:0.0 (find_param p "VH");
      s_ron = Option.value ~default:1.0 (find_param p "RON");
      s_roff = Option.value ~default:1e9 (find_param p "ROFF");
    }

(* Build one model entry from a .MODEL card. [const_params] are the already
   evaluated .PARAM values visible at that point in the netlist. *)
let entry_of_model ~const_params ~name:_ ~mtype ~mparams =
  let p = eval_params const_params mparams in
  match String.uppercase_ascii mtype with
  | "D" -> DModel (diode_of_params p)
  | "NPN" | "PNP" -> BjtModel (bjt_of_params mtype p)
  | "NMOS" | "PMOS" -> MosModel (mos_of_params mtype p)
  | "NJF" | "PJF" -> JfetModel (jfet_of_params mtype p)
  | "SW" -> SwModel (switch_of_params false p)
  | "CSW" -> SwModel (switch_of_params true p)
  | other -> OtherModel other

let add table name entry = (String.lowercase_ascii name, entry) :: table

let find_opt table name =
  List.find_opt (fun (k, _) -> k = String.lowercase_ascii name) table
  |> Option.map snd

type sim_options = {
  reltol : float;
  abstol : float;
  vntol : float;
  gmin : float;
  itl1 : int;
  use_trapezoidal : bool;
  defw : float;
  defl : float;
}

let default_options =
  {
    reltol = 1e-3;
    abstol = 1e-12;
    vntol = 1e-6;
    gmin = 1e-12;
    itl1 = 100;
    use_trapezoidal = true;
    defw = 100e-6;
    defl = 100e-6;
  }

(* Fold .OPTIONS cards (in order) over the defaults. Unknown keys ignored. *)
let options_of_cards ~const_params cards =
  let eval_opt = function
    | None -> None
    | Some e -> (
        try Some (Eval.eval_const const_params e)
        with Eval.Eval_error _ -> None)
  in
  List.fold_left
    (fun opt (k, v) ->
      match String.uppercase_ascii k with
      | "RELTOL" -> (
          match eval_opt v with
          | Some x -> { opt with reltol = x }
          | None -> opt)
      | "ABSTOL" -> (
          match eval_opt v with
          | Some x -> { opt with abstol = x }
          | None -> opt)
      | "VNTOL" -> (
          match eval_opt v with
          | Some x -> { opt with vntol = x }
          | None -> opt)
      | "GMIN" -> (
          match eval_opt v with
          | Some x -> { opt with gmin = x }
          | None -> opt)
      | "ITL1" -> (
          match eval_opt v with
          | Some x -> { opt with itl1 = int_of_float x }
          | None -> opt)
      | "DEFW" -> (
          match eval_opt v with
          | Some x -> { opt with defw = x }
          | None -> opt)
      | "DEFL" -> (
          match eval_opt v with
          | Some x -> { opt with defl = x }
          | None -> opt)
      | "METHOD" -> (
          match v with
          | Some (EVar m) -> (
              match String.uppercase_ascii m with
              | "EULER" | "GEAR" -> { opt with use_trapezoidal = false }
              | "TRAP" | "TRAPEZOIDAL" -> { opt with use_trapezoidal = true }
              | _ -> opt)
          | _ -> opt)
      | _ -> opt)
    default_options
    cards

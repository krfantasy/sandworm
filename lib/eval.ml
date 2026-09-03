(* Expression evaluation for parameters, device values, and B-sources.
   Pure OCaml, no dependencies beyond Ast. *)

open Ast

exception Eval_error of string

(* Evaluation context. [v_node] resolves V(node) / V(n1,n2) lookups,
   [i_branch] resolves I(vname) lookups. Both are only available while a
   simulation iterate exists (B-source evaluation); constant folding passes
   callbacks that raise. *)
type ctx = {
  params : (string * float) list;
  time : float;
  freq : float;
  v_node : (string -> float);
  v_diff : (string * string -> float);
  i_branch : (string -> float);
}

let no_circuit_ctx params =
  let missing what _ = raise (Eval_error ("'" ^ what ^ "' not available in constant context")) in
  {
    params;
    time = 0.0;
    freq = 0.0;
    v_node = missing "V()";
    v_diff = missing "V()";
    i_branch = missing "I()";
  }

let get_param params name =
  let key = String.uppercase_ascii name in
  match List.find_opt (fun (k, _) -> String.uppercase_ascii k = key) params with
  | Some (_, v) -> Some v
  | None -> None

let rec eval (c : ctx) (e : expr) : float =
  match e with
  | ENum n -> n
  | EStr _ -> raise (Eval_error "string literal cannot be used as a number")
  | EVar v -> (
      match get_param c.params v with
      | Some x -> x
      | None -> (
          match String.lowercase_ascii v with
          | "time" | "$time" -> c.time
          | "freq" | "frequency" -> c.freq
          | "pi" -> Float.pi
          | "e" -> Float.exp 1.0
          | "temp" | "tnom" -> 27.0
          | _ -> raise (Eval_error ("unknown variable or parameter '" ^ v ^ "'"))))
  | EUnary (Neg, e1) -> -.eval c e1
  | EUnary (Not, e1) -> if eval c e1 = 0.0 then 1.0 else 0.0
  | EBinop (op, e1, e2) -> (
      let a = eval c e1 in
      let b = eval c e2 in
      match op with
      | Add -> a +. b
      | Sub -> a -. b
      | Mul -> a *. b
      | Div -> a /. b
      | Pow -> a ** b
      | Lt -> if a < b then 1.0 else 0.0
      | Gt -> if a > b then 1.0 else 0.0
      | Le -> if a <= b then 1.0 else 0.0
      | Ge -> if a >= b then 1.0 else 0.0
      | Eq -> if a = b then 1.0 else 0.0
      | Ne -> if a <> b then 1.0 else 0.0
      | And -> if a <> 0.0 && b <> 0.0 then 1.0 else 0.0
      | Or -> if a <> 0.0 || b <> 0.0 then 1.0 else 0.0)
  | ETernary (cond, t, f) -> if eval c cond <> 0.0 then eval c t else eval c f
  | EIndex (base, _) -> eval c base
  | ECall (f, args) -> eval_call c f args

and eval_call (c : ctx) (f : string) (args : expr list) : float =
  let n = List.length args in
  let arg i = eval c (List.nth args i) in
  match String.lowercase_ascii f with
  | "v" -> (
      (* V(node), V(node1, node2), V(branch)? SPICE allows V(n+,n-) *)
      match args with
      | [ EVar a ] -> c.v_node a
      | [ ENum a ] -> c.v_node (node_of_num a)
      | [ a; b ] -> c.v_diff (name_of_atom a, name_of_atom b)
      | _ -> raise (Eval_error "V() takes 1 or 2 arguments"))
  | "i" | "ib" -> (
      match args with
      | [ EVar a ] -> c.i_branch a
      | _ -> raise (Eval_error "I() takes a voltage-source name"))
  | "sin" when n = 1 -> Float.sin (arg 0)
  | "cos" when n = 1 -> Float.cos (arg 0)
  | "tan" when n = 1 -> Float.tan (arg 0)
  | "asin" when n = 1 -> Float.asin (arg 0)
  | "acos" when n = 1 -> Float.acos (arg 0)
  | "atan" when n = 1 -> Float.atan (arg 0)
  | "atan2" when n = 2 -> Float.atan2 (arg 0) (arg 1)
  | "sinh" when n = 1 -> Float.sinh (arg 0)
  | "cosh" when n = 1 -> Float.cosh (arg 0)
  | "tanh" when n = 1 -> Float.tanh (arg 0)
  | "exp" when n = 1 -> Float.exp (arg 0)
  | "limexp" when n = 1 -> (
      (* SPICE limited exp: linear continuation past |x| = 80, for B-sources *)
      let x = arg 0 in
      if x > 80.0 then Float.exp 80.0 *. (1.0 +. x -. 80.0)
      else if x < -80.0 then 0.0
      else Float.exp x)
  | "log" when n = 1 -> Float.log (arg 0)
  | "log10" when n = 1 -> Float.log10 (arg 0)
  | "sqrt" when n = 1 -> Float.sqrt (arg 0)
  | "abs" when n = 1 -> Float.abs (arg 0)
  | "pow" when n = 2 -> (arg 0) ** (arg 1)
  | "pwr" when n = 2 ->
      let a = arg 0 and b = arg 1 in
      if a < 0.0 then raise (Eval_error "pwr() of negative base") else a ** b
  | "min" when n >= 1 -> List.fold_left (fun acc e -> Float.min acc (eval c e)) Float.infinity args
  | "max" when n >= 1 -> List.fold_left (fun acc e -> Float.max acc (eval c e)) Float.neg_infinity args
  | "limit" | "minmax" when n = 3 ->
      let x = arg 0 and lo = arg 1 and hi = arg 2 in
      Float.min hi (Float.max lo x)
  | "sgn" when n = 1 ->
      let x = arg 0 in
      if x > 0.0 then 1.0 else if x < 0.0 then -1.0 else 0.0
  | "stp" | "step" when n = 1 -> if arg 0 >= 0.0 then 1.0 else 0.0
  | "u" when n = 1 -> if arg 0 >= 0.0 then 1.0 else 0.0
  | "uramp" when n = 1 -> Float.max 0.0 (arg 0)
  | "ddt" | "sdt" | "idt" ->
      raise
        (Eval_error
           (f ^ "() time-derivative/integral sources are not supported in this version"))
  | "white" | "flicker" | "agauss" | "aunif" | "aexprand" ->
      raise (Eval_error (f ^ "() stochastic sources are not supported"))
  | _ -> (
      (* Unknown call with all-numeric args inside a transient func position
         (e.g. PULSE treated as ECall elsewhere) is an error here. *)
      match get_param c.params f with
      | Some _ -> raise (Eval_error ("parameter '" ^ f ^ "' used as function"))
      | None -> raise (Eval_error ("unknown function '" ^ f ^ "'")))

and node_of_num a =
  let i = int_of_float a in
  if float_of_int i = a then string_of_int i
  else raise (Eval_error "numeric node name must be integral")

and name_of_atom = function
  | EVar s -> s
  | ENum n -> node_of_num n
  | e -> raise (Eval_error ("V() argument must be a node name, got " ^ show_expr e))

(* Constant folding: no V()/I()/time allowed. *)
let eval_const params e =
  eval (no_circuit_ctx params) e

let eval_opt_const params = function
  | None -> None
  | Some e -> (
      try Some (eval_const params e)
      with Eval_error _ -> None)

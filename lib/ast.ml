(* Expression types - adapted from ngspice parse-bison.y *)

type binop =
  | Add | Sub | Mul | Div | Pow
  | Lt  | Gt  | Le  | Ge  | Eq  | Ne
  | And | Or

type unaryop = Neg | Not

type expr =
  | ENum of float
  | EVar of string
  | EStr of string
  | ECall of string * expr list          (* V(1), sin(x), ddt(V(1)) *)
  | EUnary of unaryop * expr
  | EBinop of binop * expr * expr
  | ETernary of expr * expr * expr       (* a ? b : c *)
  | EIndex of expr * expr                (* a[i] *)

(* Device argument - generic for all device types *)
type device_arg =
  | PosId of string                      (* positional identifier (node, model name) *)
  | PosNum of float                      (* positional number *)
  | PosStr of string                     (* positional quoted string *)
  | PosFunc of string * expr list        (* PULSE(0 5 0 1n 1n 5n 10n) *)
  | KwParam of string * expr             (* L=1u, W=10u, V=expr *)

type device = {
  name : string;
  args : device_arg list;
}

(* Dot commands *)
type ac_sweep = Dec | Oct | Lin

type dot_cmd =
  | Op
  | Ac of ac_sweep * float * float * float
  | Tran of float * float * float option
  | Dc of (string * float * float * float) list
  | Model of string * string * (string * expr) list
  | Subckt of string * string list
  | Ends of string option
  | Param of (string * expr) list
  | Options of (string * expr option) list
  | Global of string list
  | Include of string
  | Lib of string * string option
  | Save of string list
  | End
  | Control of string                   (* opaque text between .control/.endc *)
  | DotRaw of string * device_arg list  (* unknown/deferred dot commands *)

type statement =
  | Device of device
  | DotCmd of dot_cmd

type netlist = statement list

let rec show_expr = function
  | ENum n -> Float.to_string n
  | EVar s -> s
  | EStr s -> "\"" ^ s ^ "\""
  | ECall (f, args) -> f ^ "(" ^ String.concat ", " (List.map show_expr args) ^ ")"
  | EUnary (Neg, e) -> "-(" ^ show_expr e ^ ")"
  | EUnary (Not, e) -> "!(" ^ show_expr e ^ ")"
  | EBinop (op, e1, e2) ->
    let op_str = match op with
      | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Pow -> "^"
      | Lt -> "<" | Gt -> ">" | Le -> "<=" | Ge -> ">=" | Eq -> "=" | Ne -> "!="
      | And -> "&" | Or -> "||"
    in
    "(" ^ show_expr e1 ^ " " ^ op_str ^ " " ^ show_expr e2 ^ ")"
  | ETernary (c, t, f) ->
    "(" ^ show_expr c ^ " ? " ^ show_expr t ^ " : " ^ show_expr f ^ ")"
  | EIndex (e, i) -> show_expr e ^ "[" ^ show_expr i ^ "]"

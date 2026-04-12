%{
open Ast

(* Dot command dispatch: parse a generic DOT_CMD + args into specific dot_cmd variants *)
let parse_dot_cmd cmd args =
  match cmd with
  | "op" -> Op
  | "ac" ->
    (match args with
     | [PosId sweep; PosNum n1; PosNum n2; PosNum n3] ->
       let sw = match String.lowercase_ascii sweep with
         | "dec" -> Dec | "oct" -> Oct | "lin" -> Lin
         | _ -> Dec
       in
       Ac (sw, n1, n2, n3)
     | _ -> DotRaw (cmd, args))
  | "tran" ->
    (match args with
     | [PosNum step; PosNum stop] -> Tran (step, stop, None)
     | [PosNum step; PosNum stop; PosNum start] -> Tran (step, stop, Some start)
     | _ -> DotRaw (cmd, args))
  | "dc" ->
    let parse_dc_entry = function
      | [PosId v; PosNum start; PosNum stop; PosNum step] ->
        Some (v, start, stop, step)
      | _ -> None
    in
    let rec group_dc acc args =
      match args with
      | [] -> List.rev acc
      | a :: b :: c :: d :: rest ->
        (match parse_dc_entry [a; b; c; d] with
         | Some entry -> group_dc (entry :: acc) rest
         | None -> List.rev acc)
      | _ -> List.rev acc
    in
    let entries = group_dc [] args in
    if entries <> [] then Dc entries
    else DotRaw (cmd, args)
  | "model" ->
    (match args with
     | PosId name :: PosId mtype :: rest ->
       let params = List.filter_map (function
         | KwParam (k, v) -> Some (k, v)
         | _ -> None
       ) rest in
       Model (name, mtype, params)
     | PosId name :: PosStr mtype :: rest ->
       let params = List.filter_map (function
         | KwParam (k, v) -> Some (k, v)
         | _ -> None
       ) rest in
       Model (name, mtype, params)
     (* Model type followed by parenthesized params: .MODEL DMOD D(IS=1E-14)
        parses as PosFunc("D", [EBinop(Eq, EVar "IS", ENum ...)]) *)
     | [PosId name; PosFunc (mtype, exprs)] ->
       let params = List.filter_map (function
         | EBinop (Eq, EVar k, v) -> Some (k, v)
         | _ -> None
       ) exprs in
       Model (name, mtype, params)
     | _ -> DotRaw (cmd, args))
  | "subckt" ->
    let ids = List.filter_map (function PosId s -> Some s | _ -> None) args in
    (match ids with
     | name :: nodes -> Subckt (name, nodes)
     | _ -> DotRaw (cmd, args))
  | "ends" ->
    (match args with
     | [PosId s] -> Ends (Some s)
     | [] -> Ends None
     | _ -> Ends None)
  | "param" ->
    let params = List.filter_map (function
      | KwParam (k, v) -> Some (k, v)
      | _ -> None
    ) args in
    Param params
  | "options" | "option" ->
    let opts = List.filter_map (function
      | KwParam (k, v) -> Some (k, Some v)
      | PosId k -> Some (k, None)
      | _ -> None
    ) args in
    Options opts
  | "global" ->
    let ids = List.filter_map (function PosId s -> Some s | _ -> None) args in
    Global ids
  | "include" | "inc" ->
    (match args with
     | [PosStr s] -> Include s
     | [PosId s] -> Include s
     | _ -> DotRaw (cmd, args))
  | "lib" ->
    (match args with
     | [PosStr s] -> Lib (s, None)
     | [PosStr s; PosId sec] -> Lib (s, Some sec)
     | [PosId s] -> Lib (s, None)
     | _ -> DotRaw (cmd, args))
  | "save" ->
    let ids = List.filter_map (function PosId s -> Some s | _ -> None) args in
    Save ids
  | "end" -> End
  | "control" -> Control ""
  | _ -> DotRaw (cmd, args)

%}

%token <float> NUM
%token <string> ID
%token <string> STRING
%token PLUS MINUS TIMES DIVIDE POWER
%token LT GT LE GE EQ NE
%token AND OR NOT
%token LPAREN RPAREN LBRACKET RBRACKET
%token COMMA SEMICOLON COLON QUEST
%token NEWLINE EOF
%token <string> DOT_CMD

%right QUEST COLON
%left OR
%left AND
%left EQ NE
%left LT GT LE GE
%left PLUS MINUS
%left TIMES DIVIDE
%right UMINUS NOT
%right POWER

%start netlist
%type <Ast.netlist> netlist

%%

netlist:
  | EOF
    { [] }
  | list = terminated(statement_list, EOF)
    { list }

statement_list:
  | (* empty *)
    { [] }
  | xs = statement_list; NEWLINE
    { xs }
  | xs = statement_list; s = statement
    { xs @ [s] }

statement:
  | d = device_line
    { Device d }
  | d = dot_command
    { DotCmd d }

(* Device lines: name followed by positional/keyword arguments *)
device_line:
  | name = ID; args = device_args
    { { name; args } }

device_args:
  | (* empty *)
    { [] }
  | xs = device_args; x = device_arg
    { xs @ [x] }

device_arg:
  | n = NUM
    { PosNum n }
  | s = STRING
    { PosStr s }
  | k = ID; EQ; v = expr
    { KwParam (k, v) }
  | f = ID; LPAREN; args = func_arg_list; RPAREN
    { PosFunc (f, args) }
  | id = ID
    { PosId id }

(* Function arguments: space-separated expressions inside parens
   (for PULSE, SIN, PWL etc.) *)
func_arg_list:
  | (* empty *)
    { [] }
  | xs = func_arg_list_inner
    { xs }

func_arg_list_inner:
  | e = expr
    { [e] }
  | xs = func_arg_list_inner; e = expr
    { xs @ [e] }

(* Dot commands: DOT_CMD followed by arguments, dispatched in OCaml *)
dot_command:
  | cmd = DOT_CMD; args = dot_args
    { parse_dot_cmd cmd args }

dot_args:
  | (* empty *)
    { [] }
  | xs = dot_args; x = dot_arg
    { xs @ [x] }

dot_arg:
  | n = NUM
    { PosNum n }
  | s = STRING
    { PosStr s }
  | k = ID; EQ; v = expr
    { KwParam (k, v) }
  | f = ID; LPAREN; args = func_arg_list; RPAREN
    { PosFunc (f, args) }
  | id = ID
    { PosId id }

(* Expression grammar - from ngspice parse-bison.y *)
expr:
  | n = NUM
    { ENum n }
  | id = ID
    { EVar id }
  | s = STRING
    { EStr s }
  | LPAREN; e = expr; RPAREN
    { e }
  | MINUS; e = expr; %prec UMINUS
    { EUnary (Neg, e) }
  | NOT; e = expr
    { EUnary (Not, e) }
  | e1 = expr; PLUS; e2 = expr
    { EBinop (Add, e1, e2) }
  | e1 = expr; MINUS; e2 = expr
    { EBinop (Sub, e1, e2) }
  | e1 = expr; TIMES; e2 = expr
    { EBinop (Mul, e1, e2) }
  | e1 = expr; DIVIDE; e2 = expr
    { EBinop (Div, e1, e2) }
  | e1 = expr; POWER; e2 = expr
    { EBinop (Pow, e1, e2) }
  | e1 = expr; LT; e2 = expr
    { EBinop (Lt, e1, e2) }
  | e1 = expr; GT; e2 = expr
    { EBinop (Gt, e1, e2) }
  | e1 = expr; LE; e2 = expr
    { EBinop (Le, e1, e2) }
  | e1 = expr; GE; e2 = expr
    { EBinop (Ge, e1, e2) }
  | e1 = expr; EQ; e2 = expr
    { EBinop (Eq, e1, e2) }
  | e1 = expr; NE; e2 = expr
    { EBinop (Ne, e1, e2) }
  | e1 = expr; AND; e2 = expr
    { EBinop (And, e1, e2) }
  | e1 = expr; OR; e2 = expr
    { EBinop (Or, e1, e2) }
  | id = ID; LPAREN; args = separated_list(COMMA, expr); RPAREN
    { ECall (id, args) }
  | e1 = expr; QUEST; e2 = expr; COLON; e3 = expr
    { ETernary (e1, e2, e3) }
  | e1 = expr; LBRACKET; e2 = expr; RBRACKET
    { EIndex (e1, e2) }

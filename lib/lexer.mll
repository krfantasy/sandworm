{
open Parser

(* Parse a SPICE number string into a float.
   Handles mantissa, decimal point, exponent (e/E), and engineering suffixes.
   Multi-char suffixes MEG/MIL are checked before single-char M to get longest match.
   Mirrors the logic in vendor/ngspice/src/frontend/parser/numparse.c *)
let parse_spice_number s =
  let len = String.length s in
  let i = ref 0 in
  let get () = if !i < len then s.[!i] else '\000' in
  let advance () = incr i in

  (* Optional sign *)
  let sign = ref 1.0 in
  (match get () with
   | '-' -> sign := -1.0; advance ()
   | '+' -> advance ()
   | _ -> ());

  (* Integer part *)
  let int_val = ref 0.0 in
  (while let c = get () in c >= '0' && c <= '9' do
     int_val := !int_val *. 10.0 +. float_of_int (Char.code (get ()) - Char.code '0');
     advance ()
   done);

  (* Fractional part *)
  let frac_val = ref 0.0 in
  let frac_digits = ref 0 in
  if get () = '.' then begin
    advance ();
    (while let c = get () in c >= '0' && c <= '9' do
       frac_val := !frac_val *. 10.0 +. float_of_int (Char.code (get ()) - Char.code '0');
       incr frac_digits;
       advance ()
     done)
  end;

  let mantissa = ref (!sign *. (!int_val +. !frac_val *. (10.0 ** float_of_int (- !frac_digits)))) in

  (* Exponent or suffix *)
  let expo = ref 0.0 in
  let c = Char.lowercase_ascii (get ()) in

  (* Check for 'e'/'E' scientific notation *)
  if c = 'e' then begin
    advance ();
    let exp_sign = ref 1.0 in
    (match get () with
     | '-' -> exp_sign := -1.0; advance ()
     | '+' -> advance ()
     | _ -> ());
    let exp_val = ref 0.0 in
    (while let c2 = get () in c2 >= '0' && c2 <= '9' do
       exp_val := !exp_val *. 10.0 +. float_of_int (Char.code (get ()) - Char.code '0');
       advance ()
     done);
    expo := !exp_sign *. !exp_val
  end

  (* Multi-char suffixes: MEG, MIL — check before single-char M *)
  else if c = 'm' && !i + 2 < len then begin
    let c1 = Char.lowercase_ascii s.[!i + 1] in
    let c2 = Char.lowercase_ascii s.[!i + 2] in
    if c1 = 'e' && c2 = 'g' then begin
      expo := 6.0;
      i := !i + 3
    end else if c1 = 'i' && c2 = 'l' then begin
      expo := -6.0;
      mantissa := !mantissa *. 25.4;
      i := !i + 3
    end else begin
      expo := -3.0;
      advance ()
    end
  end

  (* Single-char suffixes *)
  else begin
    (match c with
     | 't' -> expo := 12.0; advance ()
     | 'g' -> expo := 9.0; advance ()
     | 'k' -> expo := 3.0; advance ()
     | 'm' -> expo := -3.0; advance ()
     | 'u' -> expo := -6.0; advance ()
     | 'n' -> expo := -9.0; advance ()
     | 'p' -> expo := -12.0; advance ()
     | 'f' -> expo := -15.0; advance ()
     | 'a' -> expo := -18.0; advance ()
     | _ -> ());
  end;

  !mantissa *. (if !expo = 0.0 then 1.0 else 10.0 ** !expo)

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = alpha | digit | '_' | '$'
let whitespace = [' ' '\t']

(* SPICE number: optional sign, digits/decimal, optional exponent or engineering suffix.
   Multi-char suffixes (MEG/MIL) are matched first for longest-match. *)
let spice_number = '-'? digit+ ('.' digit*)?
  | '-'? '.' digit+
  | '-'? digit+ ('.' digit*)? ['e' 'E'] ['+' '-']? digit+
  | '-'? digit+ ('.' digit*)? ['m' 'M'] ['e' 'E'] ['g' 'G']
  | '-'? digit+ ('.' digit*)? ['m' 'M'] ['i' 'I'] ['l' 'L']
  | '-'? digit+ ('.' digit*)? ['t' 'T' 'g' 'G' 'k' 'K' 'm' 'M' 'u' 'U' 'n' 'N' 'p' 'P' 'f' 'F' 'a' 'A']

(* Line-start tokenization.
   Same as token, except:
   - * or # at line start = comment (skip line)
   - .alpha... at line start = DOT_CMD
   - + at line start = continuation (skip +, continue tokenizing) *)
rule line_start = parse
  | whitespace
      { line_start lexbuf }
  | '\n'
      { NEWLINE }
  | eof
      { EOF }
  (* Line-start specific: comment *)
  | '*' | '#'
      { skip_to_eol lexbuf }
  (* Line-start specific: dot command *)
  | '.' alpha (alpha | digit | '_')* as cmd
      { DOT_CMD (String.lowercase_ascii (String.sub cmd 1 (String.length cmd - 1))) }
  (* Line-start specific: continuation +, skip it and tokenize rest *)
  | '+'
      { token lexbuf }
  (* Below: same patterns as token rule *)
  | spice_number as num
      { NUM (parse_spice_number num) }
  | "<="
      { LE }
  | ">="
      { GE }
  | "!="
      { NE }
  | "<>"
      { NE }
  | "||"
      { OR }
  | '-'
      { MINUS }
  | '/'
      { DIVIDE }
  | '^'
      { POWER }
  | '<'
      { LT }
  | '>'
      { GT }
  | '='
      { EQ }
  | '&'
      { AND }
  | '!'
      { NOT }
  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | '['
      { LBRACKET }
  | ']'
      { RBRACKET }
  | ','
      { COMMA }
  | ';'
      { SEMICOLON }
  | '"'
      { let s = read_double_string (Buffer.create 16) lexbuf in STRING s }
  | '\''
      { let s = read_single_string (Buffer.create 16) lexbuf in STRING s }
  | (alpha | '_' | '$' | '#' | '%') (alphanum | '.' | ':' | '$' | '#' | '%')* as id
      { ID id }
  | '\\' '\n'
      { line_start lexbuf }

(* Normal mid-line tokenization *)
and token = parse
  | whitespace
      { token lexbuf }
  | '\n'
      { NEWLINE }
  | eof
      { EOF }
  | spice_number as num
      { NUM (parse_spice_number num) }
  (* Two-char operators *)
  | "<="
      { LE }
  | ">="
      { GE }
  | "!="
      { NE }
  | "<>"
      { NE }
  | "||"
      { OR }
  (* Single-char operators and delimiters *)
  | '+'
      { PLUS }
  | '-'
      { MINUS }
  | '*'
      { TIMES }
  | '/'
      { DIVIDE }
  | '^'
      { POWER }
  | '<'
      { LT }
  | '>'
      { GT }
  | '='
      { EQ }
  | '&'
      { AND }
  | '!'
      { NOT }
  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | '['
      { LBRACKET }
  | ']'
      { RBRACKET }
  | ','
      { COMMA }
  | ';'
      { SEMICOLON }
  (* Strings *)
  | '"'
      { let s = read_double_string (Buffer.create 16) lexbuf in STRING s }
  | '\''
      { let s = read_single_string (Buffer.create 16) lexbuf in STRING s }
  (* Identifiers *)
  | (alpha | '_' | '$' | '#' | '%') (alphanum | '.' | ':' | '$' | '#' | '%')* as id
      { ID id }
  (* Backslash continuation *)
  | '\\' '\n'
      { token lexbuf }

and skip_to_eol = parse
  | '\n'
      { NEWLINE }
  | eof
      { EOF }
  | _
      { skip_to_eol lexbuf }

and read_double_string buf = parse
  | '"'
      { Buffer.contents buf }
  | '\\'
      { let c = read_escaped_char lexbuf in
        Buffer.add_char buf c;
        read_double_string buf lexbuf }
  | '\n' | eof
      { Buffer.contents buf }
  | _
      { let c = Lexing.lexeme_char lexbuf 0 in
        Buffer.add_char buf c;
        read_double_string buf lexbuf }

and read_escaped_char = parse
  | _
      { Lexing.lexeme_char lexbuf 0 }

and read_single_string buf = parse
  | '\''
      { Buffer.contents buf }
  | '\n' | eof
      { Buffer.contents buf }
  | _
      { let c = Lexing.lexeme_char lexbuf 0 in
        Buffer.add_char buf c;
        read_single_string buf lexbuf }

{
let tokenize_all s =
  let buf = Lexing.from_string s in
  let rec aux acc at_line_start =
    let tok =
      if at_line_start then line_start buf
      else token buf
    in
    if tok = EOF then List.rev acc
    else aux (tok :: acc) (tok = NEWLINE)
  in
  aux [] true

(* Token supplier for menhir: tracks line-start state *)
let next_token =
  let at_line_start = ref true in
  fun lexbuf ->
    let tok =
      if !at_line_start then line_start lexbuf
      else token lexbuf
    in
    (match tok with
     | EOF | NEWLINE -> at_line_start := true
     | _ -> at_line_start := false);
    tok
}

exception Parse_error of string

let parse_string s =
  let lexbuf = Lexing.from_string s in
  try Parser.netlist Lexer.next_token lexbuf
  with Parser.Error ->
    let pos = Lexing.lexeme_start_p lexbuf in
    let msg = Printf.sprintf "Parse error at line %d, column %d"
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
    in
    raise (Parse_error msg)

let parse_file filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  (* SPICE: first line is always the title, skip it *)
  let rest =
    match String.index_opt content '\n' with
    | Some i -> String.sub content (i + 1) (String.length content - i - 1)
    | None -> ""
  in
  parse_string rest

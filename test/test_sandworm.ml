open Sandworm

(* --- Helpers --- *)

let tok_eq expected got =
  Alcotest.check Alcotest.string __LOC__ (Token.to_string expected) (Token.to_string got)

let tokenize input = Lexer.tokenize_all input

(* --- Number parsing --- *)

let num_parsing = [
  ("integers", `Quick, fun () ->
      let tokens = tokenize "42 0 1000\n" in
      let nums = List.filter_map (function Parser.NUM f -> Some f | _ -> None) tokens in
      tok_eq (Parser.NUM 42.0) (Parser.NUM (List.nth nums 0));
      tok_eq (Parser.NUM 0.0) (Parser.NUM (List.nth nums 1));
      tok_eq (Parser.NUM 1000.0) (Parser.NUM (List.nth nums 2)));

  ("decimals", `Quick, fun () ->
      let tokens = tokenize "3.14 0.5 .5 1.\n" in
      let nums = List.filter_map (function Parser.NUM f -> Some f | _ -> None) tokens in
      tok_eq (Parser.NUM 3.14) (Parser.NUM (List.nth nums 0));
      tok_eq (Parser.NUM 0.5) (Parser.NUM (List.nth nums 1));
      tok_eq (Parser.NUM 0.5) (Parser.NUM (List.nth nums 2));
      tok_eq (Parser.NUM 1.0) (Parser.NUM (List.nth nums 3)));

  ("scientific notation", `Quick, fun () ->
      let tokens = tokenize "1e-3 2.5E6 1e+2\n" in
      let nums = List.filter_map (function Parser.NUM f -> Some f | _ -> None) tokens in
      tok_eq (Parser.NUM 0.001) (Parser.NUM (List.nth nums 0));
      tok_eq (Parser.NUM 2_500_000.0) (Parser.NUM (List.nth nums 1));
      tok_eq (Parser.NUM 100.0) (Parser.NUM (List.nth nums 2)));

  ("engineering suffix T/G/K", `Quick, fun () ->
      let tokens = tokenize "1T 2G 3K\n" in
      let nums = List.filter_map (function Parser.NUM f -> Some f | _ -> None) tokens in
      tok_eq (Parser.NUM 1e12) (Parser.NUM (List.nth nums 0));
      tok_eq (Parser.NUM 2e9) (Parser.NUM (List.nth nums 1));
      tok_eq (Parser.NUM 3e3) (Parser.NUM (List.nth nums 2)));

  ("engineering suffix m/u/n/p/f/a", `Quick, fun () ->
      let tokens = tokenize "1m 2u 3n 4p 5f 6a\n" in
      let nums = List.filter_map (function Parser.NUM f -> Some f | _ -> None) tokens in
      tok_eq (Parser.NUM 1e-3) (Parser.NUM (List.nth nums 0));
      tok_eq (Parser.NUM 2e-6) (Parser.NUM (List.nth nums 1));
      tok_eq (Parser.NUM 3e-9) (Parser.NUM (List.nth nums 2));
      tok_eq (Parser.NUM 4e-12) (Parser.NUM (List.nth nums 3));
      tok_eq (Parser.NUM 5e-15) (Parser.NUM (List.nth nums 4));
      tok_eq (Parser.NUM 6e-18) (Parser.NUM (List.nth nums 5)));

  ("MEG suffix", `Quick, fun () ->
      let tokens = tokenize "1MEG 10meg\n" in
      let nums = List.filter_map (function Parser.NUM f -> Some f | _ -> None) tokens in
      tok_eq (Parser.NUM 1e6) (Parser.NUM (List.nth nums 0));
      tok_eq (Parser.NUM 10e6) (Parser.NUM (List.nth nums 1)));

  ("MIL suffix", `Quick, fun () ->
      let tokens = tokenize "1MIL 2mil\n" in
      let nums = List.filter_map (function Parser.NUM f -> Some f | _ -> None) tokens in
      (* ngspice: MIL = 25.4e-6 (mils to meters) *)
      tok_eq (Parser.NUM 25.4e-6) (Parser.NUM (List.nth nums 0));
      tok_eq (Parser.NUM 50.8e-6) (Parser.NUM (List.nth nums 1)));
]

(* --- Comment handling --- *)

let comment_tests = [
  ("star comment at line start", `Quick, fun () ->
      let tokens = tokenize "* this is a comment\nR1 1 2 1K\n" in
      (* The comment line produces NEWLINE, then the device line tokens *)
      let non_nl = List.filter (function Parser.NEWLINE -> false | _ -> true) tokens in
      tok_eq (Parser.ID "R1") (List.nth non_nl 0);
      tok_eq (Parser.NUM 1.0) (List.nth non_nl 1);
      tok_eq (Parser.NUM 2.0) (List.nth non_nl 2);
      tok_eq (Parser.NUM 1000.0) (List.nth non_nl 3));

  ("hash comment at line start", `Quick, fun () ->
      let tokens = tokenize "# a comment\n.END\n" in
      let non_nl = List.filter (function Parser.NEWLINE -> false | _ -> true) tokens in
      tok_eq (Parser.DOT_CMD "end") (List.nth non_nl 0));

  ("mid-line star is TIMES", `Quick, fun () ->
      let tokens = tokenize "2 * 3\n" in
      let non_nl = List.filter (function Parser.NEWLINE -> false | _ -> true) tokens in
      tok_eq (Parser.NUM 2.0) (List.nth non_nl 0);
      tok_eq Parser.TIMES (List.nth non_nl 1);
      tok_eq (Parser.NUM 3.0) (List.nth non_nl 2));
]

(* --- Dot commands --- *)

let dot_cmd_tests = [
  ("dot commands lowercased", `Quick, fun () ->
      let tokens = tokenize ".AC\n.MODEL\n.SUBCKT\n.ENDS\n.END\n" in
      let cmds = List.filter_map (function Parser.DOT_CMD s -> Some s | _ -> None) tokens in
      Alcotest.(check string) __LOC__ "ac" (List.nth cmds 0);
      Alcotest.(check string) __LOC__ "model" (List.nth cmds 1);
      Alcotest.(check string) __LOC__ "subckt" (List.nth cmds 2);
      Alcotest.(check string) __LOC__ "ends" (List.nth cmds 3);
      Alcotest.(check string) __LOC__ "end" (List.nth cmds 4));
]

(* --- Strings --- *)

let string_tests = [
  ("double-quoted string", `Quick, fun () ->
      let tokens = tokenize "\"hello world\"\n" in
      let non_nl = List.filter (function Parser.NEWLINE -> false | _ -> true) tokens in
      tok_eq (Parser.STRING "hello world") (List.nth non_nl 0));

  ("single-quoted string", `Quick, fun () ->
      let tokens = tokenize "'hello world'\n" in
      let non_nl = List.filter (function Parser.NEWLINE -> false | _ -> true) tokens in
      tok_eq (Parser.STRING "hello world") (List.nth non_nl 0));

  ("double-quoted with backslash escape", `Quick, fun () ->
      let tokens = tokenize "\"hello\\\"world\"\n" in
      let non_nl = List.filter (function Parser.NEWLINE -> false | _ -> true) tokens in
      tok_eq (Parser.STRING "hello\"world") (List.nth non_nl 0));
]

(* --- Operators --- *)

let operator_tests = [
  ("arithmetic operators", `Quick, fun () ->
      let tokens = tokenize "1 + 2 - 3 * 4 / 5 ^ 6\n" in
      let ops = [Parser.PLUS; Parser.MINUS; Parser.TIMES; Parser.DIVIDE; Parser.POWER] in
      let found = List.filter_map (function
          | (Parser.PLUS | Parser.MINUS | Parser.TIMES | Parser.DIVIDE | Parser.POWER) as op -> Some op
          | _ -> None) tokens in
      List.iter2 (fun e g -> tok_eq e g) ops found);

  ("comparison operators", `Quick, fun () ->
      let tokens = tokenize "1 < 2 > 3 <= 4 >= 5 = 6 != 7\n" in
      let ops = [Parser.LT; Parser.GT; Parser.LE; Parser.GE; Parser.EQ; Parser.NE] in
      let found = List.filter_map (function
          | (Parser.LT | Parser.GT | Parser.LE | Parser.GE | Parser.EQ | Parser.NE) as op -> Some op
          | _ -> None) tokens in
      List.iter2 (fun e g -> tok_eq e g) ops found);

  ("logical operators", `Quick, fun () ->
      let tokens = tokenize "& || !\n" in
      let non_nl = List.filter (function Parser.NEWLINE -> false | _ -> true) tokens in
      tok_eq Parser.AND (List.nth non_nl 0);
      tok_eq Parser.OR (List.nth non_nl 1);
      tok_eq Parser.NOT (List.nth non_nl 2));

  ("delimiters", `Quick, fun () ->
      let tokens = tokenize "( ) [ ] , ;\n" in
      let expected = [Parser.LPAREN; Parser.RPAREN; Parser.LBRACKET; Parser.RBRACKET; Parser.COMMA; Parser.SEMICOLON] in
      let non_nl = List.filter (function Parser.NEWLINE -> false | _ -> true) tokens in
      List.iter2 (fun e g -> tok_eq e g) expected non_nl);
]

(* --- Continuation lines --- *)

let continuation_tests = [
  ("plus continuation", `Quick, fun () ->
      let tokens = tokenize "R1 1\n+ 2 1K\n" in
      let non_nl = List.filter (function Parser.NEWLINE -> false | _ -> true) tokens in
      tok_eq (Parser.ID "R1") (List.nth non_nl 0);
      tok_eq (Parser.NUM 1.0) (List.nth non_nl 1);
      tok_eq (Parser.NUM 2.0) (List.nth non_nl 2);
      tok_eq (Parser.NUM 1000.0) (List.nth non_nl 3));

  ("backslash continuation", `Quick, fun () ->
      let tokens = tokenize "R1 \\\n1 2 1K\n" in
      let non_nl = List.filter (function Parser.NEWLINE -> false | _ -> true) tokens in
      tok_eq (Parser.ID "R1") (List.nth non_nl 0);
      tok_eq (Parser.NUM 1.0) (List.nth non_nl 1);
      tok_eq (Parser.NUM 2.0) (List.nth non_nl 2);
      tok_eq (Parser.NUM 1000.0) (List.nth non_nl 3));
]

(* --- Full netlist --- *)

let netlist_tests = [
  ("minimal complete netlist", `Quick, fun () ->
      let netlist = {|
* Simple resistor divider
R1 1 2 1K
R2 2 0 2K
V1 1 0 DC 5
.OP
.END
|} in
      let tokens = tokenize netlist in
      (* Should end with DOT_CMD "end" before any trailing newlines/EOF *)
      let meaningful = List.filter (function
          | Parser.NEWLINE | Parser.EOF -> false
          | _ -> true) tokens in
      tok_eq (Parser.DOT_CMD "end") (List.nth meaningful (List.length meaningful - 1)));

  ("netlist with model", `Quick, fun () ->
      let netlist = {|
D1 1 0 DMOD
.MODEL DMOD D(IS=1E-14)
.END
|} in
      let tokens = tokenize netlist in
      let cmds = List.filter_map (function Parser.DOT_CMD s -> Some s | _ -> None) tokens in
      Alcotest.(check string) __LOC__ "model" (List.nth cmds 0);
      Alcotest.(check string) __LOC__ "end" (List.nth cmds 1));
]

(* --- Test suite --- *)

let () =
  Alcotest.run "sandworm" [
    "number parsing", num_parsing;
    "comments", comment_tests;
    "dot commands", dot_cmd_tests;
    "strings", string_tests;
    "operators", operator_tests;
    "continuation", continuation_tests;
    "full netlist", netlist_tests;
  ]

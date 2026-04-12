open Sandworm
open Ast

(* --- Helpers --- *)

let parse input = Netlist.parse_string input

let check_float loc expected got =
  Alcotest.(check (float 0.001)) loc expected got

(* --- Simple devices --- *)

let device_tests = [
  ("simple resistor", `Quick, fun () ->
      let nl = parse "R1 1 2 1K\n.END\n" in
      match nl with
      | [Device d; DotCmd End] ->
        Alcotest.(check string) __LOC__ "R1" d.name;
        (* Nodes 1, 2 are numeric tokens -> PosNum, value 1K -> PosNum *)
        (match d.args with
         | [PosNum n1; PosNum n2; PosNum n3] ->
           check_float __LOC__ 1.0 n1;
           check_float __LOC__ 2.0 n2;
           check_float __LOC__ 1000.0 n3
         | _ -> Alcotest.fail "wrong resistor args")
      | _ -> Alcotest.fail "wrong parse structure");

  ("capacitor", `Quick, fun () ->
      let nl = parse "C1 a b 1n\n.END\n" in
      match nl with
      | [Device d; DotCmd End] ->
        Alcotest.(check string) __LOC__ "C1" d.name;
        (match d.args with
         | [PosId "a"; PosId "b"; PosNum n] ->
           check_float __LOC__ 1e-9 n
         | _ -> Alcotest.fail "wrong cap args")
      | _ -> Alcotest.fail "wrong parse structure");

  ("inductor", `Quick, fun () ->
      let nl = parse "L1 1 0 10u\n.END\n" in
      match nl with
      | [Device d; DotCmd End] ->
        Alcotest.(check string) __LOC__ "L1" d.name;
        (match d.args with
         | [PosNum n1; PosNum n2; PosNum n3] ->
           check_float __LOC__ 1.0 n1;
           check_float __LOC__ 0.0 n2;
           check_float __LOC__ 10e-6 n3
         | _ -> Alcotest.fail "wrong inductor args")
      | _ -> Alcotest.fail "wrong parse structure");

  ("device with keyword params (MOSFET)", `Quick, fun () ->
      let nl = parse "M1 d g s b nmos L=1u W=10u\n.END\n" in
      match nl with
      | [Device d; DotCmd End] ->
        Alcotest.(check string) __LOC__ "M1" d.name;
        let ids = List.filter_map (function PosId s -> Some s | _ -> None) d.args in
        Alcotest.(check string) __LOC__ "d" (List.nth ids 0);
        Alcotest.(check string) __LOC__ "g" (List.nth ids 1);
        Alcotest.(check string) __LOC__ "s" (List.nth ids 2);
        Alcotest.(check string) __LOC__ "b" (List.nth ids 3);
        Alcotest.(check string) __LOC__ "nmos" (List.nth ids 4);
        let kws = List.filter_map (function KwParam (k, v) -> Some (k, v) | _ -> None) d.args in
        Alcotest.(check int) __LOC__ 2 (List.length kws);
        let (k1, v1) = List.nth kws 0 in
        let (k2, v2) = List.nth kws 1 in
        Alcotest.(check string) __LOC__ "L" k1;
        Alcotest.(check string) __LOC__ "W" k2;
        (match v1 with ENum n -> check_float __LOC__ 1e-6 n | _ -> Alcotest.fail "L value");
        (match v2 with ENum n -> check_float __LOC__ 10e-6 n | _ -> Alcotest.fail "W value")
      | _ -> Alcotest.fail "wrong parse structure");
]

(* --- Source functions --- *)

let source_tests = [
  ("PULSE source", `Quick, fun () ->
      let nl = parse "V1 in 0 PULSE(0 5 0 1n 1n 5n 10n)\n.END\n" in
      match nl with
      | [Device d; DotCmd End] ->
        (match d.args with
         | [PosId "in"; PosNum n0; PosFunc ("PULSE", args)] ->
           check_float __LOC__ 0.0 n0;
           Alcotest.(check int) __LOC__ 7 (List.length args);
           (match args with
            | [ENum v1; ENum v2; ENum v3; ENum v4; ENum v5; ENum v6; ENum v7] ->
              check_float __LOC__ 0.0 v1;
              check_float __LOC__ 5.0 v2;
              check_float __LOC__ 0.0 v3;
              check_float __LOC__ 1e-9 v4;
              check_float __LOC__ 1e-9 v5;
              check_float __LOC__ 5e-9 v6;
              check_float __LOC__ 10e-9 v7
            | _ -> Alcotest.fail "wrong PULSE args")
         | _ -> Alcotest.fail "wrong V1 args")
      | _ -> Alcotest.fail "wrong parse structure");

  ("DC source with string value", `Quick, fun () ->
      let nl = parse "V1 1 0 DC 5\n.END\n" in
      match nl with
      | [Device d; DotCmd End] ->
        (match d.args with
         | [PosNum n1; PosNum n2; PosId "DC"; PosNum n5] ->
           check_float __LOC__ 1.0 n1;
           check_float __LOC__ 0.0 n2;
           check_float __LOC__ 5.0 n5
         | _ -> Alcotest.fail "wrong V1 args")
      | _ -> Alcotest.fail "wrong parse structure");
]

(* --- Dot commands --- *)

let dot_cmd_tests = [
  (".OP", `Quick, fun () ->
      let nl = parse ".OP\n.END\n" in
      match nl with
      | [DotCmd Op; DotCmd End] -> ()
      | _ -> Alcotest.fail "wrong .OP parse");

  (".AC dec", `Quick, fun () ->
      let nl = parse ".AC dec 10 1 1k\n.END\n" in
      match nl with
      | [DotCmd (Ac (Dec, n1, n2, n3)); DotCmd End] ->
        check_float __LOC__ 10.0 n1;
        check_float __LOC__ 1.0 n2;
        check_float __LOC__ 1000.0 n3
      | _ -> Alcotest.fail "wrong .AC parse");

  (".TRAN", `Quick, fun () ->
      let nl = parse ".TRAN 1n 100n\n.END\n" in
      match nl with
      | [DotCmd (Tran (step, stop, None)); DotCmd End] ->
        check_float __LOC__ 1e-9 step;
        check_float __LOC__ 100e-9 stop
      | _ -> Alcotest.fail "wrong .TRAN parse");

  (".TRAN with start", `Quick, fun () ->
      let nl = parse ".TRAN 1n 100n 0\n.END\n" in
      match nl with
      | [DotCmd (Tran (step, stop, Some start)); DotCmd End] ->
        check_float __LOC__ 1e-9 step;
        check_float __LOC__ 100e-9 stop;
        check_float __LOC__ 0.0 start
      | _ -> Alcotest.fail "wrong .TRAN parse");

  (".DC sweep", `Quick, fun () ->
      let nl = parse ".DC V1 0 5 0.1\n.END\n" in
      match nl with
      | [DotCmd (Dc [(v, s, e, st)]); DotCmd End] ->
        Alcotest.(check string) __LOC__ "V1" v;
        check_float __LOC__ 0.0 s;
        check_float __LOC__ 5.0 e;
        check_float __LOC__ 0.1 st
      | _ -> Alcotest.fail "wrong .DC parse");

  (".MODEL", `Quick, fun () ->
      let nl = parse ".MODEL DMOD D(IS=1E-14)\n.END\n" in
      match nl with
      | [DotCmd (Model (name, mtype, params)); DotCmd End] ->
        Alcotest.(check string) __LOC__ "DMOD" name;
        Alcotest.(check string) __LOC__ "D" mtype;
        Alcotest.(check int) __LOC__ 1 (List.length params);
        let (pk, pv) = List.nth params 0 in
        Alcotest.(check string) __LOC__ "IS" pk;
        (match pv with ENum n -> check_float __LOC__ 1e-14 n | _ -> Alcotest.fail "IS value")
      | _ -> Alcotest.fail "wrong .MODEL parse");

  (".SUBCKT / .ENDS", `Quick, fun () ->
      let nl = parse ".SUBCKT mysub a b c\n.ENDS\n.END\n" in
      match nl with
      | [DotCmd (Subckt (name, nodes)); DotCmd (Ends None); DotCmd End] ->
        Alcotest.(check string) __LOC__ "mysub" name;
        Alcotest.(check int) __LOC__ 3 (List.length nodes)
      | _ -> Alcotest.fail "wrong .SUBCKT parse");

  (".OPTIONS", `Quick, fun () ->
      let nl = parse ".OPTIONS RELTOL=1e-4 NOPLOT\n.END\n" in
      match nl with
      | [DotCmd (Options opts); DotCmd End] ->
        Alcotest.(check int) __LOC__ 2 (List.length opts);
        let (k1, v1) = List.nth opts 0 in
        Alcotest.(check string) __LOC__ "RELTOL" k1;
        (match v1 with Some (ENum n) -> check_float __LOC__ 1e-4 n | _ -> Alcotest.fail "RELTOL");
        let (k2, v2) = List.nth opts 1 in
        Alcotest.(check string) __LOC__ "NOPLOT" k2;
        (match v2 with None -> () | _ -> Alcotest.fail "NOPLOT should be None")
      | _ -> Alcotest.fail "wrong .OPTIONS parse");

  (".END only", `Quick, fun () ->
      let nl = parse ".END\n" in
      match nl with
      | [DotCmd End] -> ()
      | _ -> Alcotest.fail "wrong .END parse");
]

(* --- Expressions --- *)

let expr_tests = [
  ("simple number", `Quick, fun () ->
      let nl = parse "R1 1 0 1K\n.END\n" in
      match nl with
      | [Device d; DotCmd End] ->
        (match List.nth d.args 2 with
         | PosNum n -> check_float __LOC__ 1000.0 n
         | _ -> Alcotest.fail "not a num")
      | _ -> Alcotest.fail "wrong parse");

  ("expression with parens", `Quick, fun () ->
      let nl = parse "B1 1 0 V=(1+2)*3\n.END\n" in
      match nl with
      | [Device d; DotCmd End] ->
        (match List.nth d.args 2 with
         | KwParam ("V", EBinop (Mul, EBinop (Add, ENum _, ENum _), ENum _)) -> ()
         | _ -> Alcotest.fail "wrong expression structure")
      | _ -> Alcotest.fail "wrong parse");

  ("precedence: mul over add", `Quick, fun () ->
      let nl = parse "B1 1 0 V=1+2*3\n.END\n" in
      match nl with
      | [Device d; DotCmd End] ->
        (match List.nth d.args 2 with
         | KwParam ("V", EBinop (Add, ENum 1.0, EBinop (Mul, ENum 2.0, ENum 3.0))) -> ()
         | _ -> Alcotest.fail "wrong precedence")
      | _ -> Alcotest.fail "wrong parse");
]

(* --- Full netlists --- *)

let full_netlist_tests = [
  ("resistor divider", `Quick, fun () ->
      let nl = parse {|
R1 1 2 1K
R2 2 0 2K
V1 1 0 DC 5
.OP
.END
|} in
      let devices = List.filter_map (function Device d -> Some d | _ -> None) nl in
      Alcotest.(check int) __LOC__ 3 (List.length devices);
      let cmds = List.filter_map (function DotCmd c -> Some c | _ -> None) nl in
      Alcotest.(check int) __LOC__ 2 (List.length cmds));

  ("netlist with model", `Quick, fun () ->
      let nl = parse {|
D1 1 0 DMOD
.MODEL DMOD D(IS=1E-14)
.END
|} in
      Alcotest.(check int) __LOC__ 3 (List.length nl));
]

(* --- Test suite --- *)

let () =
  Alcotest.run "parser" [
    "devices", device_tests;
    "sources", source_tests;
    "dot commands", dot_cmd_tests;
    "expressions", expr_tests;
    "full netlists", full_netlist_tests;
  ]

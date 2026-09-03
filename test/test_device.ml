open Sandworm
open Ast
open Device

(* --- Helpers --- *)

let parse_and_validate input =
  let nl = Netlist.parse_string input in
  Device.validate_netlist nl

let parse_device input =
  let nl = Netlist.parse_string input in
  match nl with
  | [Device d] -> Device.validate_device d
  | [Device d; DotCmd End] -> Device.validate_device d
  | _ -> Alcotest.fail "expected single device"

let check_float loc expected got =
  Alcotest.(check (float 0.001)) loc expected got

(* --- Two-terminal passives: R, C, L --- *)

let passive_tests = [
  ("resistor: basic", `Quick, fun () ->
      match parse_device "R1 1 2 1K\n.END\n" with
      | Ok (Resistor r) ->
        Alcotest.(check string) __LOC__ "R1" r.name;
        Alcotest.(check string) __LOC__ "1" r.pos_node;
        Alcotest.(check string) __LOC__ "2" r.neg_node;
        (match r.value with Some (ENum n) -> check_float __LOC__ 1000.0 n | _ -> Alcotest.fail "value")
      | _ -> Alcotest.fail "expected Resistor");

  ("resistor: with model", `Quick, fun () ->
      match parse_device "R1 1 2 RMOD 1K\n.END\n" with
      | Ok (Resistor r) ->
        (match r.model with Some m -> Alcotest.(check string) __LOC__ "RMOD" m | None -> Alcotest.fail "model");
        (match r.value with Some (ENum n) -> check_float __LOC__ 1000.0 n | _ -> Alcotest.fail "value")
      | _ -> Alcotest.fail "expected Resistor");

  ("capacitor: named nodes", `Quick, fun () ->
      match parse_device "C1 a b 1n\n.END\n" with
      | Ok (Capacitor c) ->
        Alcotest.(check string) __LOC__ "a" c.pos_node;
        Alcotest.(check string) __LOC__ "b" c.neg_node;
        (match c.value with Some (ENum n) -> check_float __LOC__ 1e-9 n | _ -> Alcotest.fail "value")
      | _ -> Alcotest.fail "expected Capacitor");

  ("inductor: basic", `Quick, fun () ->
      match parse_device "L1 1 0 10u\n.END\n" with
      | Ok (Inductor l) ->
        Alcotest.(check string) __LOC__ "1" l.pos_node;
        Alcotest.(check string) __LOC__ "0" l.neg_node;
        (match l.value with Some (ENum n) -> check_float __LOC__ 10e-6 n | _ -> Alcotest.fail "value")
      | _ -> Alcotest.fail "expected Inductor");

  ("resistor: with keyword params", `Quick, fun () ->
      match parse_device "R1 1 2 1K TC1=0.001\n.END\n" with
      | Ok (Resistor r) ->
        Alcotest.(check int) __LOC__ 1 (List.length r.params);
        let (k, v) = List.nth r.params 0 in
        Alcotest.(check string) __LOC__ "TC1" k;
        (match v with ENum n -> check_float __LOC__ 0.001 n | _ -> Alcotest.fail "TC1 value")
      | _ -> Alcotest.fail "expected Resistor");
]

(* --- Diode --- *)

let diode_tests = [
  ("diode: basic", `Quick, fun () ->
      match parse_device "D1 1 0 DMOD\n.END\n" with
      | Ok (Diode d) ->
        Alcotest.(check string) __LOC__ "1" d.pos_node;
        Alcotest.(check string) __LOC__ "0" d.neg_node;
        Alcotest.(check string) __LOC__ "DMOD" d.model
      | _ -> Alcotest.fail "expected Diode");

  ("diode: with area", `Quick, fun () ->
      match parse_device "D1 1 0 DMOD 1.5\n.END\n" with
      | Ok (Diode d) ->
        Alcotest.(check string) __LOC__ "DMOD" d.model;
        (match d.area with Some (ENum n) -> check_float __LOC__ 1.5 n | _ -> Alcotest.fail "area")
      | _ -> Alcotest.fail "expected Diode");

  ("diode: missing model", `Quick, fun () ->
      match parse_device "D1 1 0\n.END\n" with
      | Error e ->
        (match e.kind with Missing_model -> () | _ -> Alcotest.fail "expected Missing_model")
      | _ -> Alcotest.fail "expected error");
]

(* --- BJT --- *)

let bjt_tests = [
  ("BJT: 3 nodes + model", `Quick, fun () ->
      match parse_device "Q1 1 2 0 QN\n.END\n" with
      | Ok (Bjt b) ->
        Alcotest.(check string) __LOC__ "1" b.collector;
        Alcotest.(check string) __LOC__ "2" b.base;
        Alcotest.(check string) __LOC__ "0" b.emitter;
        Alcotest.(check string) __LOC__ "QN" b.model;
        Alcotest.(check bool) __LOC__ true (b.substrate = None)
      | _ -> Alcotest.fail "expected Bjt");

  ("BJT: 4 nodes + model (with substrate)", `Quick, fun () ->
      match parse_device "Q1 1 2 0 3 QN\n.END\n" with
      | Ok (Bjt b) ->
        (match b.substrate with Some s -> Alcotest.(check string) __LOC__ "3" s | None -> Alcotest.fail "substrate");
        Alcotest.(check string) __LOC__ "QN" b.model
      | _ -> Alcotest.fail "expected Bjt");

  ("BJT: missing model", `Quick, fun () ->
      match parse_device "Q1 1 2 0\n.END\n" with
      | Error e ->
        (match e.kind with Missing_model -> () | _ -> Alcotest.fail "expected Missing_model")
      | _ -> Alcotest.fail "expected error");
]

(* --- JFET / MESFET --- *)

let fet_tests = [
  ("JFET: basic", `Quick, fun () ->
      match parse_device "J1 1 2 0 JN\n.END\n" with
      | Ok (Jfet j) ->
        Alcotest.(check string) __LOC__ "1" j.drain;
        Alcotest.(check string) __LOC__ "2" j.gate;
        Alcotest.(check string) __LOC__ "0" j.source;
        Alcotest.(check string) __LOC__ "JN" j.model
      | _ -> Alcotest.fail "expected Jfet");

  ("MESFET: basic", `Quick, fun () ->
      match parse_device "Z1 1 2 0 ZMOD\n.END\n" with
      | Ok (Mesfet z) ->
        Alcotest.(check string) __LOC__ "ZMOD" z.model
      | _ -> Alcotest.fail "expected Mesfet");
]

(* --- MOSFET --- *)

let mosfet_tests = [
  ("MOSFET: basic with keyword params", `Quick, fun () ->
      match parse_device "M1 d g s b nmos L=1u W=10u\n.END\n" with
      | Ok (Mosfet m) ->
        Alcotest.(check string) __LOC__ "d" m.drain;
        Alcotest.(check string) __LOC__ "g" m.gate;
        Alcotest.(check string) __LOC__ "s" m.source;
        Alcotest.(check string) __LOC__ "b" m.bulk;
        Alcotest.(check string) __LOC__ "nmos" m.model;
        Alcotest.(check int) __LOC__ 2 (List.length m.params);
        let (k1, v1) = List.nth m.params 0 in
        let (k2, v2) = List.nth m.params 1 in
        Alcotest.(check string) __LOC__ "L" k1;
        Alcotest.(check string) __LOC__ "W" k2;
        (match v1 with ENum n -> check_float __LOC__ 1e-6 n | _ -> Alcotest.fail "L");
        (match v2 with ENum n -> check_float __LOC__ 10e-6 n | _ -> Alcotest.fail "W")
      | _ -> Alcotest.fail "expected Mosfet");

  ("MOSFET: missing model", `Quick, fun () ->
      match parse_device "M1 d g s b\n.END\n" with
      | Error e ->
        (match e.kind with Missing_model -> () | _ -> Alcotest.fail "expected Missing_model")
      | _ -> Alcotest.fail "expected error");
]

(* --- Sources: V, I --- *)

let source_tests = [
  ("voltage source: DC", `Quick, fun () ->
      match parse_device "V1 1 0 DC 5\n.END\n" with
      | Ok (Voltage_source s) ->
        Alcotest.(check string) __LOC__ "1" s.pos_node;
        Alcotest.(check string) __LOC__ "0" s.neg_node;
        (match s.dc with Some (ENum n) -> check_float __LOC__ 5.0 n | _ -> Alcotest.fail "dc")
      | _ -> Alcotest.fail "expected Voltage_source");

  ("voltage source: bare value", `Quick, fun () ->
      match parse_device "V1 in 0 5\n.END\n" with
      | Ok (Voltage_source s) ->
        (match s.dc with Some (ENum n) -> check_float __LOC__ 5.0 n | _ -> Alcotest.fail "dc")
      | _ -> Alcotest.fail "expected Voltage_source");

  ("voltage source: PULSE", `Quick, fun () ->
      match parse_device "V1 in 0 PULSE(0 5 0 1n 1n 5n 10n)\n.END\n" with
      | Ok (Voltage_source s) ->
        (match s.transient with
         | Some ("PULSE", args) -> Alcotest.(check int) __LOC__ 7 (List.length args)
         | _ -> Alcotest.fail "transient")
      | _ -> Alcotest.fail "expected Voltage_source");

  ("current source: DC", `Quick, fun () ->
      match parse_device "I1 1 0 DC 1m\n.END\n" with
      | Ok (Current_source s) ->
        (match s.dc with Some (ENum n) -> check_float __LOC__ 1e-3 n | _ -> Alcotest.fail "dc")
      | _ -> Alcotest.fail "expected Current_source");
]

(* --- Controlled sources: E, F, G, H --- *)

let controlled_tests = [
  ("VCVS (E)", `Quick, fun () ->
      match parse_device "E1 1 2 3 4 10\n.END\n" with
      | Ok (Vcvs v) ->
        Alcotest.(check string) __LOC__ "1" v.out_pos;
        Alcotest.(check string) __LOC__ "2" v.out_neg;
        Alcotest.(check string) __LOC__ "3" v.in_pos;
        Alcotest.(check string) __LOC__ "4" v.in_neg;
        (match v.gain with Some (ENum n) -> check_float __LOC__ 10.0 n | _ -> Alcotest.fail "gain")
      | _ -> Alcotest.fail "expected Vcvs");

  ("VCCS (G)", `Quick, fun () ->
      match parse_device "G1 1 2 3 4 5\n.END\n" with
      | Ok (Vccs v) ->
        Alcotest.(check string) __LOC__ "1" v.out_pos;
        (match v.gain with Some (ENum n) -> check_float __LOC__ 5.0 n | _ -> Alcotest.fail "gain")
      | _ -> Alcotest.fail "expected Vccs");

  ("CCCS (F)", `Quick, fun () ->
      match parse_device "F1 1 2 V1 10\n.END\n" with
      | Ok (Cccs c) ->
        Alcotest.(check string) __LOC__ "1" c.pos_node;
        Alcotest.(check string) __LOC__ "2" c.neg_node;
        Alcotest.(check string) __LOC__ "V1" c.controlling_source;
        (match c.gain with Some (ENum n) -> check_float __LOC__ 10.0 n | _ -> Alcotest.fail "gain")
      | _ -> Alcotest.fail "expected Cccs");

  ("CCVS (H)", `Quick, fun () ->
      match parse_device "H1 1 2 V1 5\n.END\n" with
      | Ok (Ccvs c) ->
        Alcotest.(check string) __LOC__ "V1" c.controlling_source;
        (match c.gain with Some (ENum n) -> check_float __LOC__ 5.0 n | _ -> Alcotest.fail "gain")
      | _ -> Alcotest.fail "expected Ccvs");
]

(* --- B-source --- *)

let bsource_tests = [
  ("B-source: V=expr", `Quick, fun () ->
      match parse_device "B1 1 0 V=1+2*3\n.END\n" with
      | Ok (Bsource b) ->
        Alcotest.(check string) __LOC__ "1" b.pos_node;
        Alcotest.(check string) __LOC__ "0" b.neg_node;
        (match b.v_expr with
         | Some (EBinop (Add, ENum 1.0, EBinop (Mul, ENum 2.0, ENum 3.0))) -> ()
         | _ -> Alcotest.fail "v_expr")
      | _ -> Alcotest.fail "expected Bsource");

  ("B-source: I=expr", `Quick, fun () ->
      match parse_device "B1 1 0 I=V(1)*2\n.END\n" with
      | Ok (Bsource b) ->
        (match b.i_expr with Some _ -> () | None -> Alcotest.fail "i_expr")
      | _ -> Alcotest.fail "expected Bsource");
]

(* --- Switches: S, W --- *)

let switch_tests = [
  ("V-switch (S)", `Quick, fun () ->
      match parse_device "S1 1 2 3 4 SWMOD\n.END\n" with
      | Ok (Vswitch s) ->
        Alcotest.(check string) __LOC__ "1" s.pos_node;
        Alcotest.(check string) __LOC__ "2" s.neg_node;
        Alcotest.(check string) __LOC__ "3" s.ctrl_pos;
        Alcotest.(check string) __LOC__ "4" s.ctrl_neg;
        Alcotest.(check string) __LOC__ "SWMOD" s.model
      | _ -> Alcotest.fail "expected Vswitch");

  ("I-switch (W)", `Quick, fun () ->
      match parse_device "W1 1 2 V1 IWMOD\n.END\n" with
      | Ok (Iswitch w) ->
        Alcotest.(check string) __LOC__ "1" w.pos_node;
        Alcotest.(check string) __LOC__ "2" w.neg_node;
        Alcotest.(check string) __LOC__ "V1" w.controlling_source;
        Alcotest.(check string) __LOC__ "IWMOD" w.model
      | _ -> Alcotest.fail "expected Iswitch");
]

(* --- Transmission line --- *)

let tline_tests = [
  ("T-line: basic", `Quick, fun () ->
      match parse_device "T1 1 0 2 0 Z0=50 TD=10n\n.END\n" with
      | Ok (Tline t) ->
        Alcotest.(check string) __LOC__ "1" t.port1_pos;
        Alcotest.(check string) __LOC__ "0" t.port1_neg;
        Alcotest.(check string) __LOC__ "2" t.port2_pos;
        Alcotest.(check string) __LOC__ "0" t.port2_neg;
        Alcotest.(check int) __LOC__ 2 (List.length t.params)
      | _ -> Alcotest.fail "expected Tline");
]

(* --- Mutual inductor --- *)

let mutual_tests = [
  ("mutual inductor: basic", `Quick, fun () ->
      match parse_device "K1 L1 L2 0.9\n.END\n" with
      | Ok (Mutual_inductor m) ->
        Alcotest.(check int) __LOC__ 2 (List.length m.inductors);
        Alcotest.(check string) __LOC__ "L1" (List.nth m.inductors 0);
        Alcotest.(check string) __LOC__ "L2" (List.nth m.inductors 1);
        (match m.coupling with Some (ENum n) -> check_float __LOC__ 0.9 n | _ -> Alcotest.fail "coupling")
      | _ -> Alcotest.fail "expected Mutual_inductor");
]

(* --- Subcircuit instance --- *)

let subckt_tests = [
  ("subckt instance: basic", `Quick, fun () ->
      match parse_device "X1 a b c mysub\n.END\n" with
      | Ok (Subckt_instance s) ->
        Alcotest.(check string) __LOC__ "X1" s.name;
        Alcotest.(check int) __LOC__ 3 (List.length s.nodes);
        Alcotest.(check string) __LOC__ "mysub" s.subckt_name
      | _ -> Alcotest.fail "expected Subckt_instance");

  ("subckt instance: with params", `Quick, fun () ->
      match parse_device "X1 a b mysub R=1K\n.END\n" with
      | Ok (Subckt_instance s) ->
        Alcotest.(check int) __LOC__ 2 (List.length s.nodes);
        Alcotest.(check string) __LOC__ "mysub" s.subckt_name;
        Alcotest.(check int) __LOC__ 1 (List.length s.params)
      | _ -> Alcotest.fail "expected Subckt_instance");
]

(* --- Error cases --- *)

let error_tests = [
  ("resistor: wrong node count", `Quick, fun () ->
      match parse_device "R1 1\n.END\n" with
      | Error e ->
        (match e.kind with Wrong_node_count _ -> () | _ -> Alcotest.fail "expected Wrong_node_count")
      | _ -> Alcotest.fail "expected error");

  ("MOSFET: wrong node count", `Quick, fun () ->
      match parse_device "M1 d g s\n.END\n" with
      | Error e ->
        (match e.kind with Wrong_node_count _ -> () | _ -> Alcotest.fail "expected Wrong_node_count")
      | _ -> Alcotest.fail "expected error");

  ("VCVS: wrong node count", `Quick, fun () ->
      match parse_device "E1 1 2 3\n.END\n" with
      | Error e ->
        (match e.kind with Wrong_node_count _ -> () | _ -> Alcotest.fail "expected Wrong_node_count")
      | _ -> Alcotest.fail "expected error");
]

(* --- Full netlist validation --- *)

let netlist_tests = [
  ("validate full netlist", `Quick, fun () ->
      match parse_and_validate {|
R1 1 2 1K
R2 2 0 2K
V1 1 0 DC 5
.OP
.END
|} with
      | Ok tnl ->
        let devices = List.filter_map (function Typed_device d -> Some d | _ -> None) tnl in
        Alcotest.(check int) __LOC__ 3 (List.length devices);
        (match devices with
         | [Resistor _; Resistor _; Voltage_source _] -> ()
         | _ -> Alcotest.fail "wrong device types")
      | Error _ -> Alcotest.fail "unexpected validation error");

  ("validate collects errors", `Quick, fun () ->
      match parse_and_validate {|
R1 1
D1 1 0
.END
|} with
      | Error errors -> Alcotest.(check int) __LOC__ 2 (List.length errors)
      | Ok _ -> Alcotest.fail "expected validation errors");
]

(* --- Test suite --- *)

let () =
  Alcotest.run "device" [
    "two-terminal passives", passive_tests;
    "diode", diode_tests;
    "bjt", bjt_tests;
    "fet", fet_tests;
    "mosfet", mosfet_tests;
    "sources", source_tests;
    "controlled sources", controlled_tests;
    "b-source", bsource_tests;
    "switches", switch_tests;
    "tline", tline_tests;
    "mutual inductor", mutual_tests;
    "subckt instance", subckt_tests;
    "errors", error_tests;
    "netlist validation", netlist_tests;
  ]

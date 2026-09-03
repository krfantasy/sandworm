(* End-to-end simulator tests against analytic values.
   Covers OP/DC/AC/TRAN, all device families, and error paths. *)

open Sandworm

let op_of_string s =
  let ckt = Circuit.of_string s in
  let sim = Sim.init ckt in
  match Sim.run sim Circuit.Op with
  | Sim.Op op -> op
  | _ -> Alcotest.fail "expected OP result"

let v op n = List.assoc (String.lowercase_ascii n) op.Sim.voltages
let ibr op n = List.assoc (String.uppercase_ascii n) op.Sim.currents

let expect_elab_error s =
  (try
     let _ = Circuit.of_string s in
     Alcotest.fail "expected Elab_error"
   with Circuit.Elab_error _ -> ())

(* --- Linear OP --- *)

let linear_tests =
  [
    ( "resistive divider",
      `Quick,
      fun () ->
        let op = op_of_string "* title\nV1 1 0 DC 5\nR1 1 2 1k\nR2 2 0 2k\n.OP\n.END\n" in
        Alcotest.(check (float 1e-6)) __LOC__ (5.0 *. 2.0 /. 3.0) (v op "2");
        Alcotest.(check (float 1e-9)) __LOC__ (-5.0 /. 3000.0) (ibr op "V1") );
    ( "VCVS",
      `Quick,
      fun () ->
        let op = op_of_string "* title\nV1 1 0 DC 1\nE1 2 0 1 0 10\n.OP\n.END\n" in
        Alcotest.(check (float 1e-9)) __LOC__ 10.0 (v op "2") );
    ( "VCCS",
      `Quick,
      fun () ->
        let op =
          op_of_string "* title\nV1 1 0 DC 1\nR1 1 0 1k\nG1 2 0 1 0 1m\nR2 2 0 1k\n.OP\n.END\n"
        in
        Alcotest.(check (float 1e-9)) __LOC__ (-1.0) (v op "2") );
    ( "CCCS",
      `Quick,
      fun () ->
        let op =
          op_of_string
            "* title\nV1 1 0 DC 1\nR1 1 0 1k\nF1 2 0 V1 5\nR2 2 0 1k\n.OP\n.END\n"
        in
        Alcotest.(check (float 1e-9)) __LOC__ 5.0 (v op "2") );
    ( "CCVS",
      `Quick,
      fun () ->
        let op = op_of_string "* title\nV1 1 0 DC 1\nR1 1 0 1k\nH1 2 0 V1 10\n.OP\n.END\n" in
        Alcotest.(check (float 1e-9)) __LOC__ (-0.01) (v op "2") );
    ( "inductor is a DC short",
      `Quick,
      fun () ->
        let op = op_of_string "* title\nV1 1 0 DC 5\nR1 1 2 1k\nL1 2 0 1m\n.OP\n.END\n" in
        Alcotest.(check (float 1e-9)) __LOC__ 0.0 (v op "2");
        Alcotest.(check (float 1e-9)) __LOC__ 5e-3 (ibr op "L1") );
  ]

(* --- Nonlinear OP --- *)

let nonlinear_tests =
  [
    ( "diode forced 1mA",
      `Quick,
      fun () ->
        let op =
          op_of_string "* title\nI1 0 a DC 1m\nD1 a 0 DMOD\n.MODEL DMOD D(IS=1E-14)\n.OP\n.END\n"
        in
        (* Vt*ln(1e-3/1e-14) = 0.6552 *)
        Alcotest.(check (float 2e-3)) __LOC__ 0.6552 (v op "a") );
    ( "diode forced 1uA",
      `Quick,
      fun () ->
        let op =
          op_of_string "* title\nI1 0 a DC 1u\nD1 a 0 DMOD\n.MODEL DMOD D(IS=1E-14)\n.OP\n.END\n"
        in
        Alcotest.(check (float 2e-3)) __LOC__ 0.4765 (v op "a") );
    ( "BJT forward active",
      `Quick,
      fun () ->
        let op =
          op_of_string
            "* title\nVBE b 0 DC 0.7\nVCE c 0 DC 5\nQ1 c b 0 QN\n.MODEL QN NPN(IS=1E-16 \
             BF=100)\n.OP\n.END\n"
        in
        (* IS*e^(0.7/Vt) = 56.7uA *)
        Alcotest.(check (float 1e-6)) __LOC__ (-56.7e-6) (ibr op "VCE");
        Alcotest.(check (float 1e-8)) __LOC__ (-56.7e-6 /. 100.0) (ibr op "VBE") );
    ( "NMOS saturation",
      `Quick,
      fun () ->
        let op =
          op_of_string
            "* title\nVGS g 0 DC 2\nVDS d 0 DC 5\nM1 d g 0 0 NMOD W=10u L=1u\n.MODEL NMOD \
             NMOS(VTO=1 KP=2E-5 LAMBDA=0.02)\n.OP\n.END\n"
        in
        (* 0.5*2e-5*10*1^2*1.1 = 110uA *)
        Alcotest.(check (float 1e-6)) __LOC__ (-110e-6) (ibr op "VDS") );
    ( "NMOS linear",
      `Quick,
      fun () ->
        let op =
          op_of_string
            "* title\nVGS g 0 DC 2\nVDS d 0 DC 0.5\nM1 d g 0 0 NMOD W=10u L=1u\n.MODEL NMOD \
             NMOS(VTO=1 KP=2E-5 LAMBDA=0.02)\n.OP\n.END\n"
        in
        (* 2e-4*(0.5-0.125)*1.01 = 75.75uA *)
        Alcotest.(check (float 1e-6)) __LOC__ (-75.75e-6) (ibr op "VDS") );
    ( "NMOS cutoff",
      `Quick,
      fun () ->
        let op =
          op_of_string
            "* title\nVGS g 0 DC 0.5\nVDS d 0 DC 5\nM1 d g 0 0 NMOD W=10u L=1u\n.MODEL NMOD \
             NMOS(VTO=1 KP=2E-5 LAMBDA=0.02)\n.OP\n.END\n"
        in
        Alcotest.(check bool) __LOC__ true (Float.abs (ibr op "VDS") < 1e-9) );
    ( "JFET on",
      `Quick,
      fun () ->
        let op =
          op_of_string
            "* title\nVGS g 0 DC 0\nVDS d 0 DC 5\nJ1 d g 0 JM\n.MODEL JM NJF(VTO=-2 \
             BETA=1E-4)\n.OP\n.END\n"
        in
        (* 1e-4*2^2 = 400uA *)
        Alcotest.(check (float 1e-5)) __LOC__ (-400e-6) (ibr op "VDS") );
    ( "PMOS on (polarity)",
      `Quick,
      fun () ->
        let op =
          op_of_string
            "* title\nVDD s 0 DC 5\nVG g 0 DC 3\nM1 d g s s PMOD W=10u L=1u\nRD d 0 4.7k\n.MODEL \
             PMOD PMOS(VTO=-1 KP=2E-5)\n.OP\n.END\n"
        in
        (* Vsg=2: Id ~ 0.5*2e-4*1 = 100uA -> Vd ~ 0.47V *)
        let vd = v op "d" in
        Alcotest.(check bool) __LOC__ true (vd > 0.3 && vd < 0.7) );
    ( "PNP on (polarity)",
      `Quick,
      fun () ->
        let op =
          op_of_string
            "* title\nVEE e 0 DC 5\nVB b 0 DC 4.3\nQ1 c b e QP\nRC c 0 1k\n.MODEL QP \
             PNP(IS=1E-16 BF=100)\n.OP\n.END\n"
        in
        (* Veb=0.7: Ic ~ 56uA -> Vc ~ 0.056V *)
        let vc = v op "c" in
        Alcotest.(check bool) __LOC__ true (vc > 0.03 && vc < 0.1) );
    ( "BJT saturation clamps Vce",
      `Quick,
      fun () ->
        let op =
          op_of_string
            "* title\nVCC c 0 DC 5\nVIN b 0 DC 5\nRC c c1 1k\nRB b b1 10k\nQ1 c1 b1 0 \
             QN\n.MODEL QN NPN(IS=1E-16 BF=100)\n.OP\n.END\n"
        in
        let vc1 = v op "c1" in
        Alcotest.(check bool) __LOC__ true (vc1 > 0.0 && vc1 < 0.5) );
  ]

(* --- DC sweep --- *)

let dc_tests =
  [
    ( "divider sweep tracks ratio",
      `Quick,
      fun () ->
        let ckt =
          Circuit.of_string "* title\nV1 1 0 DC 5\nR1 1 2 1k\nR2 2 0 2k\n.DC V1 0 5 1\n.END\n"
        in
        let sim = Sim.init ckt in
        (match Sim.run sim (List.find (function Circuit.Dc _ -> true | _ -> false) ckt.Circuit.analyses) with
        | Sim.Dc dc ->
            Alcotest.(check int) __LOC__ 6 (List.length dc.Sim.points);
            List.iter
              (fun (combo, op) ->
                let vsrc = List.hd combo in
                Alcotest.(check (float 1e-6)) __LOC__ (vsrc *. 2.0 /. 3.0) (v op "2"))
              dc.Sim.points
        | _ -> Alcotest.fail "expected DC") );
    ( "BJT switch saturates across sweep",
      `Quick,
      fun () ->
        let ckt =
          Circuit.of_string
            "* title\nVCC c 0 DC 5\nVIN b 0 DC 0\nRC c c1 1k\nRB b b1 10k\nQ1 c1 b1 0 \
             QN\n.MODEL QN NPN(IS=1E-16 BF=100)\n.DC VIN 0 5 1\n.END\n"
        in
        let sim = Sim.init ckt in
        (match Sim.run sim (List.find (function Circuit.Dc _ -> true | _ -> false) ckt.Circuit.analyses) with
        | Sim.Dc dc ->
            let last = List.nth dc.Sim.points 5 in
            let vc1 = v (snd last) "c1" in
            Alcotest.(check bool) __LOC__ true (vc1 > 0.0 && vc1 < 0.5)
        | _ -> Alcotest.fail "expected DC") );
  ]

(* --- AC --- *)

let ac_tests =
  let run_ac_f0 netlist f0 =
    let ckt = Circuit.of_string netlist in
    let sim = Sim.init ckt in
    Sim.run_ac sim ~sweep:Ast.Lin ~npts:2.0 ~fstart:f0 ~fstop:f0
  in
  [
    ( "RC lowpass corner",
      `Quick,
      fun () ->
        let f0 = 1.0 /. (2.0 *. Float.pi *. 1e3 *. 1e-6) in
        let ac =
          run_ac_f0 "* title\nV1 1 0 DC 0 AC 1\nR1 1 2 1k\nC1 2 0 1u\n.END\n" f0
        in
        let v2 = List.assoc "2" ac.Sim.v_nodes |> fun a -> a.(0) in
        Alcotest.(check (float 1e-3)) __LOC__ (1.0 /. Float.sqrt 2.0) (Complex.norm v2);
        Alcotest.(check (float 0.5)) __LOC__ (-45.0)
          (Complex.arg v2 *. 180.0 /. Float.pi) );
    ( "inductor impedance",
      `Quick,
      fun () ->
        let f0 = 1.0 /. (2.0 *. Float.pi *. 1e-3) in
        (* wL = 1 ohm in series with 1 ohm -> |I| = 1/sqrt(2) *)
        let ac = run_ac_f0 "* title\nV1 1 0 DC 0 AC 1\nR1 1 2 1\nL1 2 0 1m\n.END\n" f0 in
        let i = List.assoc "V1" ac.Sim.i_branches |> fun a -> a.(0) in
        Alcotest.(check (float 1e-3)) __LOC__ (1.0 /. Float.sqrt 2.0) (Complex.norm i) );
    ( "coupled inductors aid",
      `Quick,
      fun () ->
        let f0 = 1.0 /. (2.0 *. Float.pi *. 1e-3) in
        (* L1+L2+2M = 3mH in series with 1 ohm -> |I| = 1/sqrt(10) *)
        let ac =
          run_ac_f0 "* title\nV1 1 0 DC 0 AC 1\nR1 1 2 1\nL1 2 3 1m\nL2 3 0 1m\nK1 L1 L2 0.5\n.END\n" f0
        in
        let i = List.assoc "V1" ac.Sim.i_branches |> fun a -> a.(0) in
        Alcotest.(check (float 1e-3)) __LOC__ (1.0 /. Float.sqrt 10.0) (Complex.norm i) );
  ]

(* --- Transient --- *)

let tran_of_string s =
  let ckt = Circuit.of_string s in
  let sim = Sim.init ckt in
  match Sim.run sim (List.find (function Circuit.Tran _ -> true | _ -> false) ckt.Circuit.analyses) with
  | Sim.Tran tr -> tr
  | _ -> Alcotest.fail "expected TRAN"

let tran_tests =
  [
    ( "RC charge UIC",
      `Quick,
      fun () ->
        let tr =
          tran_of_string "* title\nV1 1 0 DC 5\nR1 1 2 1k\nC1 2 0 1u\n.TRAN 0.1m 5m UIC\n.END\n"
        in
        let at t =
          let k = ref 0 in
          Array.iteri (fun i tt -> if Float.abs (tt -. t) < 1e-12 then k := i) tr.Sim.times;
          (List.assoc "2" tr.Sim.v_nodes).(!k)
        in
        (* 5*(1-e^-1) = 3.1606 *)
        Alcotest.(check (float 0.05)) __LOC__ 3.1606 (at 1e-3);
        (* 5*(1-e^-5) = 4.9663 *)
        Alcotest.(check (float 0.05)) __LOC__ 4.9663 (at 5e-3) );
    ( "no-UIC starts from OP",
      `Quick,
      fun () ->
        let tr = tran_of_string "* title\nV1 1 0 DC 5\nR1 1 2 1k\nC1 2 0 1u\n.TRAN 0.1m 1m\n.END\n" in
        Alcotest.(check (float 1e-6)) __LOC__ 5.0 (List.assoc "2" tr.Sim.v_nodes |> fun a -> a.(0)) );
    ( "SIN peaks",
      `Quick,
      fun () ->
        let tr =
          tran_of_string "* title\nV1 1 0 SIN(0 5 1k)\nR1 1 0 1k\n.TRAN 10u 1m UIC\n.END\n"
        in
        let vmax = Array.fold_left Float.max Float.neg_infinity (List.assoc "1" tr.Sim.v_nodes) in
        Alcotest.(check (float 0.05)) __LOC__ 5.0 vmax );
    ( "PULSE level",
      `Quick,
      fun () ->
        let tr =
          tran_of_string
            "* title\nV1 1 0 PULSE(0 5 0 1n 1n 1m 2m)\nR1 1 0 1k\n.TRAN 10u 2m UIC\n.END\n"
        in
        let v1 = List.assoc "1" tr.Sim.v_nodes in
        let k = ref 0 in
        Array.iteri (fun i tt -> if Float.abs (tt -. 0.5e-3) < 1e-12 then k := i) tr.Sim.times;
        Alcotest.(check (float 1e-9)) __LOC__ 5.0 v1.(!k) );
    ( "PWL ramp",
      `Quick,
      fun () ->
        let tr =
          tran_of_string "* title\nV1 1 0 PWL(0 0 1m 5)\nR1 1 0 1k\n.TRAN 10u 1m UIC\n.END\n"
        in
        let v1 = List.assoc "1" tr.Sim.v_nodes in
        let k = ref 0 in
        Array.iteri (fun i tt -> if Float.abs (tt -. 0.5e-3) < 1e-12 then k := i) tr.Sim.times;
        Alcotest.(check (float 1e-9)) __LOC__ 2.5 v1.(!k) );
    ( "EXP charge",
      `Quick,
      fun () ->
        let tr =
          tran_of_string
            "* title\nV1 1 0 EXP(0 5 0 1m 2m 1m)\nR1 1 0 1k\n.TRAN 10u 1m UIC\n.END\n"
        in
        let v1 = List.assoc "1" tr.Sim.v_nodes in
        let k = ref 0 in
        Array.iteri (fun i tt -> if Float.abs (tt -. 1e-3) < 1e-12 then k := i) tr.Sim.times;
        Alcotest.(check (float 0.02)) __LOC__ 3.1606 v1.(!k) );
  ]

(* --- Behavioral / structural --- *)

let misc_tests =
  [
    ( "Bv doubles",
      `Quick,
      fun () ->
        let op = op_of_string "* title\nV1 1 0 DC 1\nB1 2 0 V=V(1)*2\nR2 2 0 1k\n.OP\n.END\n" in
        Alcotest.(check (float 1e-6)) __LOC__ 2.0 (v op "2") );
    ( "Bi with param gain",
      `Quick,
      fun () ->
        let op =
          op_of_string
            "* title\nI1 0 1 DC 1m\nR1 1 0 1k\nB1 2 0 V=V(1)*GAIN\nR2 2 0 1k\n.PARAM GAIN=2\n.OP\n.END\n"
        in
        Alcotest.(check (float 1e-6)) __LOC__ 2.0 (v op "2") );
    ( "Bi current source",
      `Quick,
      fun () ->
        let op = op_of_string "* title\nV1 1 0 DC 1\nB1 2 0 I=V(1)*1m\nR2 2 0 1k\n.OP\n.END\n" in
        Alcotest.(check (float 1e-6)) __LOC__ 1.0 (v op "2") );
    ( "subcircuit divider",
      `Quick,
      fun () ->
        let op =
          op_of_string
            "* title\nV1 1 0 DC 5\nX1 1 2 0 DIV2\n.SUBCKT DIV2 A B C\nR1 A B 1k\nR2 B C 2k\n.ENDS\n.OP\n.END\n"
        in
        Alcotest.(check (float 1e-6)) __LOC__ (5.0 *. 2.0 /. 3.0) (v op "2") );
    ( "param resistor value",
      `Quick,
      fun () ->
        let op =
          op_of_string "* title\nV1 1 0 DC 5\nR1 1 2 1k\nR2 2 0 RVAL\n.PARAM RVAL=2k\n.OP\n.END\n"
        in
        Alcotest.(check (float 1e-6)) __LOC__ (5.0 *. 2.0 /. 3.0) (v op "2") );
    ( "switch turns on",
      `Quick,
      fun () ->
        let ckt =
          Circuit.of_string
            "* title\nV1 1 0 DC 5\nS1 1 2 3 0 SWMOD\nR1 2 0 1k\nVCTRL 3 0 DC 0\n.MODEL SWMOD \
             SW(VT=2.5 RON=1 ROFF=1E9)\n.DC VCTRL 0 5 5\n.END\n"
        in
        let sim = Sim.init ckt in
        (match Sim.run sim (List.find (function Circuit.Dc _ -> true | _ -> false) ckt.Circuit.analyses) with
        | Sim.Dc dc ->
            let v2 vo = v (snd vo) "2" in
            Alcotest.(check bool) __LOC__ true (v2 (List.nth dc.Sim.points 0) < 0.01);
            Alcotest.(check (float 0.05)) __LOC__ (5.0 *. 1000.0 /. 1001.0)
              (v2 (List.nth dc.Sim.points 1))
        | _ -> Alcotest.fail "expected DC") );
    ( "floating nodes rest at zero (GMIN)",
      `Quick,
      fun () ->
        let op = op_of_string "* title\nR1 1 2 1k\n.OP\n.END\n" in
        Alcotest.(check bool) __LOC__ true
          (Float.abs (v op "1") < 1e-6 && Float.abs (v op "2") < 1e-6) );
    ( "TLINE unsupported",
      `Quick,
      fun () -> expect_elab_error "* title\nV1 1 0 DC 1\nT1 1 0 2 0 Z0=50 TD=10n\nR2 2 0 50\n.OP\n.END\n" );
    ( "missing model",
      `Quick,
      fun () -> expect_elab_error "* title\nD1 1 0 DMOD\n.OP\n.END\n" );
    ( "K without coupling",
      `Quick,
      fun () ->
        expect_elab_error "* title\nV1 1 0 DC 1\nL1 1 0 1m\nL2 1 0 1m\nK1 L1 L2\n.OP\n.END\n" );
  ]

let () =
  Alcotest.run "sim"
    [
      ("linear OP", linear_tests);
      ("nonlinear OP", nonlinear_tests);
      ("DC sweep", dc_tests);
      ("AC", ac_tests);
      ("transient", tran_tests);
      ("misc", misc_tests);
    ]

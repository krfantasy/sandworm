(* sandworm CLI: run all analyses in a SPICE netlist and print results. *)

open Sandworm

let () =
  let file = ref None in
  let dump = ref false in
  Arg.parse
    [ ("--dump", Arg.Set dump, "print elaborated circuit and exit") ]
    (fun s -> file := Some s)
    "sandworm [--dump] <netlist.cir>";
  match !file with
  | None ->
      Printf.eprintf "usage: %s <netlist.cir>\n" Sys.argv.(0);
      exit 2
  | Some f -> (
      try
        let ckt = Circuit.of_file f in
        if !dump then print_string (Circuit.dump ckt)
        else
          let s = Sim.init ckt in
          List.iter
          (fun a ->
            let r = Sim.run s a in
            print_string (Sim.string_of_result ckt.Circuit.title r);
            print_newline ())
          ckt.Circuit.analyses
      with
      | Circuit.Elab_error msg ->
          Printf.eprintf "error: %s\n" msg;
          exit 1
      | Sim.Sim_error msg ->
          Printf.eprintf "simulation error: %s\n" msg;
          exit 1
      | Sim.No_convergence msg ->
          Printf.eprintf "no convergence: %s\n" msg;
          exit 1
      | Netlist.Parse_error msg ->
          Printf.eprintf "parse error: %s\n" msg;
          exit 1)

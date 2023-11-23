open Pog2why

let version_option = "-v"
let version () =
  let text =
    "Copyright (C) 2022  Claude Stolze, INRIA\nLicense GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\nThis is free software: you are free to change and redistribute it.\nThere is NO WARRANTY, to the extent permitted by law.\n"
  in
  print_string text; exit 0
let version_doc = " output version information and exit"

let prover_option = "-p"
let prover = ref ""
let prover_doc = " select prover"

let proveme_option = "-P"
let proveme = ref false
let proveme_fun () = proveme := true
let proveme_doc = " prove the obligation"

let input_option = "-i"
let input = ref ""
let input_doc = " input filename"

let output_option = "-o"
let output = ref ""
let output_doc = " output filename"

let goal_option = "-a"
let arg_1 = ref 0
let arg_2 = ref 0
let goal_list = ref []
let goal_add () = goal_list := (!arg_1,!arg_2) :: !goal_list
let goal_doc = "M N  select the N-th Simple_Goal child element from the M-th Proof_Obligation element for translation"

let all_goal_option = "-A"
let all_goal = ref false
let all_goal_doc = " translate all goals"

let options =
  [
    version_option, Arg.Unit version, version_doc;
    goal_option, Arg.(Tuple [Set_int arg_1; Set_int arg_2; Unit goal_add]), goal_doc;
    all_goal_option, Arg.Set all_goal, all_goal_doc;
    prover_option, Arg.Set_string prover, prover_doc;
    proveme_option, Arg.Unit proveme_fun, proveme_doc;
    input_option, Arg.Set_string input, input_doc;
    output_option, Arg.Set_string output, output_doc
  ]

let usage = "Usage: " ^ Sys.argv.(0) ^ " [-v -A] -p prover -a M1 N1 ... -a Mk Nk -i INPUT -o OUTPUT"

let () =
  begin
    Arg.parse options ignore usage;
    goal_list := List.rev !goal_list;
    if !input = "" || !output = "" then
      begin
        print_endline usage;
        exit 1
      end
  end

let pog = !input |> file_to_tree

let s =
  let choice = if !all_goal then None else Some !goal_list in
  parse_pog choice pog

open Why3
open Format

let out = formatter_of_out_channel (open_out !output)

(* let () = Why3.Debug.set_flag (Why3.Debug.lookup_flag "interm_task") *)

let () =
  if !prover = "" then
    printme out
  else
    let config = Whyconf.init_config None in
    let main : Whyconf.main = Whyconf.get_main config in
    (* let provers : Whyconf.config_prover Whyconf.Mprover.t =
      Whyconf.get_provers config in *)
    let prov : Whyconf.config_prover =
      let fp = Whyconf.parse_filter_prover !prover in
      let provers = Whyconf.filter_provers config fp in
      if Whyconf.Mprover.is_empty provers then begin
          eprintf "Prover %s not installed or not configured@." !prover;
          exit 1
        end else
        snd (Whyconf.Mprover.max_binding provers)
    in
    (* builds the environment from the [loadpath] *)
    let env : Env.env = Env.create_env (Whyconf.loadpath main) in
    let driver : Driver.driver =
      try
        (* Driver.load_driver_file_and_extras main env "vampire.drv" [] *)
        Driver.load_driver_file_and_extras main env "tptp-tff0.drv" []
        (* Driver.load_driver_for_prover main env prov *)
      with e ->
        eprintf "Failed to load driver for %s: %a@." !prover
          Exn_printer.exn_printer e;
        exit 1
    in
    if !proveme then
      begin
        Seq.iter (fun t ->
            Whyutils.my_task := t;
            (* print_endline "Calling prover"; *)
            ignore @@ result prov driver;
            (* print_endline "Done"; *)
            printf "@[%s answers %a@." !prover
              (Call_provers.print_prover_result ?json:None) (result prov driver)) s
      end
    else
      begin
        let t =
          match Seq.uncons s with
          | Some (t, _) -> t
          | _ -> assert false
        in
        Whyutils.my_task := t;
        print_tptp driver out
      end

open Pogparser

let version_option = "-v"
let version () =
  let text =
    "Copyright (C) 2022  Claude Stolze, INRIA\nLicense GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\nThis is free software: you are free to change and redistribute it.\nThere is NO WARRANTY, to the extent permitted by law.\n"
  in
  print_string text; exit 0
let version_doc = " output version information and exit"

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
    input_option, Arg.Set_string input, input_doc;
    output_option, Arg.Set_string output, output_doc
  ]

let usage = "Usage: " ^ Sys.argv.(0) ^ " [-v] -a M1 N1 ... -a Mk Nk -i INPUT -o OUTPUT"

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

let print_pkg () =
  let out = Out_channel.open_text "lambdapi.pkg" in
  Out_channel.output_string out @@
    "package_name = " ^ package_name ^ "\nroot_path    = " ^ package_name ^ "\n";
  Out_channel.close out

let pog = !input |> file_to_tree

let () =
  if not (Sys.file_exists !output) then
    Sys.mkdir !output @@ 7*8*8 + 5*8 + 5;
  if not (Sys.is_directory !output) then
    begin
      prerr_endline (!output ^ " is not a directory.");
      exit 1
    end;
  Sys.chdir !output;
  print_pkg ();
  parse_pog pog

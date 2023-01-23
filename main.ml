open Pogparser

let version_option = "-v"
let version () =
  let text =
    "Copyright (C) 2022  Claude Stolze, INRIA\nLicense GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\nThis is free software: you are free to change and redistribute it.\nThere is NO WARRANTY, to the extent permitted by law.\n"
  in
  print_string text; exit 0
let version_doc = "output version information and exit\n"

let input_option = "-i"
let input = ref ""
let input_doc = "input filename"

let output_option = "-o"
let output = ref ""
let output_doc = "output filename"

let options =
  [
    version_option, Arg.Unit version, version_doc;
    input_option, Arg.Set_string input, input_doc;
    output_option, Arg.Set_string output, output_doc
  ]

let usage = "Usage: " ^ Sys.argv.(0) ^ " [-v] -i INPUT -o OUTPUT"

let () =
  begin
    Arg.parse options ignore usage;
    if !input = "" || !output = "" then
      begin
        print_endline usage;
        exit 1
      end
  end

let ( >> ) f g x = g (f x)

let pog = !input |> file_to_tree |> parse_pog

let print_pkg () =
  let out = Out_channel.open_text "lambdapi.pkg" in
  Out_channel.output_string out
    "package_name = Foo\nroot_path    = Foo\n";
  Out_channel.close out

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
  Queue.iter (parse_po pog) pog.obligations

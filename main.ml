open Markup
open Pogparser
open Checker

let ( >> ) f g x = g (f x)

let input = ref ""
let arg_input arg = input := arg
let output = ref ""

let usage = "Usage: " ^ Sys.argv.(0) ^ " [-v] INPUT OUTPUT\n"
let version_option = "-v"
let version_text =
  "Copyright (C) 2022  Claude Stolze, INRIA\nLicense GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\nThis is free software: you are free to change and redistribute it.\nThere is NO WARRANTY, to the extent permitted by law.\n"
let version_doc = "output version information and exit\n"

let output_option = "-o"
let output_doc = "output file name"

let version () = print_endline version_text; exit 0
let options = [version_option, Arg.Unit (version), version_doc;
               output_option, Arg.Set_string output, output_doc]

let () = Arg.parse options arg_input usage
let () = if !input = "" || !output = "" then begin print_endline usage; exit 1 end

let filename = !input

let (input, close) = file filename

let stream = input |> parse_xml |> signals |> trim

let test = stream |> tree
                       ~text:(fun l -> Text(String.trim @@ String.concat "" l))
                       ~element:(fun (_, name) l children -> Element (name, List.map (fun ((_,x),y) -> (x,y)) l, children)) |>
             function
             | Some x -> x
             | None -> failwith "Not an XML file"

let () = close ()

let ast = parse_proof_Obligations test
let def = pos2ns ast

let filename = !output
let out = Out_channel.open_text filename

let () =
  let print = Printer.print_ns >> Out_channel.output_string out |> List.iter in
  Out_channel.output_string out "require open B.syntax;\n\n";
  print def

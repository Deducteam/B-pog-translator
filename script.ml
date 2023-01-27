(* do not execute this script if you do not know what you are doing *)

#load "unix.cma";;

let command = Sys.command
(* let command x = print_endline x; 0 *)


let rec seq a b = if a = b then [b] else a :: (seq (a+1) b)

let dir n = if n < 10 then "000" ^ (string_of_int n) else "00" ^ (string_of_int n)

let source n = (Unix.getenv "HOME") ^ "/dataset-pog/" ^ dir n ^ "/"
let dest n = (Unix.getenv "HOME") ^ "/pog-translated/" ^ dir n ^ "/"

let poglist n = Sys.readdir (source n) |> Array.to_list |> List.filter @@ String.ends_with ~suffix:".pog" |> List.sort String.compare

let to_com n f = "./main.native -i " ^ source n ^ f ^ " -o " ^ dest n ^ (String.sub f 0 (String.length f - 4)) (* ^ ".lp" *)

let handle =
  function
  | 255 (* Ctrl+C (?) *)-> exit 130
  | 0 -> ()
  | x -> print_int x; print_newline (); exit x

let command n = List.iter (fun x -> print_int n; print_string "/"; print_endline x; handle (command (to_com n x))) (poglist n)

let () = List.iter command (seq 0 35)

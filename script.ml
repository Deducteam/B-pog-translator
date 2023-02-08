(* do not execute this script if you do not know what you are doing *)

(* TODO problem with 0009/00230 *)
(* TODO problem with 0015/00310 *)
(* TODO problem with 0015/00312 *)
(* TODO problem with 0015/00314 *)
(* TODO problem with 0015/00318 *)
(* TODO problem with 0015/00320 *)
(* TODO problem with 0015/00322 *)
(* TODO problem with 0015/00325 *)
(* TODO problem with 0015/00327 *)
(* TODO problem with 0015/00329 *)
(* TODO problem with 0015/00333 *)
(* TODO problem with 0015/00335 *)
(* TODO problem with 0015/00337 *)
(* TODO problem with 0015/00339 *)
(* TODO problem with 0015/00341 *)
(* TODO problem with 0015/00343 *)
(* TODO problem with 0015/00347 *)
(* TODO problem with 0015/00349 *)
(* TODO problem with 0015/00353 *)
(* TODO problem with 0015/00355 *)
(* TODO problem with 0015/00357 *)
(* TODO problem with 0015/00359 *)
(* TODO problem with 0015/00363 *)
(* TODO problem with 0015/00365 *)
(* TODO problem with 0015/00367 *)
(* TODO problem with 0016/00917 *)
(* TODO problem with 0016/00919 *)
(* TODO problem with 0016/00921 *)
(* TODO problem with 0016/00923 *)
(* TODO problem with 0016/00926 *)
(* TODO problem with 0016/00928 *)
(* TODO problem with 0016/00930 *)
(* TODO problem with 0016/00938 *)
(* TODO problem with 0016/00940 *)
(* TODO problem with 0016/00942 *)
(* TODO problem with 0016/00944 *)
(* TODO problem with 0016/00946 *)
(* TODO problem with 0016/00950 *)
(* TODO problem with 0016/00952 *)
(* TODO problem with 0016/00954 *)
(* TODO problem with 0016/00956 *)
(* TODO problem with 0016/00958 *)
(* TODO problem with 0016/00960 *)
(* TODO problem with 0016/00962 *)
(* TODO problem with 0016/00964 *)
(* TODO problem with 0016/00966 *)
(* TODO problem with 0016/00969 *)

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

let comm n = List.iter (fun x -> print_int n; print_string "/"; print_endline x; handle (command (to_com n x))) (poglist n)

let () = List.iter comm (seq 32 35) (* (seq 0 35) *)

let problems = ["0009/00230"; "0015/00310"; "0015/00312"; "0015/00314"; "0015/00318"; "0015/00320"; "0015/00322"; "0015/00325"; "0015/00327"; "0015/00329"; "0015/00333"; "0015/00335"; "0015/00337"; "0015/00339"; "0015/00341"; "0015/00343"; "0015/00347"; "0015/00349"; "0015/00353"; "0015/00355"; "0015/00357"; "0015/00359"; "0015/00363"; "0015/00365"; "0015/00367"; "0016/00917"; "0016/00919"; "0016/00921"; "0016/00923"; "0016/00926"; "0016/00928"; "0016/00930"; "0016/00938"; "0016/00940"; "0016/00942"; "0016/00944"; "0016/00946"; "0016/00950"; "0016/00952"; "0016/00954"; "0016/00956"; "0016/00958"; "0016/00960"; "0016/00962"; "0016/00964"; "0016/00966"; "0016/00969"]

let dest n = (Unix.getenv "HOME") ^ "/pog-translated/" ^ dir n ^ "/"

let to_com f = "lambdapi check -c " ^ f

let comm n = Sys.readdir (dest n) |> Array.to_list |> List.sort String.compare |> List.iter (fun x -> Sys.readdir (dest n ^ x) |> Array.to_list |> List.filter @@ String.ends_with ~suffix:".lp" |> List.iter (fun y -> handle (command (to_com (dest n ^ x ^ "/" ^ y)))))

let () = List.iter comm (seq 32 35) (* (seq 0 35) *)

let () = ()

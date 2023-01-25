open Syntax

(* Integers *)
(* Divide one decimal digit by 2 *)
let divby2 c n =
  let open Stdlib in
  n mod 2 = 1, (if c then n / 2 + 5 else n / 2)

(* Divide a list of decimal digits by 2 *)
let get_lsb_div l = List.fold_left_map divby2 false l

let rec trim =
  function
  | 0 :: l -> trim l
  | l -> l

(* Convert a list of decimal digits in binary (reversed) *)
let rec dec_2_bin_rev =
  function
  | [] -> []
  | l -> let b, l = get_lsb_div l in
         let l = trim l in
         b :: dec_2_bin_rev l

let char2int =
  function
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | _ -> 0

let rec bin2string =
  function
  | [true] -> xh
  | true :: l -> x1 (bin2string l)
  | false :: l -> x0 (bin2string l)
  | _ -> assert false

let helper s = String.fold_right (fun c l -> char2int c :: l) s []

let int n =
  match String.get n 0 with
  | '0' -> zero
  | '-' -> zneg (bin2string @@ dec_2_bin_rev @@ List.tl @@ helper n)
  | _ -> zpos (bin2string @@ dec_2_bin_rev @@ helper n)

(* TODO: lists *)
let to_list x = List.fold_right cons x (nil ())

let rec my_fold f o =
  function
  | [] -> o
  | [x] -> x
  | x :: l -> f x (my_fold f o l)

let unary_op =
  function
  | "imax" -> imax
  | "rmax" -> rmax
  | "imin" -> imin
  | "rmin" -> rmin
  | "card" -> card
  | "dom" -> dom
  | "ran" -> ran
  | "POW" -> pow
  | "POW1" -> pow1
  | "FIN" -> fin
  | "FIN1" -> fin1
  | "union" -> union_gen
  | "inter" -> inter_gen
  | "seq" -> seq
  | "seq1" -> seq1
  | "iseq" -> iseq
  | "iseq1" -> iseq1
  | "-i" -> minusi
  | "-r" -> minusr
  | "~" -> inversion
  | "size" -> size
  | "perm" -> perm
  | "first" -> first
  | "last" -> last
  | "id" -> identity
  | "closure" -> closure
  | "closure1" -> closure1
  | "tail" -> tail
  | "front" -> front
  | "rev" -> rev
  | "conc" -> conc
  | "succ" -> succ
  | "pred" -> pred
  | "rel" -> rel
  | "fnc" -> fnc
  | "real" -> real
  | "floor" -> floor
  | "ceiling" -> ceiling
  | "tree" -> tree
  | "btree" -> btree
  | "top" -> top
  | "sons" -> sons
  | "prefix" -> pref
  | "postfix" -> post
  | "sizet" -> sizet
  | "mirror" -> mirror
  | "left" -> left_tree
  | "right" -> right_tree
  | "infix" -> infix_tree
  | "bin" -> bin_unary
  | x -> failwith ("Invalid unary expression : " ^ x)

let binary_op =
  function
  | "=>" -> imply
  | "<=>" -> equiv
  | ":" -> belong
  | "/:" -> notbelong
  | "<:" -> included
  | "/<:" -> notincluded
  | "<<:" -> strictincluded
  | "/<<:" -> notstrictincluded
  | "=" -> equal
  | "/=" -> notequal
  (* integer comparison *)
  | ">=i" -> igeq
  | ">i" -> igreater
  | "<i" -> ismaller
  | "<=i" -> ileq
  (* real comparison *)
  | ">=r" -> rgeq
  | ">r" -> rgreater
  | "<r" -> rsmaller
  | "<=r" -> rleq
  (* float comparison *)
  | ">=f" -> fgeq
  | ">f" -> fgreater
  | "<f" -> fsmaller
  | "<=f" -> fleq
  | "," -> pair
  | "*i" -> iprod
  | "*r" -> rprod
  | "*f" -> fprod
  | "*s" -> sprod
  | "**i" -> iexp
  | "**r" -> rexp
  | "+i" -> iplus
  | "+r" -> rplus
  | "+f" -> fplus
  | "+->" -> partial
  | "+->>" -> partialsurj
  | "-i" -> iminus
  | "-r" -> rminus
  | "-f" -> fminus
  | "-s" -> sminus
  | "-->" -> total
  | "-->>" -> totalsurj
  | "->" -> headinsert
  | ".." -> interval
  | "/i" -> idiv
  | "/r" -> rdiv
  | "/f" -> fdiv
  | "/\\" -> inter
  | "/|\\" -> headrestrict
  | ";" -> compose
  | "<+" -> overload
  | "<->" -> relation
  | "<-" -> tailinsert
  | "<<|" -> domsubtract
  | "<|" -> domrestrict
  | ">+>" -> partialinject
  | ">->" -> totalinject
  | ">->>" -> totalbiject
  | "><" -> directprod
  | "||" -> parallelprod
  | "\\/" -> union
  | "\\|/" -> tailrestrict
  | "^" -> concat
  | "mod" -> ( mod )
  | "|->" -> map
  | "|>" -> imagerestrict
  | "|>>" -> imagesubtract
  | "[" -> image (* image of a relation *)
  | "(" -> eval (* image of a function *)
  | "prj1" -> prj1
  | "prj2" -> prj2
  | "iterate" -> iterate
  | "const" -> const
  | "rank" -> rank
  | "father" -> father
  | "subtree" -> subtree
  | "arity" -> arity
  | x -> failwith ("Invalid binary expression : " ^ x)

let ternary_op =
  function
  | "son" -> son
  | "bin" -> bin_ternary
  | x -> failwith ("Invalid ternary expression : " ^ x)

let nary_op =
  function
  | "[" -> fun x -> sequence (to_list x)
  | "{" -> fun x -> extension (to_list x)
  | "&" -> my_fold conj trueu
  | "or" -> my_fold disj falseu
  | x -> failwith ("Invalid n-ary expression : " ^ x)

let quantified_pred_op s args body =
  match s with
  | "!" -> List.fold_left (fun x (i,t) -> forall i (tau t) x) body args
  | "#" -> List.fold_left (fun x (i,t) -> exists i (tau t) x) body args
  | x -> failwith ("Invalid predicate quantifier : " ^ x)

let rec helper =
  function
  | [] -> assert false
  | [(_,y)] -> typnil y
  | (_,y) :: l -> typcons y (helper l)

let quantified_exp_op s args b1 b2 =
  let helper = helper args in
  let args = List.map (fun (x,y) -> x, Some (tau y)) args in
  let b1 = Lp.Binder(Lp.Uid "λ", args, b1) in
  let b2 = Lp.Binder(Lp.Uid "λ", args, b2) in
  begin
    match s with
    | "%" -> lambda helper
    | "iSIGMA" -> isigma helper
    | "rSIGMA" -> rsigma helper
    | "iPI" -> ipi helper
    | "rPI" -> rpi helper
    | "INTER" -> inter' helper
    | "UNION" -> union' helper
    | x -> failwith ("Invalid expression quantifier : " ^ x)
  end b1 b2

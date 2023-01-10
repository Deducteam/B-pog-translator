open Pogparser
open Checker

type dec_bigint = int list
type bin_bigint = bool list

let divby2 c n = n mod 2 = 1, (if c then n / 2 + 5 else n / 2)
let get_lsb_div l = List.fold_left_map divby2 false l
let rec trim =
  function
  | 0 :: l -> trim l
  | l -> l

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
    | [true] -> "xH"
    | true :: l -> "x1 (" ^ bin2string l ^ ")"
    | false :: l -> "x0 (" ^ bin2string l ^ ")"
    | _ -> assert false

let string2int s =
  let foo s = String.fold_right (fun c l -> char2int c :: l) s [] in
  match String.get s 0 with
  | '0' -> "0"
  | '-' -> "Zneg (" ^ (bin2string @@ dec_2_bin_rev @@ List.tl @@ foo s) ^ ")"
  | _ -> "Zpos (" ^ (bin2string @@ dec_2_bin_rev @@ foo s) ^ ")"

let rec print_type_group =
  function
  | Product (t1, t2) -> "(" ^ print_type_group t1 ^ ") * (" ^ print_type_group t2 ^ ")"
  | Id_type s ->
     begin
       match s with
       | "INTEGER" -> "Z_T"
       | "REAL" -> "R_T"
       | "FLOAT" -> "FLOAT_T"
       | "BOOL" -> "BOOL_T"
       | "STRING" -> "STRING_T"
       | s -> "type?" ^ s
     end
  | Pow t -> "Set (" ^ print_type_group t ^ ")"
  | Struct_type l -> "struct_T (" ^ (print_struct l) ^ ")"
  | Generic_Type -> failwith "should not happen"
and print_struct =
  function
  | [] -> assert false
  | [(i,x)] -> "struct_nil label?" ^ i ^ " (" ^ (print_type_group x) ^ ")"
  | (i,x) :: l -> "struct_cons label?" ^ i ^ " (" ^ (print_type_group x) ^ ") (" ^ print_struct l ^ ")"

let prove_access s =
  let rec foo =
    function
      | [(x, _)] when x = s -> "accessible_nil"
      | (x, _) :: l when x = s -> "accessible_cons"
      | x :: l -> "skip (" ^ foo l ^ ")"
      | _ -> failwith "label " ^ s ^ " not found"
  in
  function
    | Struct_type l -> foo l
    | _ -> failwith "not a record"

let print_var_list =
  List.fold_left (fun s (n,x,y) -> s ^ " (" ^ name n x y ^ " : τ (" ^ print_type_group (get_type n) ^ "))") ""

let rec print_curry_helper =
  function
  | [] -> assert false
  | [(x,_,_)] -> "typnil (" ^ (print_type_group (get_type x)) ^ ")"
  | (x,_,_) :: l -> "typcons (" ^ (print_type_group (get_type x)) ^ ") (" ^ print_curry_helper l ^ ")"

let rec print_pred_group =
  function
  | Binary_Pred (op, p1, p2) ->
     let p1 = print_pred_group p1 in
     let p2 = print_pred_group p2 in
     begin
       match op with
         | Imply -> "(" ^ p1 ^ ") ⇒ (" ^ p2 ^ ")"
         | Equiv -> "(" ^ p1 ^ ") ⇔ (" ^ p2 ^ ")"
     end
  | Exp_Comparison (op, e1, e2) ->
     let e1 = print_expr_group e1 in
     let e2 = print_expr_group e2 in
     begin
       match op with
       | Belong -> "(" ^ e1 ^ ") ∈ (" ^ e2 ^ ")"
       | NotBelong -> "(" ^ e1 ^ ") notin (" ^ e2 ^ ")"
       | Included -> "(" ^ e1 ^ ") ⊆ (" ^ e2 ^ ")"
       | NotIncluded -> "(" ^ e1 ^ ") notincluded (" ^ e2 ^ ")"
       | StrictIncluded -> "(" ^ e1 ^ ") ⊂ (" ^ e2 ^ ")"
       | NotStrictIncluded -> "(" ^ e1 ^ ") notstrictincluded (" ^ e2 ^ ")"
       | Equal -> "(" ^ e1 ^ ") = (" ^ e2 ^ ")"
       | NotEqual -> "(" ^ e1 ^ ") ≠ (" ^ e2 ^ ")"
       | IGeq -> "(" ^ e1 ^ ") ≥i (" ^ e2 ^ ")"
       | IGreater -> "(" ^ e1 ^ ") >i (" ^ e2 ^ ")"
       | ISmaller -> "(" ^ e1 ^ ") <i (" ^ e2 ^ ")"
       | ILeq -> "(" ^ e1 ^ ") ≤i (" ^ e2 ^ ")"
       | RGeq -> "(" ^ e1 ^ ") ≥r (" ^ e2 ^ ")"
       | RGreater -> "(" ^ e1 ^ ") >r (" ^ e2 ^ ")"
       | RSmaller -> "(" ^ e1 ^ ") <r (" ^ e2 ^ ")"
       | RLeq -> "(" ^ e1 ^ ") ≤r (" ^ e2 ^ ")"
       | FGeq -> "(" ^ e1 ^ ") ≥f (" ^ e2 ^ ")"
       | FGreater -> "(" ^ e1 ^ ") >f (" ^ e2 ^ ")"
       | FSmaller -> "(" ^ e1 ^ ") <f (" ^ e2 ^ ")"
       | FLeq -> "(" ^ e1 ^ ") ≤f (" ^ e2 ^ ")"
     end
  | Quantified_Pred (op, v, p) ->
     begin
       match op with
       | Forall -> (List.fold_right (fun (n,x,y) s -> "`∀ " ^ name n x y ^ " : τ (" ^ print_type_group (get_type n) ^ "), (" ^ s ^ ")") v (print_pred_group p))
       | Exists -> (List.fold_right (fun (n,x,y) s -> "`∃ " ^ name n x y ^ " : τ (" ^ print_type_group (get_type n) ^ "), (" ^ s ^ ")") v (print_pred_group p))
     end
  | Not p -> "¬ (" ^ print_pred_group p ^ ")"
  | Nary_Pred (op, pl) ->
     let sop = begin
     match op with
     | And -> "∧"
     | Or -> "∨"
     end in
     begin
       match pl with
       | [] -> if op = And then "FALSE" else "TRUE"
       | pl -> List.fold_left (fun s l -> "(" ^ s ^ ") " ^ sop ^ " (" ^ (print_pred_group l) ^ ")") (print_pred_group @@ List.hd pl) @@ List.tl pl
     end
and print_expr_group =
  function
  | Unary_Exp (n,op,e) ->
     let e = print_expr_group e in
     begin
       match op with
       | Max -> failwith "should not exist"
       | IMax -> "imax (" ^ e ^ ")"
       | RMax -> "rmax (" ^ e ^ ")"
       | Min -> failwith "should not exist"
       | IMin -> "imin (" ^ e ^ ")"
       | RMin -> "rmin (" ^ e ^ ")"
       | Card -> "card (" ^ e ^ ")"
       | Dom -> "dom (" ^ e ^ ")"
       | Ran -> "ran (" ^ e ^ ")"
       | POW -> "pow (" ^ e ^ ")"
       | POW1 -> "pow1 (" ^ e ^ ")"
       | FIN -> "fin (" ^ e ^ ")"
       | FIN1 -> "fin1 (" ^ e ^ ")"
       | Union_gen -> "union_gen (" ^ e ^ ")"
       | Inter_gen -> "inter_gen (" ^ e ^ ")"
       | Seq -> "seq (" ^ e ^ ")"
       | Seq1 -> "seq1 (" ^ e ^ ")"
       | ISeq -> "iseq (" ^ e ^ ")"
       | Iseq1 -> "iseq1 (" ^ e ^ ")"
       | Minus -> failwith "should not exist"
       | IMinus -> "minusi (" ^ e ^ ")"
       | RMinus -> "minusr (" ^ e ^ ")"
       | Inversion -> "~ (" ^ e ^ ")"
       | Size -> "size (" ^ e ^ ")"
       | Perm -> "perm (" ^ e ^ ")"
       | First -> "first (" ^ e ^ ")"
       | Last -> "last (" ^ e ^ ")"
       | Identity -> "id (" ^ e ^ ")"
       | Closure -> "closure (" ^ e ^ ")"
       | Closure1 -> "closure1 (" ^ e ^ ")"
       | Tail -> "tail (" ^ e ^ ")"
       | Front -> "front (" ^ e ^ ")"
       | Rev -> "rev (" ^ e ^ ")"
       | Conc -> "conc (" ^ e ^ ")"
       | Succ -> "succ (" ^ e ^ ")"
       | Pred -> "pred (" ^ e ^ ")"
       | Rel -> "rel (" ^ e ^ ")"
       | Fnc -> "fnc (" ^ e ^ ")"
       | Real -> "real (" ^ e ^ ")"
       | Floor -> "floor (" ^ e ^ ")"
       | Ceiling -> "ceiling (" ^ e ^ ")"
       | Tree -> "tree (" ^ e ^ ")"
       | Btree -> "btree (" ^ e ^ ")"
       | Top -> "top (" ^ e ^ ")"
       | Sons -> "sons (" ^ e ^ ")"
       | Prefix -> "pref (" ^ e ^ ")"
       | Postfix -> "post (" ^ e ^ ")"
       | Sizet -> "sizet (" ^ e ^ ")"
       | Mirror -> "mirror (" ^ e ^ ")"
       | Left -> "left_tree (" ^ e ^ ")"
       | Right -> "right_tree (" ^ e ^ ")"
       | Infix -> "infix_tree_ (" ^ e ^ ")"
       | Bin_unary -> "bin_unary (" ^ e ^ ")"
     end
  | Binary_Exp (n,op,e1,e2) ->
     let e1 = print_expr_group e1 in
     let e2 = print_expr_group e2 in
     begin
       match op with
       | Pair -> "pair (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | Prod -> failwith "should not exist"
       | IProd -> "(" ^ e1 ^ ") *_i (" ^ e2 ^ ")"
       | RProd -> "(" ^ e1 ^ ") *_r (" ^ e2 ^ ")"
       | FProd -> "(" ^ e1 ^ ") *_f (" ^ e2 ^ ")"
       | SProd -> "(" ^ e1 ^ ") *_s (" ^ e2 ^ ")"
       | Exp -> failwith "should not exist"
       | IExp -> "(" ^ e1 ^ ") **i (" ^ e2 ^ ")"
       | RExp -> "(" ^ e1 ^ ") **r (" ^ e2 ^ ")"
       | Plus -> failwith "should not exist"
       | IPlus -> "(" ^ e1 ^ ") +_i (" ^ e2 ^ ")"
       | RPlus -> "(" ^ e1 ^ ") +_r (" ^ e2 ^ ")"
       | FPlus -> "(" ^ e1 ^ ") +_f (" ^ e2 ^ ")"
       | Partial -> "(" ^ e1 ^ ") +-> (" ^ e2 ^ ")"
       | PartialSurj -> "(" ^ e1 ^ ") +->> (" ^ e2 ^ ")"
       | Minus -> failwith "should not exist"
       | IMinus -> "(" ^ e1 ^ ") -_i (" ^ e2 ^ ")"
       | RMinus -> "(" ^ e1 ^ ") -_r (" ^ e2 ^ ")"
       | FMinus -> "(" ^ e1 ^ ") -_f (" ^ e2 ^ ")"
       | SMinus -> "(" ^ e1 ^ ") -_s (" ^ e2 ^ ")"
       | Total -> "(" ^ e1 ^ ") --> (" ^ e2 ^ ")"
       | TotalSurj -> "(" ^ e1 ^ ") -->> (" ^ e2 ^ ")"
       | HeadInsert -> "(" ^ e1 ^ ") -> (" ^ e2 ^ ")"
       | Interval -> "(" ^ e1 ^ ") -- (" ^ e2 ^ ")"
       | Div -> failwith "should not exist"
       | IDiv -> "(" ^ e1 ^ ") div_i (" ^ e2 ^ ")"
       | RDiv -> "(" ^ e1 ^ ") div_r (" ^ e2 ^ ")"
       | FDiv -> "(" ^ e1 ^ ") div_f (" ^ e2 ^ ")"
       | Inter -> "(" ^ e1 ^ ") ∩ (" ^ e2 ^ ")"
       | HeadRestrict -> "headrestrict (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | Compose -> "comp (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | Overload -> "(" ^ e1 ^ ") <+ (" ^ e2 ^ ")"
       | Relation -> "(" ^ e1 ^ ") <-> (" ^ e2 ^ ")"
       | TailInsert -> "(" ^ e1 ^ ") <- (" ^ e2 ^ ")"
       | DomSubtract -> "dom- (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | DomRestrict -> "domrestrict (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | PartialInject -> "(" ^ e1 ^ ") >+> (" ^ e2 ^ ")"
       | TotalInject -> "(" ^ e1 ^ ") >-> (" ^ e2 ^ ")"
       | DoesntExist -> failwith "should not exist"
       | TotalBiject -> "(" ^ e1 ^ ") >->> (" ^ e2 ^ ")"
       | DirectProd -> "(" ^ e1 ^ ") >< (" ^ e2 ^ ")"
       | ParallelProd -> "(" ^ e1 ^ ") par (" ^ e2 ^ ")"
       | Union -> "(" ^ e1 ^ ") ∪ (" ^ e2 ^ ")"
       | TailRestrict -> "tailrestrict (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | Concat -> "(" ^ e1 ^ ") ^ (" ^ e2 ^ ")"
       | Mod -> "(" ^ e1 ^ ") mod (" ^ e2 ^ ")"
       | Maplet -> "(" ^ e1 ^ ") map (" ^ e2 ^ ")"
       | ImageRestrict -> "imagerestrict (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | ImageSubtract -> "image- (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | Image -> "image (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | Eval -> "eval (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | Doesntexist2 -> failwith "should not exist"
       | Prj1 -> "prj1 (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | Prj2 -> "prj2 (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | Iterate -> "iterate (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | Const -> "const (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | Rank -> "rank (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | Father -> "father (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | Subtree -> "subtree (" ^ e1 ^ ") (" ^ e2 ^ ")"
       | Arity -> "arity (" ^ e1 ^ ") (" ^ e2 ^ ")"
     end
  | Ternary_Exp (n, op, e1, e2, e3) -> "TODO ternary"
  | Nary_Exp (n, op, el) ->
     let s = List.fold_left (fun s e -> "cons (" ^ (print_expr_group e) ^ ") ("^ s ^ ")") ("cons (" ^ (print_expr_group @@ List.hd @@ List.rev el) ^ ") nil") (List.tl @@ List.rev el) in
     begin
       match op with
       | Sequence -> "sequence (" ^ s ^ ")"
       | Extension -> "extension (" ^ s ^ ")"
     end
  | Boolean_Literal (n, b) -> if b then "true" else "false"
  | Boolean_Exp (n, p) -> "Boolean_Exp (" ^ print_pred_group p ^ ")"
  | EmptySet n ->
     begin
       match get_type n with
       | Pow t -> "emptyset (" ^ print_type_group t ^ ")"
       | _ -> failwith "not a set"
     end
  | EmptySeq n ->
     begin
       match get_type n with
       | Pow (Product(Id_type "INTEGER",t)) -> "emptyseq (" ^ print_type_group t ^ ")"
       | _ -> failwith "not a set"
     end
  | Id_exp (n,id,o) -> name n id o
  | Integer_Literal (n, s) -> string2int s
  | Quantified_Exp (n, op, v, p, e) ->
     begin
       match op with
       | Lambda -> "%" ^ print_quantified v p e
       | SIGMA -> failwith "should not exist"
       | ISIGMA -> "iSIGMA" ^ print_quantified v p e
       | RSIGMA -> "rSIGMA" ^ print_quantified v p e
       | PI -> failwith "should not exist"
       | IPI -> "iPI" ^ print_quantified v p e
       | RPI -> "rPI" ^ print_quantified v p e
       | INTER -> "INTER" ^ print_quantified v p e
       | UNION -> "UNION" ^ print_quantified v p e
     end
  | Quantified_Set (n, v, p) ->
     let abs = " (λ" ^ (print_var_list v) ^ ", " in
   "comprehension [" ^ print_curry_helper v ^ "]" ^ abs ^ (print_pred_group p) ^ ")"
  | STRING_Literal (n, s) -> "TODO string"
  | Struct_exp (n, l) -> "struct (" ^ (print_struct l) ^ ")"
  | Record (n, l) -> "record (" ^ (print_struct l) ^ ")"
  | Real_Literal (n, l) -> "TODO real"
  | Record_Update (e1,s,e2) -> "TODO record update"
  | Record_Field_Access (n, e, s) ->
     let t = find_type e in
     let p = prove_access s t in
     "record_field_access (" ^ print_expr_group e ^ ") label?" ^ s ^ " (" ^ p ^ ")"
and print_quantified v p e =
  let abs = " (λ" ^ (print_var_list v) ^ ", " in
   " [" ^ print_curry_helper v ^ "]" ^ abs ^ (print_pred_group p) ^ ")" ^ abs ^ (print_expr_group e) ^ ")"
and print_struct =
  function
  | [] -> assert false
  | [(i,x)] -> "record_nil label?" ^ i ^ " (" ^ (print_expr_group x) ^ ")"
  | (i,x) :: l -> "record_cons label?" ^ i ^ " (" ^ (print_expr_group x) ^ ") (" ^ print_struct l ^ ")"

let print_expr =
  function
  | Pred p -> print_pred_group p
  | Expr e -> print_expr_group e

let print_typ =
  function
  | ID -> "ID"
  | T -> "T"
  | U -> "U"
  | Thm p -> "Thm (" ^ (print_pred_group p) ^ ")"
  | Tau t -> "τ (" ^ (print_type_group t) ^ ")"

let print_def (Def (s, (t,e))) = let t = print_typ t in
                                match e with
                                | None -> "constant symbol " ^ s ^ " : " ^ t ^ ";\n"
                                | Some e -> let e = print_expr e in
                                          "symbol " ^ s ^ " : " ^ t ^ " ≔ " ^ e ^ ";\n"


let print_ns (NS (s,dl)) = List.fold_left (fun s d -> s ^ (print_def d)) ("/* " ^ s ^ " */\n") dl ^ "\n"

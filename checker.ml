open Pogparser

(*
list id's, their type, definitions (if any), dependencies
*)

(*
define --> name (of section), sets (with names and list of ids inside), and predicates (without name)
po --> name (of section), list of hypothesis (how to encode?) (no name), list of local hypothesis (no name), goals (goals have name)
maybe a po could be a section/module (does that exist?)
typeinfo --> name, type is T, may have definition, no dependency (not a module).
*)

let ti = ref []

type expr = Pred of pred_group | Expr of exp_group

type typ = ID | T | U | Thm of pred_group | Tau of type_group

type def =
  Def of string * (typ * expr option)

type namespace =
| NS of string * def list

let typeInfos2ns (TI l) =
  let rec add_list l =
    function
    | Product (t1, t2) -> let l = add_list l t1 in add_list l t2
    | Id_type s -> if List.mem s predefined_sets then
                     l
                   else if List.mem s l then
                       l
                     else s :: l
    | Pow t -> add_list l t
    | Struct_type s -> List.fold_left (fun l (_,t) -> add_list l t) l s
    | Generic_Type -> l
  in
  let l =
    List.fold_left add_list [] @@ List.map (fun (_,x) -> x) l |>
      List.map (fun x -> Def (x,(T,None))) in NS ("Types", l)

let get_type n = List.assoc n !ti
let name n s o =
  match o with
  | None -> if List.mem s predefined_sets then s else s ^ "_" ^ (string_of_int n)
  | Some x -> s ^ "'" ^ (string_of_int x) ^ "_" ^ (string_of_int n)

let rec find_type =
  function
  | Unary_Exp (n,_,_) | Binary_Exp (n,_,_,_) | Ternary_Exp (n,_,_,_,_)| Nary_Exp (n,_,_) | Boolean_Literal (n,_) | Boolean_Exp (n,_) | EmptySet n | EmptySeq n | Id_exp (n,_,_) | Integer_Literal (n, _) | Quantified_Exp (n,_,_,_,_) | Quantified_Set (n,_,_) | STRING_Literal (n,_) | Struct_exp (n,_) | Record (n,_) | Real_Literal (n,_) | Record_Field_Access (n,_,_) -> get_type n
  | Record_Update (e,_,_) -> find_type e

let const2def (t,s,o) =
  Def (name t s o, (Tau (get_type t), None))

let free_vars2ns () =
  NS ("constants", List.map const2def @@ varlist ())

let strings2ns () =
  NS ("IDs", List.map (fun x -> Def (x, (ID, None))) @@ stringlist ())

let set2def (Set ((t, s, o), l)) =
  match get_type t with
  | Pow x ->
     begin
       match l with
       | None -> rem_var (t,s,o); [Def (name t s o, (Tau (Pow x), None))]
       | Some l ->
          rem_var (t,s,o);
          let e = Nary_Exp (t, Extension, List.map (fun x -> Id_exp x) l)
          in
          let d = Def (name t s o, (Tau (Pow x), Some (Expr e))) in
          List.fold_left (fun l i -> rem_var i; const2def i :: l) [d] (List.rev l)
     end
  | _ -> failwith ("Error:" ^ s ^ "does not have a set type")

let define2ns (Define (s,sl,pl)) =
  let s' = if s = "B definitions" then "B" else s in
  let sd = List.concat @@ List.map set2def sl in
  let pd = List.mapi (fun i x -> Def (string_of_int i ^ s', (Thm x, None))) pl in
  NS (s,sd), NS (s, pd)

let simple_goal2ns suffix gl ll i (Simple_Goal (s, lh, pl, _)) =
  let s = sanitizename s in
  let hl = List.concat [gl; List.map (fun x -> List.assoc x ll) lh] in
  let g = Nary_Pred (And, pl) in
  let po = List.fold_right (fun x p -> Binary_Pred (Imply, x, p)) hl g in
  Def ((string_of_int i) ^ s ^ suffix, (U, Some (Pred po)))

let po2ns i (PO (s,_,gl,ll,gol)) =
  let suffix = "_" ^ (string_of_int i) ^ sanitizename s in
  NS (s, List.mapi (simple_goal2ns suffix gl ll) gol)

let rec insert s =
  if List.mem s predefined_sets then fun x -> x else
    function
    | [] -> [s]
    | x :: l when x > s -> s :: x :: l
    | x :: l when x = s -> x :: l
    | x :: l -> x :: (insert s l)

let ti2ns () =
  let rec foo l =
    function
    | Product (t1, t2) -> let l = foo l t1 in foo l t2
    | Id_type s -> insert s l
    | Pow t -> foo l t
    | Struct_type sl -> List.fold_left (fun l (_,t) -> foo l t) l sl
    | Generic_Type -> failwith "should not happen"
  in
  let l = List.fold_left (fun l (_,t) -> foo l t) [] !ti in
  NS ("Types", List.map (fun s -> Def ("type?" ^ s, (T, None))) l)

let pos2ns (POs (d, po, ti')) =
  let ti' = match ti' with
    | None -> []
    | Some (TI x) -> x
  in
  ti := ti';
  let t = ti2ns () in
  let ds, dp = List.split @@ List.map define2ns d in
  let i = strings2ns () in
  let c = free_vars2ns () in (* c should be evaluated after ds, because of side-effects *)
  let p = List.mapi po2ns po in
  t :: i :: c :: List.concat [ds; dp; p]

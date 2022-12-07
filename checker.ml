open Parser

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

type typ = T | U | Thm of pred_group | Tau of type_group

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
let name s o =
  match o with
  | None -> s
  | Some x -> s ^ "'" ^ (string_of_int x)

let const2def (t,s,o) =
  Def (name s o, (Tau (get_type t), None))

let free_vars2ns () =
  NS ("constants", List.map const2def @@ varlist ())

let set2def (Set ((t, s, o), l)) =
  match get_type t with
  | Pow x ->
     begin
       match l with
       | None -> rem_var (t,s,o); [Def (name s o, (Tau (Pow x), None))]
       | Some l ->
          rem_var (t,s,o);
          let e = Nary_Exp (t, Extension, List.map (fun x -> Id_exp x) l)
          in
          let d = Def (name s o, (Tau (Pow x), Some (Expr e))) in
          List.fold_left (fun l i -> rem_var i; const2def i :: l) [d] (List.rev l)
     end
  | _ -> failwith ("Error:" ^ s ^ "does not have a set type")

let define2ns (Define (s,sl,pl)) =
  let s' = if s = "B definitions" then "B" else s in
  let sd = List.concat @@ List.map set2def sl in
  let pd = List.mapi (fun i x -> Def (string_of_int i ^ s', (Thm x, None))) pl in
  NS (s,List.concat [sd;pd])

let po2ns (PO (s,dl,hl,lhl,gl)) = failwith "TODO"

let pos2ns (POs (d, po, ti')) =
  let ti' = match ti' with
    | None -> []
    | Some (TI x) -> x
  in ti := ti';
  let d = List.map define2ns d in
  let c = free_vars2ns () in (* c should be evaluated after d, because of side-effects *)
  print_endline "TODO po";
  c :: d

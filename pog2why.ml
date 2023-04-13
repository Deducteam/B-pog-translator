open Why3
open Whyutils

type ast = Element of string * (string * string) list * ast list | Text of string
exception Bad_Ast of ast
let parse_fail x = raise (Bad_Ast x)

let counter () =
  object
    val mutable x = 0

    method init =
      x <- 0

    method get =
      let y = x in
      x <- x + 1;
      y
  end

let one_table id =
  object
    val id = id
    val c = counter ()
    val h = Hashtbl.create 128

    method add x = Hashtbl.add h c#get x
    method iter f = Hashtbl.iter (fun _ -> f) h
    method get n =
      try
        Some (Hashtbl.find h n)
      with
        Not_found -> prerr_endline @@ "Not found: " ^ id ^ " " ^ string_of_int n; None
  end

let possible_contexts = ["B definitions";"ctx";"seext";"inv";"ass";"lprp";"inprp";"inext";"cst";"sets";"mchcst";"aprp";"abs";"imlprp";"imprp";"imext"]

let type_infos = Hashtbl.create 128
let define_table = Hashtbl.create (List.length possible_contexts)
let po_table = one_table "Proof Obligation"
let anon_table = Hashtbl.create 128

let type_name x = "t_" ^ x
let label_name x = "l_" ^ x
let var_name x s n = "v_" ^ x ^ (match s with None -> "" | Some x -> "'" ^ x) ^ "_" ^ n
let anon_set_name = "anon_set"
let anon_fun_name = "anon_fun"
let anon_bool_name = "anon_bool"

let parse_type_group id_set =
  let id_check s =
    if Stdlib.not (Hashtbl.mem id_set s) then
      begin
        let new_id = Ty.create_tysymbol (Ident.id_fresh s) [] Ty.NoDef in
        Hashtbl.add id_set s new_id;
        my_theory := Theory.add_decl !my_theory @@ Decl.create_ty_decl new_id;
        new_id
      end
    else Hashtbl.find id_set s
  in
  let rec foo =
    let parse_record_item =
      function
      | Element ("Record_Item", args, [x]) ->
         let label = List.assoc "label" args |> label_name in
         failwith "TODO struct items (how?)"
      | x -> parse_fail x
    in
    function
    | Element ("Binary_Exp", args, [x;y]) ->
       Ty.ty_tuple [foo x; foo y]
    | Element ("Id", args, children) ->
       begin
         match List.assoc "value" args with
         | "INTEGER" -> Ty.ty_int
         | "REAL" -> Ty.ty_real
         | "FLOAT" -> Ty.ty_real
         | "BOOL" -> Ty.ty_bool
         | "STRING" -> Ty.ty_str
         | s -> let s = (type_name s) in Ty.ty_app (id_check s) []
       end
    | Element ("Unary_Exp", args, [x]) -> Ty.ty_app set [foo x]
    | Element ("Struct", args, children) -> failwith "TODO struct (how?)"
    | x -> parse_fail x
  in foo

let parse_type_infos =
  let id_set = Hashtbl.create 128 in
  List.iter @@
    function
    | Element ("Type", args, [x]) ->
       let i = int_of_string @@ List.assoc "id" args in
       let t = parse_type_group id_set x in
       Hashtbl.add type_infos i (x, t)
    | x -> parse_fail x

let rec parse_pred =
  function
  | Element ("Binary_Pred", args, [x;y]) ->
     let op = List.assoc "op" args |> binary_op in
     let c1 = parse_pred x in
     let c2 = parse_pred y in
     op c1 c2
  | Element ("Exp_Comparison", args, [x;y]) ->
     let op = List.assoc "op" args |> binary_op in
     let c1 = parse_exp x in
     let c2 = parse_exp y in
     op c1 c2
  | Element ("Quantified_Pred", args, [Element ("Variables", _, children);Element ("Body", _, [b])]) ->
     let op = List.assoc "type" args |> quantified_pred_op in
     let v = parse_variables children in
     let p = parse_pred b in
     let () = rem_variables children in
     op v p
  | Element ("Unary_Pred", args, [x]) as err ->
     let () = if List.assoc "op" args <> "not" then parse_fail err in (* for debugging *)
     not (parse_pred x)
  | Element ("Nary_Pred", args, children) ->
     let op = List.assoc "op" args |> nary_op in
     let l = List.map parse_pred children in
     op l
  | x -> parse_fail x
and parse_exp =
  let parse_record_item =
    function
    | Element ("Record_Item", args, [x]) ->
       let label = List.assoc "label" args |> label_name |> lp_id in
       let c = parse_exp x in
       label, c
    | x -> parse_fail x
  in
  function
  | Element ("Unary_Exp", args, [x]) ->
     let op = List.assoc "op" args |> unary_op in
     let c = parse_exp x in
     op c
  | Element ("Binary_Exp", args, [x;y]) ->
     let op = List.assoc "op" args |> binary_op in
     let c1 = parse_exp x in
     let c2 = parse_exp y in
     op c1 c2
  | Element ("Ternary_Exp", args, [x;y;z]) ->
     let op = List.assoc "op" args |> ternary_op in
     let c1 = parse_exp x in
     let c2 = parse_exp y in
     let c3 = parse_exp z in
     op c1 c2 c3
  | Element ("Nary_Exp", args, children) ->
     let op = List.assoc "op" args |> nary_op in
     let c = List.map parse_exp children in
     op c
  | Element ("Boolean_Literal", args, _) ->
     begin
       match List.assoc "value" args with
       | "TRUE" -> trueb
       | "FALSE" -> falseb
       | _ -> failwith "wrong Boolean literal"
     end
  | Element ("Boolean_Exp", _, [x]) ->
     let c = parse_pred x in
     boolean_exp c
  | Element ("EmptySet", args, _) ->
     let t = List.assoc "typref" args |> int_of_string in
     emptyset (unset (Hashtbl.find type_infos t))
  | Element ("EmptySeq", args, children) ->
     let t = List.assoc "typref" args |> int_of_string in
     emptyseq (unseq (Hashtbl.find type_infos t))
  | Element ("Id", args, children) ->
     let o = List.assoc "typref" args in
     let t = o |> int_of_string |> Hashtbl.find type_infos in
     let id = List.assoc "value" args in
     let a = List.assoc_opt "suffix" args in
     let name = object_name id o a in
     begin
       env#add name t None;
       lp_id name
     end
  | Element ("Integer_Literal", args, children) ->
     let v = List.assoc "value" args in
     let name = int_name v in
     begin
       env#add name z_t (Some (int v));
       lp_id name
     end
  | Element ("Quantified_Exp", args, [Element ("Variables", _, children);Element ("Pred", _, [p]);Element ("Body", _, [b])]) ->
     let op = List.assoc "type" args |> quantified_exp_op in
     let v = parse_variables children in
     let p = parse_pred p in
     let b = parse_exp b in
     let () = rem_variables children in
     op v p b
  | Element ("Quantified_Set", args, [Element ("Variables", _, children);Element ("Body", _, [b])]) ->
     let v = parse_variables children  in
     let h = helper v in
     let v = List.map (fun (x,y) -> x, Some (tau y)) v in
     let b = Lp.Binder(Lp.Uid "λ", v, parse_pred b) in
     let () = rem_variables children in
     comprehension h b
  | Element ("STRING_Literal", args, children) ->
     let v = List.assoc "value" args in
     let v = string_name v in
     begin
       print_endline "Warning: strings are not fully supported.";
       env#add v string_t None;
       lp_id v
     end
  | Element ("Struct", args, children) ->
     List.map parse_record_item children |> to_record |> struct_exp
  | Element ("Record", args, children) ->
     List.map parse_record_item children |> to_record |> record
  | Element ("Real_Literal", args, children) ->
     let v = "{| " ^ List.assoc "value" args ^ " |}" in
     begin
       print_endline "Warning: reals are not fully supported.";
       env#add v r_t None;
       lp_id v
     end
  | Element ("Record_Update", args, [x;y]) ->
     let t = List.assoc "typref" args |> int_of_string |> Hashtbl.find type_infos in
     let c = parse_exp x in
     let v = parse_exp y in
     let label = List.assoc "label" args |> label_name |> lp_id in
     let p = accessible_proof label t in
     record_update c label v p
  | Element ("Record_Field_Access", args, [Element(_, args', _) as x]) ->
     let t = List.assoc "typref" args' |> int_of_string |> Hashtbl.find type_infos in
     let c = parse_exp x in
     let label = List.assoc "label" args |> label_name |> lp_id in
     let p = accessible_proof label t in
     record_field_access c label p
  | x -> parse_fail x

let parse_goal hyp loc_hyp l =
  let rec foo =
    function
    | Element ("Tag", _, _) :: l -> foo l
    | Element ("Ref_Hyp", args, _) :: l ->
       let n = List.assoc "num" args in
       let h = n |> Hashtbl.find loc_hyp |> Lazy.force in
       foo l |> Term.t_implies h
    | Element ("Goal", _, [x]) :: _ ->
       parse_pred x
    | Element ("Proof_State", _, _) :: l -> foo l
    | x :: l -> parse_fail x
    | _ -> assert false
  in
  let name = "goal" in
  let term = foo l in
  let term = Queue.fold (fun x h -> Term.t_implies (Lazy.force h) x) hyp in
  term (* TODO add the term as a goal in the theory *)


let add_po_table c =
  let def = Queue.create () in
  let hyp = Queue.create () in
  let loc_hyp = Hashtbl.create 128 in
  let goal = one_table "Goal" in
  let pass =
    function
    | Element("Definition", args, []) ->
       Queue.add (List.assoc "name" args) def
    | Element("Hypothesis", _, [x]) ->
       Queue.add x hyp
    | Element("Local_Hyp", args, [x]) ->
       let n = List.assoc "num" args in
       Hashtbl.add loc_hyp n x
    | Element("Simple_Goal", _, l) ->
       goal#add (lazy (parse_goal hyp loc_hyp l))
    | x -> parse_fail x
  in
  begin
    List.iter pass c;
    po_table#add (def, hyp, loc_hyp, goal)
  end

let foo = lazy (print_endline "foo"; 4)
let bar = lazy (print_endline "bar"; Lazy.force foo)

(* None for all the goals, Some x for the goal x *)
let parse_po choice (def, goal) =
  Queue.iter (fun d -> Lazy.force (Hashtbl.find define_table d)) def;
  match choice with
  | None -> goal#iter Lazy.force
  | Some x -> Lazy.force (goal#get x)


(*
  TODO:
  - for sets defined by comprehension, and booleans defined by predicates, have a hashtbl for them
  - for predicates:
  function that transform them into terms
  - for terms:
  function that transform them into terms
  - for sets:
  functions that transform them into term, and give them a unique name (through a hashtable)
  - for define blocks:
  function that transform sets into declarations, and predicates into axioms, while removing stuff that has already been declared
  - for proof_obligation blocks:
  function that process the "Definition" blocks (unqueue them), add hypotheses as axioms (unqueue them), parse local-hyps (print them only once, if reasked for, only give their name) (Lazy monad?), then parse the wanted goal
 *)

(* choice is None for getting all the POs, or Some([(a1,b1);...;(an,bn)]) for goals a1:b1 ... an:bn *)
let parse_pog choice =
  let pass1 =
    function
    | Element("Define", args, children) ->
       let x = List.assoc "name" args in
       Hashtbl.add define_table x (lazy (parse_def children))
    | Element ("Proof_Obligation", args, children) ->
       add_po_table children
    | Element ("TypeInfos", args, children) -> parse_type_infos children
    | _ -> ()
  in
  function
  | Element ("Proof_Obligations", args, children) ->
     begin
       List.iter pass1 children;
       match choice with
       | None -> po_table#iter (parse_po None)
       | Some l ->
          let error a b = prerr_endline ("Goal " ^ string_of_int a ^ ":" ^ string_of_int b ^" does not exist.") in
          let parse (a,b) =
            Option.value ~default:(error a b)
              (Option.bind (po_table#get a) (fun x -> parse_po (Some b) x))
          in
          List.iter parse l
     end
  | x -> parse_fail x


let () = Format.printf "@[my new theory is as follows:@\n@\n%a@]@."
           Pretty.print_theory (Theory.close_theory !my_theory)

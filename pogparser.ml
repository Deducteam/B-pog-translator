open Syntax
open Expression

let type_infos = Hashtbl.create 128

let package_name = "POG"

let var_file = "vars.lp"
let var_out = ref (Out_channel.stdout)
let require_var = Lp.Dependency (true, true, Qid(package_name, Uid("vars")))

let out = ref (Out_channel.stdout)

let sanitizestring = String.map (function ' ' -> '_' | c -> c)

let label_name x = "l_" ^ x
let type_name x = "t_" ^ x
let string_name x = "s_" ^ (sanitizestring x)
let axiom_name x = "a_" ^ x
let int_name x = x ^ "'"
let constants = ["INTEGER"; "REAL"; "FLOAT"; "BOOL"; "STRING"; int_name "0"]
let object_name x t =
  function
  | None -> if List.mem x constants then x else "o_" ^ x ^ "_" ^ t
  | Some s -> "o_" ^ x ^ "'" ^ s ^ "_" ^ t
let hyp_name x = "h_" ^ x
let goal_name x = "g_" ^ x

class counter =
  object
    val mutable x = 0

    method init =
      x <- 0

    method get =
      let y = x in
      x <- x + 1;
      y
  end
let counter = new counter
let counter' = new counter

let counterfile = new counter

let define_file' x = if x = "B definitions" then "b" else x
let define_file x = define_file' x ^ ".lp"
let po_file s = let n = counterfile#get |> string_of_int in
                s ^ "_" ^ n ^ ".lp"

let env =
  object
    val s = Hashtbl.create 512

    method add x t y =
      if Stdlib.not (Hashtbl.mem s x || List.mem x constants) then
        let opt =
          match y with
          | Some _ -> []
          | None -> [Lp.Constant]
        in
        begin
          Lp.print !var_out @@ Lp.Symbol(opt, Lp.Uid x, [], Some (tau t), y);
          Hashtbl.add s x ()
        end

    method tmp x = Hashtbl.add s x ()

    method rem x = Hashtbl.remove s x
  end

type ast = Element of string * (string * string) list * ast list | Text of string
exception Bad_Ast of ast
let parse_fail x = raise (Bad_Ast x)

let rec to_struct =
  function
  | [] -> assert false
  | [x,y] -> struct_nil x y
  | (x,y) :: l -> struct_cons x y (to_struct l)

let rec to_record =
  function
  | [] -> assert false
  | [x,y] -> record_nil x y
  | (x,y) :: l -> record_cons x y (to_record l)

let rec accessible_proof label =
  function
  | Lp.App(Lp.Id (Lp.Uid "struct_nil"), [(false, x); (false, y)]) -> accessible_nil ()
  | Lp.App(Lp.Id (Lp.Uid "struct_cons"), [(false, x); (false, y); (false, z)]) -> if x = label then accessible_cons () else skip (accessible_proof label z)
  | Lp.App(Lp.Id (Lp.Uid "struct_T"), [(false, x)]) -> accessible_proof label x
  | _ -> failwith "accessible_proof"

let parse_type_group id_set =
  let id_check s =
    if Stdlib.not (Hashtbl.mem id_set s) then
      begin
        Lp.print !var_out (Lp.Symbol ([Lp.Constant], Lp.Uid s, [], Some id, None));
        Hashtbl.add id_set s ()
      end
  in
  let rec foo =
    let parse_record_item =
      function
      | Element ("Record_Item", args, [x]) ->
         let label = List.assoc "label" args |> label_name in
         id_check label;
         let c = foo x in
         lp_id label, c
      | x -> parse_fail x
    in
    function
    | Element ("Binary_Exp", args, [x;y]) ->
       (foo x) * (foo y)
    | Element ("Id", args, children) ->
       begin
         match List.assoc "value" args with
         | "INTEGER" -> z_t
         | "REAL" -> r_t
         | "FLOAT" -> float_t
         | "BOOL" -> bool_t
         | "STRING" -> string_t
         | s -> let s = (type_name s) in
                begin
                  id_check s;
                  lp_id s |> type_t
                end
       end
    | Element ("Unary_Exp", args, [x]) -> set (foo x)
    | Element ("Struct", args, children) ->
       List.map parse_record_item children |> to_struct |> struct_t
    | x -> parse_fail x
  in foo

(* probably useless, but names of define contexts should belong here *)
let possible_contexts = ["B definitions";"ctx";"seext";"inv";"ass";"lprp";"inprp";"inext";"cst";"sets";"mchcst";"aprp";"abs";"imlprp";"imprp";"imext"]

let parse_variables =
  let foo =
    function
    | Element ("Id", args, []) ->
       let o = List.assoc "typref" args in
       let n = o |> int_of_string in
       let id = List.assoc "value" args in
       let name = List.assoc_opt "suffix" args |> object_name id o in
       env#tmp name;
       Lp.Uid name, Hashtbl.find type_infos n
    | x -> parse_fail x
  in List.map foo

let rem_variables =
  let foo =
    function
    | Element ("Id", args, []) ->
       let o = List.assoc "typref" args in
       let id = List.assoc "value" args in
       let name = List.assoc_opt "suffix" args |> object_name id o in
       env#rem name
    | x -> parse_fail x
  in List.iter foo

(* hack for emptyset and emptyseq *)
let unset =
  function
  | Lp.App (_,[(false,x)]) -> x
  | _ -> assert false
let unseq =
  function
  | Lp.App (_,[(false,Lp.Infixapp (_, _, Lp.Id (Lp.Uid "Z_T"), Lp.Uid "*", x))]) -> x
  | _ -> assert false

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

let new_axiom a =
  let name = string_of_int counter#get |> axiom_name in
  let term = parse_pred a in
  Lp.print !out @@ Lp.Symbol([Lp.Constant], Lp.Uid name, [], Some (thm term), None)

let parse_hyp a num =
  let name = hyp_name num in
  let term = parse_pred a in
  begin
    Lp.print !out @@ Lp.Symbol([], Lp.Uid name, [], Some u, Some term)
  end

let parse_goal l =
  let vars = Hashtbl.create 512 in
  let rec foo =
    function
    | Element ("Tag", _, [Text(s)]) :: l->
       begin
         Lp.print !out Lp.Newline;
         Lp.print !out @@ Lp.Comment s;
         Lp.print !out Lp.Newline;
         foo l
       end
    | Element ("Ref_Hyp", args, _) :: l ->
       let n = List.assoc "num" args in
       let h = n |> hyp_name |> lp_id in
       foo l |> imply h
    | Element ("Goal", _, [x]) :: _ ->
       parse_pred x
    | Element ("Proof_State", _, _) :: l -> foo l
    | x :: l -> parse_fail x
    | _ -> assert false
  in
  let name = string_of_int counter'#get |> goal_name in
  let term = foo l in
  let term = Hashtbl.fold (fun x t term -> forall x t term) vars term in
  Lp.print !out @@ Lp.Symbol([], Lp.Uid name, [], Some u, Some term)

let parse_id =
  function
  | Element ("Id", args, []) ->
     let o = List.assoc "typref" args in
     let t = o |> int_of_string |> Hashtbl.find type_infos in
     let v = List.assoc "value" args in
     let s = List.assoc_opt "suffix" args in
     let name = object_name v o s in
     begin
       env#add name t None;
       lp_id name
     end
  | x -> parse_fail x

let parse_set name t =
  function
  | [Element ("Enumerated_Values", _, children)] ->
     let l = List.map parse_id children in
     env#add name t (Some (extension (to_list l)))
  | [] -> env#add name t None
  | _ -> assert false


let parse_define =
  function
  | Element ("Set", _, Element("Id", args,[]) :: l) ->
     let o = List.assoc "typref" args in
     let t = o |> int_of_string |> Hashtbl.find type_infos in
     let v = List.assoc "value" args in
     let s = List.assoc_opt "suffix" args in
     let name = object_name v o s in
     parse_set name t l
  | Element (_, args,_) as a ->
     new_axiom a
  | x -> parse_fail x

let parse_po =
  function
  | Element("Tag", _, [Text(s)]) :: l ->
     begin
       let name = po_file s in
       let x = Out_channel.open_text name in
       let parse_po' =
         function
         | Element("Definition", args, []) ->
            let name = List.assoc "name" args |> define_file' in
            Lp.print !out (Lp.Dependency (true, false, Qid(package_name, Uid(name))))
         | Element("Hypothesis", _, [x]) ->
            new_axiom x
         | Element("Local_Hyp", args, [x]) ->
            List.assoc "num" args
            |> parse_hyp x
         | Element("Simple_Goal", _, l) ->
            parse_goal l
         | x -> parse_fail x
       in
       out := x;
       counter#init;
       counter'#init;
       Lp.print !out Syntax.requireme;
       Lp.print !out require_var;
       List.iter parse_po' l;
       Out_channel.close x
     end
  | _ -> assert false

let parse_type_infos =
         let id_set = Hashtbl.create 128 in
         List.iter @@
           function
           | Element ("Type", args, [x]) ->
              let i = int_of_string @@ List.assoc "id" args in
              let t = parse_type_group id_set x in
              Hashtbl.add type_infos i t
           | x -> parse_fail x

let parse_pog =
  let pass1 =
    function
    | Element ("TypeInfos", args, children) -> parse_type_infos children
    | _ -> ()
  in
  let pass2 =
    function
    | Element("Define", args, children) ->
       let x = List.assoc "name" args |> define_file |> Out_channel.open_text in
       begin
         out := x;
         counter#init;
         Lp.print !out Syntax.requireme;
         Lp.print !out require_var;
         List.iter parse_define children;
         Out_channel.close x
       end
    | Element ("Proof_Obligation", args, children) -> parse_po children
    | _ -> ()
  in
  function
  | Element ("Proof_Obligations", args, children) ->
     begin
       let vars = Out_channel.open_text var_file in
       var_out := vars;
       Lp.print !var_out Syntax.requireme;
       List.iter pass1 children;
       List.iter pass2 children;
       Out_channel.close vars
     end
  | x -> parse_fail x

(* Convert the whole XML file into an ast, kept in memory (consumes lot of memory if the XML is big) *)
let file_to_tree s =
  let open Markup in
  let input, close = file s in
  let output =
    input |> parse_xml |> signals |> trim |>
      tree
        ~text:(fun l -> Text(String.trim @@ String.concat "" l))
        ~element:(fun (_, name) l children -> Element (name, List.map (fun ((_,x),y) -> (x,y)) l, children)) |>
      function
      | Some x -> x
      | None -> failwith "Not an XML file"
  in close (); output

open Syntax
open Expression

(* new idea:
   create a folder for the output
   the POs should become a structure containing:
   - a hashtable for the Define blocks : (string, ast) Hashtbl.t
   - a list for the Proof_Obligation blocks: ast list
   - the typeInfos block : (string, Lp.term) Hashtbl.t
   - the header, obtained from "require open ..." followed by the declarations of the typeInfos blocks

   Then, for each po:
   - take the name of the PO in order to create a new file
   - write the header
   - make a pass on every required define block
   - Have a set of already declared stuff (i.e. free name, integers)
   - if a "required" dependency does not exist, declare it
   - declare all local hypothesis
   - declare all goals

   I should have a global out-file and a global set of ids as an environment
 *)

let sanitizestring = String.map (function ' ' -> '_' | c -> c)

let label_name x = "l_" ^ x
let type_name x = "t_" ^ x
let string_name x = "s_" ^ (sanitizestring x)
let axiom_name x = "a_" ^ x
let constants = ["INTEGER"; "REAL"; "FLOAT"; "BOOL"; "STRING"; "0"]
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

let env =
  object
    val s = Hashtbl.create 512
    val hyp_var = Hashtbl.create 512
    val mutable o = stdout
    val mutable is_hyp = false

    method init out =
      begin
        o <- out;
        Hashtbl.clear s;
        Hashtbl.clear hyp_var
      end

    method add ?(is_cst=false) x t y =
      if Stdlib.not (Hashtbl.mem s x || List.mem x constants) then
        (* if is_hyp && Stdlib.not (is_cst) then Hashtbl.replace hyp_var x t else *)
        let opt =
          match y with
          | Some _ -> []
          | None -> [Lp.Constant]
        in
        begin
          Lp.print o @@ Lp.Symbol(opt, Lp.Uid x, [], Some (tau t), y);
          Hashtbl.add s x ()
        end

    method tmp x = Hashtbl.add s x ()

    (* method get_hyp_var =
      begin
        let args = Hashtbl.fold (fun x tx args -> ((Lp.Uid x, tx) :: args)) hyp_var [] in
        Hashtbl.clear hyp_var; args
      end *)

    method rem x = Hashtbl.remove s x

   (* method flip_hyp = () (* is_hyp <- Stdlib.not (is_hyp) *) *)
  end

type ast = Element of string * (string * string) list * ast list | Text of string
exception Bad_Ast of ast
let parse_fail x = raise (Bad_Ast x)

type pog =
  {
    definitions : (string, ast list) Hashtbl.t;
    obligations : (ast list) Queue.t;
    hypotheses : (string, (Lp.id * Lp.term) list) Hashtbl.t;
    type_infos : (int, Lp.term) Hashtbl.t;
    header : Lp.command list
  }

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

let parse_type_group hd id_set =
  let id_check s =
    if Stdlib.not (Hashtbl.mem id_set s) then
      begin
        Queue.add (Lp.Symbol ([Lp.Constant], Lp.Uid s, [], Some id, None)) hd;
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

let parse_pog =
  let d = Hashtbl.create (List.length possible_contexts) in
  let o = Queue.create () in
  let ti = Hashtbl.create 128 in
  let hp = Hashtbl.create 128 in
  let hd = Queue.create () in
  let iter =
    function
    | Element("Define", args, children) -> Hashtbl.add d (List.assoc "name" args) children
    | Element ("Proof_Obligation", args, children) -> Queue.add children o
    | Element ("TypeInfos", args, children) ->
       begin
         let id_set = Hashtbl.create 128 in
         children |> List.iter @@
                       function
                       | Element ("Type", args, [x]) ->
                          let i = int_of_string @@ List.assoc "id" args in
                          let t = parse_type_group hd id_set x in
                          Hashtbl.add ti i t
                       | x -> parse_fail x
       end
    | x -> parse_fail x
  in
  function
  | Element ("Proof_Obligations", args, children) ->
     begin
       Queue.add Syntax.requireme hd;
       List.iter iter children;
       { definitions = d; obligations = o; hypotheses = hp; type_infos = ti; header = List.of_seq (Queue.to_seq hd) }
     end
  | x -> parse_fail x

let parse_variables ti =
  let foo =
    function
    | Element ("Id", args, []) ->
       let o = List.assoc "typref" args in
       let n = o |> int_of_string in
       let id = List.assoc "value" args in
       let name = List.assoc_opt "suffix" args |> object_name id o in
       env#tmp name;
       Lp.Uid name, Hashtbl.find ti n
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

let rec parse_pred ti =
  let parse_exp x = parse_exp ti x in
  let parse_pred x = parse_pred ti x in
  let parse_variables x = parse_variables ti x in
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
and parse_exp ti =
  let parse_exp x = parse_exp ti x in
  let parse_pred x = parse_pred ti x in
  let parse_variables x = parse_variables ti x in
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
     emptyset (unset (Hashtbl.find ti t))
  | Element ("EmptySeq", args, children) ->
     let t = List.assoc "typref" args |> int_of_string in
     emptyseq (unseq (Hashtbl.find ti t))
  | Element ("Id", args, children) ->
     let o = List.assoc "typref" args in
     let t = o |> int_of_string |> Hashtbl.find ti in
     let id = List.assoc "value" args in
     let a = List.assoc_opt "suffix" args in
     let name = object_name id o a in
     begin
       env#add name t None;
       lp_id name
     end
  | Element ("Integer_Literal", args, children) ->
     let v = List.assoc "value" args in
     begin
       env#add ~is_cst:true v z_t (Some (int v));
       lp_id v
     end
  | Element ("Quantified_Exp", args, [Element ("Variables", _, children);Element ("Pred", _, [p]);Element ("Body", _, [b])]) ->
     let op = List.assoc "type" args |> quantified_exp_op in
     let v = parse_variables children in
     let p = parse_pred p in
     let b = parse_exp b in
     let () = rem_variables children in
     (* let t = List.assoc "typref" args |> int_of_string in emptyset (unset (Hashtbl.find ti t)) (* TODO remove *) *)
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
       env#add ~is_cst:true v string_t None;
       lp_id v
     end
  | Element ("Struct", args, children) ->
     List.map parse_record_item children |> to_record |> struct_exp
  | Element ("Record", args, children) ->
     List.map parse_record_item children |> to_record |> record
  | Element ("Real_Literal", args, children) ->
     let v = List.assoc "value" args in
     begin
       print_endline "Warning: reals are not fully supported.";
       env#add ~is_cst:true v r_t None;
       lp_id v
     end
  | Element ("Record_Update", args, [x;y]) ->
     let t = List.assoc "typref" args |> int_of_string |> Hashtbl.find ti in
     let c = parse_exp x in
     let v = parse_exp y in
     let label = List.assoc "label" args |> label_name |> lp_id in
     let p = accessible_proof label t in
     record_update c label v p
  | Element ("Record_Field_Access", args, [Element(_, args', _) as x]) ->
     let t = List.assoc "typref" args' |> int_of_string |> Hashtbl.find ti in
     let c = parse_exp x in
     let label = List.assoc "label" args |> label_name |> lp_id in
     let p = accessible_proof label t in
     record_field_access c label p
  | x -> parse_fail x

let new_axiom out ti a =
  let name = string_of_int counter#get |> axiom_name in
  let term = parse_pred ti a in
  Lp.print out @@ Lp.Symbol([Lp.Constant], Lp.Uid name, [], Some (thm term), None)

let parse_hyp out ti (* hp *) a num =
  (* let () = env#flip_hyp in *)
  let name = hyp_name num in
  let term = parse_pred ti a in
  (* let () = env#flip_hyp in
  let args = env#get_hyp_var in *)
  begin
    (* Hashtbl.add hp num args; *)
    Lp.print out @@ Lp.Symbol([], Lp.Uid name, [] (* List.map (fun (x,t) -> x, Some (tau t)) args *), Some u, Some term)
  end

let parse_goal out ti (* hp *) l =
  let vars = Hashtbl.create 512 in
  let rec foo =
    function
    | Element ("Tag", _, [Text(s)]) :: l->
       begin
         Lp.print out Lp.Newline;
         Lp.print out @@ Lp.Comment s;
         Lp.print out Lp.Newline;
         foo l
       end
    | Element ("Ref_Hyp", args, _) :: l ->
       let n = List.assoc "num" args in
       let h = n |> hyp_name |> lp_id in
       (* let args = Hashtbl.find hp n in *)
       let f = imply (* (app *) h (* (List.map (fun (x,_) -> (false, Lp.Id x)) args)) *) in
       begin
         (* List.iter (fun (x,t) -> Hashtbl.replace vars x t) args; *)
         foo l |> f
       end
    | Element ("Goal", _, [x]) :: _ ->
       parse_pred ti x
    | Element ("Proof_State", _, _) :: l -> foo l
    | x :: l -> parse_fail x
    | _ -> assert false
  in
  let name = string_of_int counter'#get |> goal_name in
  let term = foo l in
  let term = Hashtbl.fold (fun x t term -> forall x t term) vars term in
  Lp.print out @@ Lp.Symbol([], Lp.Uid name, [], Some u, Some term)

let parse_id out ti =
  function
  | Element ("Id", args, []) ->
     let o = List.assoc "typref" args in
     let t = o |> int_of_string |> Hashtbl.find ti in
     let v = List.assoc "value" args in
     let s = List.assoc_opt "suffix" args in
     let name = object_name v o s in
     begin
       env#add name t None;
       lp_id name
     end
  | x -> parse_fail x

let parse_set out ti name t =
  function
  | [Element ("Enumerated_Values", _, children)] ->
     let l = List.map (parse_id out ti) children in
     env#add name t (Some (extension (to_list l)))
  | [] -> env#add name t None
  | _ -> assert false


let parse_def out ti l =
  let foo = function
    | Element ("Set", _, Element("Id", args,[]) :: l) ->
       let o = List.assoc "typref" args in
       let t = o |> int_of_string |> Hashtbl.find ti in
       let v = List.assoc "value" args in
       let s = List.assoc_opt "suffix" args in
       let name = object_name v o s in
       parse_set out ti name t l
    | Element (_, args,_) as a ->
       new_axiom out ti a
    | x -> parse_fail x
  in
  List.iter foo l

let parse_po pog =
  function
  | Element("Tag", _, [Text(s)]) :: l ->
     begin
       let n = counterfile#get |> string_of_int in
       let out = Out_channel.open_text (s ^ "_" ^ n ^ ".lp") in
       let parse_po' =
         function
         | Element("Definition", args, []) ->
            List.assoc "name" args
            |> Hashtbl.find pog.definitions
            |> parse_def out pog.type_infos
         | Element("Hypothesis", _, [x]) ->
            new_axiom out pog.type_infos x
         | Element("Local_Hyp", args, [x]) ->
            List.assoc "num" args
            |> parse_hyp out pog.type_infos (* pog.hypotheses *) x
         | Element("Simple_Goal", _, l) ->
            parse_goal out pog.type_infos (* pog.hypotheses *) l
         | x -> parse_fail x
       in
       List.iter (Lp.print out) pog.header;
       counter#init;
       counter'#init;
       env#init out;
       List.iter parse_po' l;
       Out_channel.close out
     end
  | _ -> assert false

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

let predefined_sets = ["BOOL"; "INTEGER"; "REAL"; "FLOAT"; "STRING"]

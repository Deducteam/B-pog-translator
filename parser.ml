type ast = Element of string * (string * string) list * ast list | Text of string

exception Bad_Ast of ast

let parse_fail x = raise (Bad_Ast x)

let predefined_sets = ["BOOL"; "INTEGER"; "REAL"; "FLOAT"; "STRING"]

type id = int * string * int option (* for name clashes *)
module Id =
  struct
    type t = int * string * int option (* for name clashes *)
    let compare (_,x,y) (_,x',y') =
      match String.compare x x' with
      | 0 ->
         begin
           match y, y' with
           | Some a, Some b -> Int.compare a b
           | Some a, None -> 1
           | None, Some b -> -1
           | None, None -> 0
         end
      | c -> c
  end

module Id_Set = Set.Make(Id)

let free_vars = ref (Id_Set.empty)
let bound_vars = ref (Id_Set.empty)
let add_var x = if not (Id_Set.mem x !bound_vars) then free_vars := Id_Set.add x !free_vars
let rem_var x = free_vars := Id_Set.remove x !free_vars
let add_bound x = bound_vars := Id_Set.add x !bound_vars
let rem_bound x = bound_vars := Id_Set.remove x !bound_vars
let rem_list l = List.iter rem_bound l
let varlist () = Id_Set.elements !free_vars

let parse_id =
  function
  | Element ("Id", args, children) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     let v = List.assoc "value" args in
     let s = Option.map int_of_string @@ List.assoc_opt "suffix" args in
     let r = typref, v, s in
     begin
       match s with
         | Some x -> add_var r
         | None ->
            if not (List.mem v predefined_sets) then add_var r
     end; r
  | x -> parse_fail x

type binary_pred_op =
  | Imply (* => *)
  | Equiv (* <=> *)

let parse_binary_pred_op =
  function
  | "=>" -> Imply
  | "<=>" -> Equiv
  | x -> failwith ("Invalid binary predicate operator : " ^ x)

type nary_pred_op =
  | And (* & *)
  | Or (* or *)

let parse_nary_pred_op =
  function
  | "&" -> And
  | "or" -> Or
  | x -> failwith ("Invalid n-ary predicate operator : " ^ x)

type comparison_op =
  | Belong (* : *)
  | NotBelong (* /: *)
  | Included (* <: *)
  | NotIncluded (* /<: *)
  | StrictIncluded (* <<: *)
  | NotStrictIncluded (* /<<: *)
  | Equal (* = *)
  | NotEqual (* /= *)
  (* integer comparison *)
  | IGeq (* >=i *)
  | IGreater (* >i *)
  | ISmaller (* <i *)
  | ILeq (* <=i *)
  (* real comparison *)
  | RGeq (* >=r *)
  | RGreater (* >r *)
  | RSmaller (* <r *)
  | RLeq (* <=r *)
  (* float comparison *)
  | FGeq (* >=f *)
  | FGreater (* >f *)
  | FSmaller (* <f *)
  | FLeq (* <=f *)

let parse_comparison_op =
  function
  | ":" -> Belong
  | "/:" -> NotBelong
  | "<:" -> Included
  | "/<:" -> NotIncluded
  | "<<:" -> StrictIncluded
  | "/<<:" -> NotStrictIncluded
  | "=" -> Equal
  | "/=" -> NotEqual
  (* integer comparison *)
  | ">=i" -> IGeq
  | ">i" -> IGreater
  | "<i" -> ISmaller
  | "<=i" -> ILeq
  (* real comparison *)
  | ">=r" -> RGeq
  | ">r" -> RGreater
  | "<r" -> RSmaller
  | "<=r" -> RLeq
  (* float comparison *)
  | ">=f" -> FGeq
  | ">f" -> FGreater
  | "<f" -> FSmaller
  | "<=f" -> FLeq
  | x -> failwith ("Invalid comparison operator : " ^ x)

type quantified_pred_op =
  | Forall (* ! *)
  | Exists (* # *)

let parse_quantified_pred_op =
  function
  | "!" -> Forall
  | "#" -> Exists
  | x -> failwith ("Invalid predicate quantifier : " ^ x)

type unary_exp_op =
  | Max (* max *) (* should not exist *)
  | IMax (* imax *)
  | RMax (* rmax *)
  | Min (* min *) (* should not exist *)
  | IMin (* imin *)
  | RMin (* rmin *)
  | Card (* card *)
  | Dom (* dom *)
  | Ran (* ran *)
  | POW (* POW *)
  | POW1 (* POW1 *)
  | FIN (* FIN *)
  | FIN1 (* FIN1 *)
  | Union_gen (* union *)
  | Inter_gen (* inter *)
  | Seq (* seq *)
  | Seq1 (* seq1 *)
  | ISeq (* iseq *)
  | Iseq1 (* iseq1 *)
  | Minus (* - *) (* should not exist *)
  | IMinus (* -i *)
  | RMinus (* -r *)
  | Inversion (* ~ *)
  | Size (* size *)
  | Perm (* perm *)
  | First (* first *)
  | Last (* last *)
  | Identity (* id *)
  | Closure (* closure *)
  | Closure1 (* closure1 *)
  | Tail (* tail *)
  | Front (* front *)
  | Rev (* rev *)
  | Conc (* conc *)
  | Succ (* succ *)
  | Pred (* pred *)
  | Rel (* rel *)
  | Fnc (* fnc *)
  | Real (* real *)
  | Floor (* floor *)
  | Ceiling (* ceiling *)
  | Tree (* tree *)
  | Btree (* btree *)
  | Top (* top *)
  | Sons (* sons *)
  | Prefix (* prefix *)
  | Postfix (* postfix *)
  | Sizet (* sizet *)
  | Mirror (* mirror *)
  | Left (* left *)
  | Right (* right *)
  | Infix (* infix *)
  | Bin_unary (* bin *)

let parse_unary_exp_op =
  function
  | "max" -> Max (* should not exist *)
  | "imax" -> IMax
  | "rmax" -> RMax
  | "min" -> Min (* should not exist *)
  | "imin" -> IMin
  | "rmin" -> RMin
  | "card" -> Card
  | "dom" -> Dom
  | "ran" -> Ran
  | "POW" -> POW
  | "POW1" -> POW1
  | "FIN" -> FIN
  | "FIN1" -> FIN1
  | "union" -> Union_gen
  | "inter" -> Inter_gen
  | "seq" -> Seq
  | "seq1" -> Seq1
  | "iseq" -> ISeq
  | "iseq1" -> Iseq1
  | "-" -> Minus (* should not exist *)
  | "-i" -> IMinus
  | "-r" -> RMinus
  | "~" -> Inversion
  | "size" -> Size
  | "perm" -> Perm
  | "first" -> First
  | "last" -> Last
  | "id" -> Identity
  | "closure" -> Closure
  | "closure1" -> Closure1
  | "tail" -> Tail
  | "front" -> Front
  | "rev" -> Rev
  | "conc" -> Conc
  | "succ" -> Succ
  | "pred" -> Pred
  | "rel" -> Rel
  | "fnc" -> Fnc
  | "real" -> Real
  | "floor" -> Floor
  | "ceiling" -> Ceiling
  | "tree" -> Tree
  | "btree" -> Btree
  | "top" -> Top
  | "sons" -> Sons
  | "prefix" -> Prefix
  | "postfix" -> Postfix
  | "sizet" -> Sizet
  | "mirror" -> Mirror
  | "left" -> Left
  | "right" -> Right
  | "infix" -> Infix
  | "bin" -> Bin_unary
  | x -> failwith ("Invalid unary expression : " ^ x)

type binary_exp_op =
  | Pair (* , *)
  | Prod (* * *) (* should not exist *)
  | IProd (* *i *)
  | RProd (* *r *)
  | FProd (* *f *)
  | SProd (* *s *)
  | Exp (* ** *) (* should not exist *)
  | IExp (* **i *)
  | RExp (* **r *)
  | Plus (* + *) (* should not exist *)
  | IPlus (* +i *)
  | RPlus (* +r *)
  | FPlus (* +f *)
  | Partial (* +-> *)
  | PartialSurj (* +->> *)
  | Minus (* - *) (* should not exist *)
  | IMinus (* -i *)
  | RMinus (* -r *)
  | FMinus (* -f *)
  | SMinus (* -s *)
  | Total (* --> *)
  | TotalSurj (* -->> *)
  | HeadInsert (* -> *)
  | Interval (* .. *)
  | Div (* / *) (* should not exist *)
  | IDiv (* /i *)
  | RDiv (* /r *)
  | FDiv (* /f *)
  | Inter (* /\ *)
  | HeadRestrict (* /|\ *)
  | Compose (* ; *)
  | Overload (* <+ *)
  | Relation (* <-> *)
  | TailInsert (* <- *)
  | DomSubtract (* <<| *)
  | DomRestrict (* <| *)
  | PartialInject (* >+> *)
  | TotalInject (* >-> *)
  | DoesntExist (* >+>> *) (* should not exist *)
  | TotalBiject (* >->> *)
  | DirectProd (* >< *)
  | ParallelProd (* || *)
  | Union (* \/ *)
  | TailRestrict (* \|/ *)
  | Concat (* ^ *)
  | Mod (* mod *)
  | Maplet (* |-> *)
  | ImageRestrict (* |> *)
  | ImageSubtract (* |>> *)
  | Image (* [ *) (* Image of a relation *)
  | Eval (* ( *) (* Image of a function *)
  | Doesntexist2 (* <' *) (* should not exist *)
  | Prj1 (* prj1 *)
  | Prj2 (* prj2 *)
  | Iterate (* iterate *)
  | Const (* const *)
  | Rank (* rank *)
  | Father (* father *)
  | Subtree (* subtree *)
  | Arity (* arity *)

let parse_binary_exp_op =
  function
  | "," -> Pair
  | "*" -> Prod (* should not exist *)
  | "*i" -> IProd
  | "*r" -> RProd
  | "*f" -> FProd
  | "*s" -> SProd
  | "**" -> Exp (* should not exist *)
  | "**i" -> IExp
  | "**r" -> RExp
  | "+" -> Plus (* should not exist *)
  | "+i" -> IPlus
  | "+r" -> RPlus
  | "+f" -> FPlus
  | "+->" -> Partial
  | "+->>" -> PartialSurj
  | "-" -> Minus (* should not exist *)
  | "-i" -> IMinus
  | "-r" -> RMinus
  | "-f" -> FMinus
  | "-s" -> SMinus
  | "-->" -> Total
  | "-->>" -> TotalSurj
  | "->" -> HeadInsert
  | ".." -> Interval
  | "/" -> Div (* should not exist *)
  | "/i" -> IDiv
  | "/r" -> RDiv
  | "/f" -> FDiv
  | "/\\" -> Inter
  | "/|\\" -> HeadRestrict
  | ";" -> Compose
  | "<+" -> Overload
  | "<->" -> Relation
  | "<-" -> TailInsert
  | "<<|" -> DomSubtract
  | "<|" -> DomRestrict
  | ">+>" -> PartialInject
  | ">->" -> TotalInject
  | ">+>>" -> DoesntExist (* should not exist *)
  | ">->>" -> TotalBiject
  | "><" -> DirectProd
  | "||" -> ParallelProd
  | "\\/" -> Union
  | "\\|/" -> TailRestrict
  | "^" -> Concat
  | "mod" -> Mod
  | "|->" -> Maplet
  | "|>" -> ImageRestrict
  | "|>>" -> ImageSubtract
  | "[" -> Image (* Image of a relation *)
  | "(" -> Eval (* Image of a function *)
  | "<'" -> Doesntexist2 (* should not exist *)
  | "prj1" -> Prj1
  | "prj2" -> Prj2
  | "iterate" -> Iterate
  | "const" -> Const
  | "rank" -> Rank
  | "father" -> Father
  | "subtree" -> Subtree
  | "arity" -> Arity
  | x -> failwith ("Invalid binary expression : " ^ x)

type ternary_exp_op =
  | Son (* son *)
  | Bin_ternary (* bin *)

let parse_ternary_exp_op =
  function
  | "son" -> Son
  | "bin" -> Bin_ternary
  | x -> failwith ("Invalid ternary expression : " ^ x)

type nary_exp_op =
  | Sequence (* [ *)
  | Extension (* { *)

let parse_nary_exp_op =
  function
  | "[" -> Sequence
  | "{" -> Extension
  | x -> failwith ("Invalid n-ary expression : " ^ x)

type quantified_exp_op =
   | Lambda (* % *)
   | SIGMA (* SIGMA *) (* should not exist *)
   | ISIGMA (* iSIGMA *)
   | RSIGMA (* rSIGMA *)
   | PI (* PI *) (* should not exist *)
   | IPI (* iPI *)
   | RPI (* rPI *)
   | INTER (* INTER *)
   | UNION (* UNION *)

let parse_quantified_exp_op =
  function
  | "%" -> Lambda
  | "SIGMA" -> SIGMA (* should not exist *)
  | "iSIGMA" -> ISIGMA
  | "rSIGMA" -> RSIGMA
  | "PI" -> PI (* should not exist *)
  | "iPI" -> IPI
  | "rPI" -> RPI
  | "INTER" -> INTER
  | "UNION" -> UNION
  | x -> failwith ("Invalid expression quantifier : " ^ x)

(* the int is the 0-starting index of the TypeInfos *)
type variables_type = (int * string * int option) list

let parse_variable =
  function
  | Element ("Id", args, []) -> let n = int_of_string @@ List.assoc "typref" args, List.assoc "value" args, Option.map int_of_string @@ List.assoc_opt "suffix" args in add_bound n; n
  | x -> parse_fail x

let parse_variables_type = List.map parse_variable

type pred_group =
  | Binary_Pred of binary_pred_op * pred_group * pred_group
  | Exp_Comparison of comparison_op * exp_group * exp_group
  | Quantified_Pred of quantified_pred_op * variables_type * pred_group
  | Not of pred_group
  | Nary_Pred of nary_pred_op * pred_group list
and exp_group = (* the int is the 0-starting index of the TypeInfos *)
  | Unary_Exp of int * unary_exp_op * exp_group
  | Binary_Exp of int * binary_exp_op * exp_group * exp_group
  | Ternary_Exp of int * ternary_exp_op * exp_group * exp_group * exp_group
  | Nary_Exp of int * nary_exp_op * exp_group list (* non-empty *)
  | Boolean_Literal of int * bool
  | Boolean_Exp of int * pred_group
  | EmptySet of int
  | EmptySeq of int
  | Id_exp of id
  | Integer_Literal of int * string (* represent int as a string to avoid size problems *)
  | Quantified_Exp of int * quantified_exp_op * variables_type * pred_group * exp_group
  | Quantified_Set of int * variables_type * pred_group
  | STRING_Literal of int * string
  | Struct_exp of int * (string * exp_group) list (* non-empty *)
  | Record of int * (string * exp_group) list (* non-empty *)
  | Real_Literal of int * string (* represent a real as a string *)
  | Record_Update of exp_group * string * exp_group
  | Record_Field_Access of int * exp_group * string

let rec parse_pred_group =
  function
  | Element ("Binary_Pred", args, [x;y]) ->
     let op = parse_binary_pred_op @@ List.assoc "op" args in
     let c1 = parse_pred_group x in
     let c2 = parse_pred_group y in
     Binary_Pred (op, c1, c2)
  | Element ("Exp_Comparison", args, [x;y]) ->
     let op = parse_comparison_op @@ List.assoc "op" args in
     let c1 = parse_exp_group @@ x in
     let c2 = parse_exp_group @@ y in
     Exp_Comparison (op, c1, c2)
  | Element ("Quantified_Pred", args, [x;y]) ->
     let op = parse_quantified_pred_op @@ List.assoc "type" args in
     let v =
       begin
         match x with
         | Element ("Variables", _, children) -> parse_variables_type children
         | x -> parse_fail x
       end in
     let p =
       begin
         match y with
         | Element ("Body", _, [x]) -> parse_pred_group x
         | x -> parse_fail x
       end in
     rem_list v; Quantified_Pred (op, v, p)
  | Element ("Unary_Pred", args, [x]) as err ->
     let () = if List.assoc "op" args <> "not" then parse_fail err in
     Not(parse_pred_group x)
  | Element ("Nary_Pred", args, children) ->
     let op = parse_nary_pred_op @@ List.assoc "op" args in
     let c = List.map parse_pred_group children in
     Nary_Pred (op, c)
  | x -> parse_fail x
and parse_exp_group =
  let parse_record_item =
    function
    | Element ("Record_Item", args, [x]) ->
       let label = List.assoc "label" args in
       let c = parse_exp_group x in
       (label, c)
    | x -> parse_fail x
  in
  function
  | Element ("Unary_Exp", args, [x]) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     let op = parse_unary_exp_op @@ List.assoc "op" args in
     let c = parse_exp_group x in
     Unary_Exp (typref, op, c)
  | Element ("Binary_Exp", args, [x;y]) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     let op = parse_binary_exp_op @@ List.assoc "op" args in
     let c1 = parse_exp_group x in
     let c2 = parse_exp_group y in
     Binary_Exp (typref, op, c1, c2)
  | Element ("Ternary_Exp", args, [x;y;z]) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     let op = parse_ternary_exp_op @@ List.assoc "op" args in
     let c1 = parse_exp_group x in
     let c2 = parse_exp_group y in
     let c3 = parse_exp_group z in
     Ternary_Exp (typref, op, c1, c2, c3)
  | Element ("Nary_Exp", args, children) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     let op = parse_nary_exp_op @@ List.assoc "op" args in
     let c = List.map parse_exp_group children in
     Nary_Exp (typref, op, c)
  | Element ("Boolean_Literal", args, children) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     let v =
       begin
         match List.assoc "value" args with
         | "TRUE" -> true
         | "FALSE" -> false
         | _ -> failwith "wrong Boolean literal"
       end in
     Boolean_Literal (typref, v)
  | Element ("Boolean_Exp", args, [x]) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     let c = parse_pred_group x in
     Boolean_Exp(typref, c)
  | Element ("EmptySet", args, children) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     EmptySet (typref)
  | Element ("EmptySeq", args, children) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     EmptySeq (typref)
  | Element ("Id", args, children) as x ->
     let typref, v, s = parse_id x in
     Id_exp (typref, v, s)
  | Element ("Integer_Literal", args, children) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     let v = List.assoc "value" args in
     Integer_Literal (typref, v)
  | Element ("Quantified_Exp", args, [x;y;z]) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     let op = parse_quantified_exp_op @@ List.assoc "type" args in
     let v =
       begin
         match x with
         | Element ("Variables", _, children) -> parse_variables_type children
         | x -> parse_fail x
       end in
     let p =
       begin
         match y with
         | Element ("Pred", _, [x]) -> parse_pred_group x
         | x -> parse_fail x
       end in
     let b =
       begin
         match z with
         | Element ("Body", _, [x]) -> parse_exp_group x
         | x -> parse_fail x
       end in
     rem_list v; Quantified_Exp (typref, op, v, p, b)
  | Element ("Quantified_Set", args, [x;y]) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     let v =
       begin
         match x with
         | Element ("Variables", _, children) -> parse_variables_type children
         | x -> parse_fail x
       end in
     let b =
       begin
         match y with
         | Element ("Body", _, [x]) -> parse_pred_group x
         | x -> parse_fail x
       end in
     rem_list v; Quantified_Set (typref, v, b)
  | Element ("STRING_Literal", args, children) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     let v = List.assoc "value" args in
     STRING_Literal (typref, v)
  | Element ("Struct", args, children) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     let c = List.map parse_record_item children in
     Struct_exp (typref, c)
  | Element ("Record", args, children) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     let c = List.map parse_record_item children in
     Record (typref, c)
  | Element ("Real_Literal", args, children) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     let v = List.assoc "value" args in
     Real_Literal (typref, v)
  | Element ("Record_Update", args, [x;y]) ->
     let c1 = parse_exp_group x in
     let label = List.assoc "label" args in
     let c2 = parse_exp_group y in
     Record_Update (c1, label, c2)
  | Element ("Record_Field_Access", args, [x]) ->
     let typref = int_of_string @@ List.assoc "typref" args in
     let c = parse_exp_group x in
     let label = List.assoc "label" args in
     Record_Field_Access (typref, c, label)
  | x -> parse_fail x

type type_group =
  | Product of type_group * type_group
  | Id_type of string
  | Pow of type_group
  | Struct_type of (string * type_group) list (* non-empty *)
  | Generic_Type

let int_type = Id_type "INTEGER"
let bool_type = Id_type "BOOL"
let real_type = Id_type "REAL"
let float_type = Id_type "FLOAT"
let string_type = Id_type "STRING"

let rec parse_type_group =
  let parse_record_item =
    function
    | Element ("Record_Item", args, [x]) ->
       let label = List.assoc "label" args in
       let c = parse_type_group @@ x in
       (label, c)
    | x -> parse_fail x
  in
  function
  | Element ("Binary_Exp", args, [x;y]) ->
     let c1 = parse_type_group x in
     let c2 = parse_type_group y in
     Product (c1, c2)
  | Element ("Id", args, children) ->
     let v = List.assoc "value" args in
     Id_type v
  | Element ("Unary_Exp", args, [x]) ->
     let c = parse_type_group x in
     Pow c
  | Element ("Struct", args, children) ->
     let c = List.map parse_record_item children in
     Struct_type c
  | Element ("Generic_Type", args, children) ->
     Generic_Type
  | x -> parse_fail x

(* probably useless, but names of define contexts should belong here *)
let possible_contexts = ["B definitions";"ctx";"seext";"inv";"ass";"lprp";"inprp";"inext";"cst";"sets";"mchcst";"aprp";"abs";"imlprp";"imprp";"imext"]

type set = Set of id * id list option

let parse_set =
  function
  | Element ("Set", args, x::children) ->
     let typref, v, s = parse_id @@ x in
     let c =
       begin
         match children with
         | [Element ("Enumerated_Values", args, children)] ->
            Option.some @@ List.map parse_id children
         | _ -> None
       end in
     Set ((typref, v, s), c)
  | x -> parse_fail x

type define = Define of string * set list * pred_group list

let take s l = List.filter (function | Element (n,_,_) -> n = s | _ -> false) l

let parse_define =
  function
  | Element ("Define", args, children) ->
     let n = List.assoc "name" args in
     let s = List.map parse_set (take "Set" children) in
     let p = List.map parse_pred_group (List.filter (function | Element (n,_,_) -> n <> "Set" | _ -> false) children) in
     Define (n, s, p)
  | x -> parse_fail x

type local_Hyp = int * pred_group

let parse_local_hyp =
  function
  | Element ("Local_Hyp", args, x::_) ->
     let n = int_of_string @@ List.assoc "num" args in
     let s = parse_pred_group x in
     (n, s)
  | x -> parse_fail x

(* deprecated *)
type proof_State = string * string * string

let parse_proof_State =
  function
  | Element ("Proof_State", args, children) ->
     let pa = List.assoc "passList" args in
     let m = List.assoc "methodList" args in
     let pr = List.assoc "proofState" args in
     pa, m, pr
  | x -> parse_fail x

(* name of Goal * list of local hypothesis * list of goals * list of proof states *)
type simple_Goal = Simple_Goal of string * int list * pred_group list * proof_State list

let parse_simple_goal =
  function
  | Element ("Simple_Goal", args, x::children) ->
     let s =
       begin
         match x with
         | Element("Tag", args, [Text(s)]) -> s
         | x -> parse_fail x
       end in
     let lh =
       take "Ref_Hyp" children |>
         List.map @@
           function
           | Element (_,args,_) -> int_of_string @@ List.assoc "num" args
           | x -> parse_fail x
     in
     let g =
       take "Goal" children |>
         List.map @@
           function
           | Element (_,_, x::_) -> parse_pred_group x
           | x -> parse_fail x
     in
     let ps = List.map parse_proof_State (take "Proof_State" children) in
     Simple_Goal (s, lh, g, ps)
  | x -> parse_fail x

(* name of PO * list of names of definitions used * list of hypothesis * list of local hypothesis * list of goals *)
type proof_Obligation = PO of string * string list * pred_group list * local_Hyp list * simple_Goal list

let parse_proof_Obligation =
  function
  | Element ("Proof_Obligation", args, x::children) ->
     let s =
       begin
         match x with
         | Element("Tag", args, [Text(s)]) -> s
         | x -> parse_fail x
       end in
     let d =
       take "Definition" children
       |> List.map @@
            function
            | Element (_,args,_) -> List.assoc "name" args
            | x -> parse_fail x
     in
     let h =
       take "Hypothesis" children
       |> List.map @@
            function
            | Element (_,_,[x]) -> parse_pred_group x
            | x -> parse_fail x
     in
     let lh =
       take "Local_Hyp" children
       |> List.map parse_local_hyp
     in
     let sg =
       take "Simple_Goal" children
       |> List.map parse_simple_goal
     in
     PO (s, d, h, lh, sg)
  | x -> parse_fail x

type typeInfos = TI of (int * type_group) list

let parse_typeInfos =
  function
  | Element ("TypeInfos",args,children) ->
     let c = children
             |> List.map @@
                  function
                  | Element ("Type", args, [x]) ->
                     let i = int_of_string @@ List.assoc "id" args in
                     let t = parse_type_group x in
                     i, t
                  | x -> parse_fail x
     in
     TI c
  | x -> parse_fail x

type proof_Obligations = POs of define list * proof_Obligation list * typeInfos option

let parse_proof_Obligations =
  function
  | Element ("Proof_Obligations", args, children) ->
     let d = List.map parse_define @@ take "Define" children in
     let po = List.map parse_proof_Obligation @@ take "Proof_Obligation" children in
     let ti = List.map parse_typeInfos @@ take "TypeInfos" children in
     POs (d, po, match ti with [] -> None | x :: _ -> Some x)
  | x -> parse_fail x

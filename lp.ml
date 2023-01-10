(* Context:
   priority of symbols
   are symbols infix
   are symbols binders
 *)

type level = (float, int) Either.t

let level_to_string =
  function
  | Either.Left f -> string_of_float f
  | Either.Right n -> string_of_int n

let cmp_level l1 l2 =
  match l1, l2 with
  | Either.Left f1, Either.Left f2 -> Float.compare f1 f2
  | Either.Left f1, Either.Right n2 -> Float.compare f1 (float_of_int n2)
  | Either.Right n1, Either.Left f2 -> Float.compare (float_of_int n1) f2
  | Either.Right n1, Either.Right n2 -> Int.compare n1 n2

type id =
  | Uid of string
  | Qid of string * id

let rec id_to_string =
  function
  | Uid s -> s
  | Qid (s,i) -> s ^ "." ^ id_to_string i

type term =
  | Binder of id * (id * term option) list * term
  | Infixapp of level * bool option * term * id * term
  | Prefixapp of level * id * term
  | Postfixapp of level * term * id
  | App of term * (bool (* implicit? *) * term) list
  | Id of id
  | Underscore
  | Meta of id * term list
  | Pattern of id * term list

type priority =
  | FunPriority
  | AppPriority
  | FixPriority of level

(* If a term of priority p1 is the parent of a term of priority p2, should we parenthesize? (negative for no, positive for yes, 0 for same priority *)
let cmp_priority p1 p2 =
  match p1, p2 with
  | FunPriority, _ -> -1
  | AppPriority, _ -> 1
  | FixPriority _, (FunPriority | AppPriority) -> 1
  | FixPriority l1, FixPriority l2 -> cmp_level l1 l2

let term_to_string t =
  let rec aux ?(assoc = false) prior =
    let rec arg_to_string s =
      function
      | id, None -> s ^ " " ^ id_to_string id
      | id, Some t -> s ^ " (" ^ id_to_string id ^ " : " ^ aux FunPriority t ^ ")"
    in
    let app_to_string s =
      function
        | true, t -> s ^ " [" ^ aux FunPriority t ^ "]"
        | false, t -> s ^ " " ^ aux AppPriority t
    in
    let p trigger s =
      match cmp_priority prior trigger with
      | -1 -> s
      | 0 -> if assoc then s else "(" ^ s ^ ")"
      | _ -> "(" ^ s ^ ")"
    in
  function
  | Binder (i, l, t) -> p FunPriority (id_to_string i ^ (List.fold_left arg_to_string "" l) ^ ", " ^ aux FunPriority t)
  | Infixapp (l, b, t1, i, t2) ->
     let l = FixPriority l in
     let assoc1, assoc2 =
       match b with
         | None -> false, false
         | Some true -> true, false
         | Some false -> false, true
     in
     p l (aux ~assoc:assoc1 l t1 ^ " " ^ id_to_string i ^ " " ^ aux ~assoc:assoc2 l t2)
  | Prefixapp (l, i, t) ->
     let l = FixPriority l in
     p l (id_to_string i ^ " " ^ aux l t)
  | Postfixapp (l, t, i) ->
     let l = FixPriority l in
     p l (aux l t ^ " " ^ id_to_string i)
  | App (t, l) -> p AppPriority (aux AppPriority t ^ (List.fold_left app_to_string "" l))
  | Id i -> id_to_string i
  | Underscore -> "_"
  | Meta (i, l) -> "?" ^ (id_to_string i) ^ "[" ^ (String.concat "; " (List.map (aux FunPriority) l)) ^ "]"
  | Pattern (i, l) -> "$" ^ (id_to_string i) ^ "[" ^ (String.concat "; " (List.map (aux FunPriority) l)) ^ "]"
  in aux FunPriority t

(* true = left, false = right *)
type notation =
  | Infix of bool option * level
  | Postfix of level
  | Prefix of level
  | Quantifier

let notation_to_string =
  function
  | Infix (b, l) -> " infix " ^
                      begin
                        match b with
                          | None -> ""
                          | Some true -> "left "
                          | Some false -> "right "
                      end ^
                        level_to_string l
  | Postfix l -> " postfix " ^ level_to_string l
  | Prefix l -> " prefix " ^ level_to_string l
  | Quantifier -> " quantifier"

type modifier =
  | Associative of bool (* left? *)
  | Commutative
  | Constant
  | Injective
  | Opaque
  | Private
  | Protected
  | Sequential

let modifier_to_string =
  function
  | Associative b -> (if b then "left" else "right") ^ " associative "
  | Commutative -> "commutative "
  | Constant -> "constant "
  | Injective -> "injective "
  | Opaque -> "opaque "
  | Private -> "private "
  | Protected -> "protected "
  | Sequential -> "sequential "

type command =
  | Comment of string
  | Dependency of bool (* require? *) * bool (* open? *) * id
  | Symbol of modifier list * id * term option * term option
  | Rule of term * term
  | Notation of id * notation
(* I won't do builtins, inductives, unif_rule, coerce_rule, or queries *)

let command_to_string =
  function
  | Comment s -> "/* " ^ s ^ " */"
  | Dependency (b1, b2, i) ->
     let s1 = if b1 then "require " else "" in
     let s2 = if b2 then "open " else "" in
     let s3 = id_to_string i in
     s1 ^ s2 ^ s3 ^ ";\n"
  | Symbol (l, i, t1, t2) ->
     let s1 = String.concat "" (List.map modifier_to_string l) in
     let s2 = id_to_string i in
     let s3 = match t1 with
       | Some t -> " : " ^ term_to_string t
       | None -> ""
     in
     let s4 = match t2 with
       | Some t -> " ≔ " ^ term_to_string t
       | None -> ""
     in s1 ^ s2 ^ s3 ^ s4 ^ ";\n"
  | Rule (t1, t2) ->
     "rule " ^ term_to_string t1 ^ " ↪ " ^ term_to_string t2 ^ ";\n"
  | Notation (i, n) -> "notation " ^ id_to_string i ^ notation_to_string n ^ ";\n"

(* lambdapi code *)
type lp = command list

(* directly print to a channel *)
let print_lp out lp = List.iter (fun c -> Out_channel.output_string out (command_to_string c)) lp

(* Translation of the signature of syntax.lp into OCaml *)

let implicit x l =
  match x with
  | None -> l
  | Some x -> (true, x) :: l

let binary_op level is_left name x y = Lp.Infixapp (Either.Right level, is_left, x, Lp.Uid name, y)
let app x l = Lp.App(x,l)

let id = Lp.Id (Lp.Uid "ID")

let u = Lp.Id (Lp.Uid "U")
let thm x = app (Lp.Id (Lp.Uid "Thm")) [(false, x)]

let set x = app (Lp.Id (Lp.Uid "Set")) [(false, x)]
let ( * ) = binary_op 1 (Some true) "*"

let z_t = Lp.Id (Lp.Uid "Z_T")
let integer = Lp.Id (Lp.Uid "INTEGER")
let r_t = Lp.Id (Lp.Uid "R_T")
let real = Lp.Id (Lp.Uid "REAL")
let float_t = Lp.Id (Lp.Uid "FLOAT_T")
let float = Lp.Id (Lp.Uid "FLOAT")
let bool_t = Lp.Id (Lp.Uid "BOOL_T")
let bool = Lp.Id (Lp.Uid "BOOL")
let string_t = Lp.Id (Lp.Uid "STRING_T")
let string = Lp.Id (Lp.Uid "STRING")

let type_t x = app (Lp.Id (Lp.Uid "type_T")) [(false, x)]

let list x = app (Lp.Id (Lp.Uid "list")) [(false, x)]
let nil ?(t=None) () = app (Lp.Id (Lp.Uid "nil")) (implicit t [])
let cons ?(t=None) x y = app (Lp.Id (Lp.Uid "cons")) @@ implicit t [(false, x); (false, y)]
let typlist = Lp.Id (Lp.Uid "typlist")
let typnil x = app (Lp.Id (Lp.Uid "typnil")) [(false, x)]
let typcons x y = app (Lp.Id (Lp.Uid "typcons")) [(false, x); (false, y)]
let curryt x y = app (Lp.Id (Lp.Uid "curryT")) [(false, x); (false, y)]

let curryu x = app (Lp.Id (Lp.Uid "curryU")) [(false, x)]

let to_t x = app (Lp.Id (Lp.Uid "to_T")) [(false, x)]

let struct_sig = Lp.Id (Lp.Uid "struct_sig")
let struct_nil x y = app (Lp.Id (Lp.Uid "struct_nil")) [(false, x); (false, y)]
let struct_cons x y z = app (Lp.Id (Lp.Uid "struct_cons")) [(false, x); (false, y); (false, z)]
let struct_t x y = app (Lp.Id (Lp.Uid "to_T")) [(false, x); (false, y)]

let record_sig = Lp.Id (Lp.Uid "record_sig")
let record_nil ?(t=None) x y = app (Lp.Id (Lp.Uid "record_nil")) @@ implicit t [(false, x); (false, y)]
let record_cons ?(t=None) x y = app (Lp.Id (Lp.Uid "record_cons")) @@ implicit t [(false, x); (false, y)]

let struct_type x = app (Lp.Id (Lp.Uid "struct_type")) [(false, x)]

let record_type x = app (Lp.Id (Lp.Uid "record_type")) [(false, x)]

let accessible x y z = app (Lp.Id (Lp.Uid "accessible")) [(false, x); (false, y); (false, z)]
let accessible_nil ?(id=None) ?(t=None) () = app (Lp.Id (Lp.Uid "accessible_nil")) @@ implicit id @@ implicit t []
let accessible_cons ?(id=None) ?(t=None) ?(s=None) () = app (Lp.Id (Lp.Uid "accessible_cons")) @@ implicit id @@ implicit t @@ implicit s []
let skip ?(id1=None) ?(t1=None) ?(id2=None) ?(t2=None) ?(s=None) x = app (Lp.Id (Lp.Uid "skip")) @@ implicit id1 @@ implicit t1 @@ implicit id2 @@ implicit t2 @@ implicit s [(false, x)]

let trueu = Lp.Uid "TRUE"
let falseu = Lp.Uid "FALSE"
let imply = binary_op 1 (Some false) "⇒"
let equiv = binary_op 2 (Some false) "⇔"
let disj = binary_op 3 (Some false) "∨"
let conj = binary_op 4 (Some false) "∧"
let not x = app (Lp.Id (Lp.Uid "¬")) [(false, x)]

let forall ?(t=None) x body =
  match body with
  | Lp.Binder (Lp.Uid "`∀", l, body) -> Lp.Binder (Lp.Uid "`∀", (x,t) :: l, body)
  | _ -> Binder (Lp.Uid "`∀", [(x,t)], body)
let exists ?(t=None) x body =
  match body with
  | Lp.Binder (Lp.Uid "`∃", l, body) -> Lp.Binder (Lp.Uid "`∃", (x,t) :: l, body)
  | _ -> Lp.Binder (Lp.Uid "`∃", [(x,t)], body)

let belong = binary_op 15 (Some false) "∈"
let notbelong = binary_op 15 (Some false) "notin"
let included = binary_op 15 (Some false) "⊆"
let notincluded = binary_op 15 (Some false) "notincluded"
let strictincluded = binary_op 15 (Some false) "⊂"
let notstrictincluded = binary_op 15 (Some false) "notstrictincluded"
let equal = binary_op 15 (Some false) "="
let notequal = binary_op 15 (Some false) "≠"
let igeq = binary_op 15 (Some false) "≥i"
let igreater = binary_op 15 (Some false) ">i"
let ismaller = binary_op 15 (Some false) "<i"
let ileq = binary_op 15 (Some false) "≤i"
let rgeq = binary_op 15 (Some false) "≥r"
let rgreater = binary_op 15 (Some false) ">r"
let rsmaller = binary_op 15 (Some false) "<r"
let rleq = binary_op 15 (Some false) "≤r"
let fgeq = binary_op 15 (Some false) "≥f"
let fgreater = binary_op 15 (Some false) ">f"
let fsmaller = binary_op 15 (Some false) "<f"
let fleq = binary_op 15 (Some false) "≤f"

let imax x = app (Lp.Id (Lp.Uid "imax")) [(false, x)]
let rmax x = app (Lp.Id (Lp.Uid "rmax")) [(false, x)]
let imin x = app (Lp.Id (Lp.Uid "imin")) [(false, x)]
let rmin x = app (Lp.Id (Lp.Uid "rmin")) [(false, x)]
let card x = app (Lp.Id (Lp.Uid "card")) [(false, x)]
let dom x = app (Lp.Id (Lp.Uid "dom")) [(false, x)]
let ran x = app (Lp.Id (Lp.Uid "ran")) [(false, x)]
let pow x = app (Lp.Id (Lp.Uid "pow")) [(false, x)]
let pow1 x = app (Lp.Id (Lp.Uid "pow1")) [(false, x)]
let fin x = app (Lp.Id (Lp.Uid "fin")) [(false, x)]
let fin1 x = app (Lp.Id (Lp.Uid "fin1")) [(false, x)]
let union_gen x = app (Lp.Id (Lp.Uid "union_gen")) [(false, x)]
let inter_gen x = app (Lp.Id (Lp.Uid "inter_gen")) [(false, x)]
let seq x = app (Lp.Id (Lp.Uid "seq")) [(false, x)]
let seq1 x = app (Lp.Id (Lp.Uid "seq1")) [(false, x)]
let iseq x = app (Lp.Id (Lp.Uid "iseq")) [(false, x)]
let iseq1 x = app (Lp.Id (Lp.Uid "iseq1")) [(false, x)]
let minusi x = app (Lp.Id (Lp.Uid "minusi")) [(false, x)]
let minusr x = app (Lp.Id (Lp.Uid "minusr")) [(false, x)]
let inversion x = app (Lp.Id (Lp.Uid "~")) [(false, x)]
let size x = app (Lp.Id (Lp.Uid "size")) [(false, x)]
let perm x = app (Lp.Id (Lp.Uid "perm")) [(false, x)]
let first x = app (Lp.Id (Lp.Uid "first")) [(false, x)]
let last x = app (Lp.Id (Lp.Uid "last")) [(false, x)]
let identity x = app (Lp.Id (Lp.Uid "id")) [(false, x)]
let closure x = app (Lp.Id (Lp.Uid "closure")) [(false, x)]
let closure1 x = app (Lp.Id (Lp.Uid "closure1")) [(false, x)]
let tail x = app (Lp.Id (Lp.Uid "tail")) [(false, x)]
let front x = app (Lp.Id (Lp.Uid "front")) [(false, x)]
let rev x = app (Lp.Id (Lp.Uid "rev")) [(false, x)]
let conc x = app (Lp.Id (Lp.Uid "conc")) [(false, x)]
let succ x = app (Lp.Id (Lp.Uid "succ")) [(false, x)]
let pred x = app (Lp.Id (Lp.Uid "pred")) [(false, x)]
let rel x = app (Lp.Id (Lp.Uid "rel")) [(false, x)]
let fnc x = app (Lp.Id (Lp.Uid "fnc")) [(false, x)]
let real x = app (Lp.Id (Lp.Uid "real")) [(false, x)]
let floor x = app (Lp.Id (Lp.Uid "floor")) [(false, x)]
let ceiling x = app (Lp.Id (Lp.Uid "ceiling")) [(false, x)]
let tree x = app (Lp.Id (Lp.Uid "tree")) [(false, x)]
let btree x = app (Lp.Id (Lp.Uid "btree")) [(false, x)]
let top x = app (Lp.Id (Lp.Uid "top")) [(false, x)]
let sons x = app (Lp.Id (Lp.Uid "sons")) [(false, x)]
let pref x = app (Lp.Id (Lp.Uid "pref")) [(false, x)]
let post x = app (Lp.Id (Lp.Uid "post")) [(false, x)]
let sizet x = app (Lp.Id (Lp.Uid "sizet")) [(false, x)]
let mirror x = app (Lp.Id (Lp.Uid "mirror")) [(false, x)]
let left_tree x = app (Lp.Id (Lp.Uid "left_tree")) [(false, x)]
let right_tree x = app (Lp.Id (Lp.Uid "right_tree")) [(false, x)]
let infix_tree x = app (Lp.Id (Lp.Uid "infix_tree")) [(false, x)]
let bin_unary x = app (Lp.Id (Lp.Uid "bin_unary")) [(false, x)]

let pair x y = app (Lp.Id (Lp.Uid "pair")) [(false, x); (false, y)]
let iprod = binary_op 190 (Some true) "*_i"
let rprod = binary_op 190 (Some true) "*_r"
let fprod = binary_op 190 (Some true) "*_f"
let sprod = binary_op 190 (Some true) "*_s"
let iexp = binary_op 200 (Some true) "**i"
let rexp = binary_op 200 (Some true) "**r"
let iplus = binary_op 180 (Some true) "+_i"
let rplus = binary_op 180 (Some true) "+_r"
let fplus = binary_op 180 (Some true) "+_f"
let partial = binary_op 125 (Some true) "+->"
let partialsurj = binary_op 125 (Some true) "+->>"
let iminus = binary_op 180 (Some true) "-_i"
let rminus = binary_op 180 (Some true) "-_r"
let fminus = binary_op 180 (Some true) "-_f"
let sminus = binary_op 180 (Some true) "-_s"
let total = binary_op 125 (Some true) "-->"
let totalsurj = binary_op 125 (Some true) "-->>"
let headinsert = binary_op 160 (Some true) "->"
let interval = binary_op 170 (Some true) "--"
let idiv = binary_op 190 (Some true) "div_i"
let rdiv = binary_op 190 (Some true) "div_r"
let fdiv = binary_op 190 (Some true) "div_f"
let inter = binary_op 160 (Some true) "∩"
let headrestrict x y = app (Lp.Id (Lp.Uid "headrestrict")) [(false, x); (false, y)]
let compose x y = app (Lp.Id (Lp.Uid "comp")) [(false, x); (false, y)]
let overload = binary_op 160 (Some true) "<+"
let relation = binary_op 125 (Some true) "<->"
let tailinsert = binary_op 160 (Some true) "<-"
let domsubtract x y = app (Lp.Id (Lp.Uid "dom-")) [(false, x); (false, y)]
let domrestrict x y = app (Lp.Id (Lp.Uid "domrestrict")) [(false, x); (false, y)]
let partialinject = binary_op 125 (Some true) ">+>"
let totalbiject = binary_op 125 (Some true) ">->"
let directprod = binary_op 125 (Some true) ">->>"
let parallelprod = binary_op 160 (Some true) "><"
let union = binary_op 160 (Some true) "∪"
let tailrestrict x y = app (Lp.Id (Lp.Uid "tailrestrict")) [(false, x); (false, y)]
let concat = binary_op 160 (Some true) "^"
let ( mod ) = binary_op 190 (Some true) "mod"
let map = binary_op 160 (Some true) "map"
let imagerestrict x y = app (Lp.Id (Lp.Uid "imagerestrict")) [(false, x); (false, y)]
let imagesubtract x y = app (Lp.Id (Lp.Uid "image-")) [(false, x); (false, y)]
let image x y = app (Lp.Id (Lp.Uid "image")) [(false, x); (false, y)]
let eval x y = app (Lp.Id (Lp.Uid "eval")) [(false, x); (false, y)]
let prj1 x y = app (Lp.Id (Lp.Uid "prj1")) [(false, x); (false, y)]
let prj2 x y = app (Lp.Id (Lp.Uid "prj2")) [(false, x); (false, y)]
let iterate x y = app (Lp.Id (Lp.Uid "iterate")) [(false, x); (false, y)]
let const x y = app (Lp.Id (Lp.Uid "const")) [(false, x); (false, y)]
let rank x y = app (Lp.Id (Lp.Uid "rank")) [(false, x); (false, y)]
let father x y = app (Lp.Id (Lp.Uid "father")) [(false, x); (false, y)]
let subtree x y = app (Lp.Id (Lp.Uid "subtree")) [(false, x); (false, y)]
let arity x y = app (Lp.Id (Lp.Uid "arity")) [(false, x); (false, y)]

let son x y z = app (Lp.Id (Lp.Uid "son")) [(false, x); (false, y); (false, z)]
let bin_ternary x y z = app (Lp.Id (Lp.Uid "bin_ternary")) [(false, x); (false, y); (false, z)]

let sequence x = app (Lp.Id (Lp.Uid "sequence")) [(false, x)]
let extension x = app (Lp.Id (Lp.Uid "extension")) [(false, x)]

let lambda x y = app (Lp.Id (Lp.Uid "%")) [(false, x); (false, y)]
let isigma x y = app (Lp.Id (Lp.Uid "iSIGMA")) [(false, x); (false, y)]
let rsigma x y = app (Lp.Id (Lp.Uid "rSIGMA")) [(false, x); (false, y)]
let ipi x y = app (Lp.Id (Lp.Uid "iPI")) [(false, x); (false, y)]
let rpi x y = app (Lp.Id (Lp.Uid "rPI")) [(false, x); (false, y)]
let inter' x y = app (Lp.Id (Lp.Uid "INTER")) [(false, x); (false, y)]
let union' x y = app (Lp.Id (Lp.Uid "UNION")) [(false, x); (false, y)]
let comprehension x = app (Lp.Id (Lp.Uid "comprehension")) [(false, x)]

let trueb = Lp.Uid "true"
let falseb = Lp.Uid "false"
let boolean_exp x = app (Lp.Id (Lp.Uid "Boolean_Exp")) [(false, x)]

let emptyset x = app (Lp.Id (Lp.Uid "emptyset")) [(false, x)]
let emptyseq x = app (Lp.Id (Lp.Uid "emptyseq")) [(false, x)]
let cst x y = app (Lp.Id (Lp.Uid "cst")) [(false, x); (false, y)]

let pos_int = Lp.Uid "pos_int"
let xh = Lp.Uid "xH"
let x0 x = app (Lp.Id (Lp.Uid "x0")) [(false, x)]
let x1 x = app (Lp.Id (Lp.Uid "x1")) [(false, x)]
let zero = Lp.Uid "0"
let zpos x = app (Lp.Id (Lp.Uid "Zpos")) [(false, x)]
let zneg x = app (Lp.Id (Lp.Uid "Zneg")) [(false, x)]

let struct_exp x = app (Lp.Id (Lp.Uid "struct")) [(false, x)]
let record x = app (Lp.Id (Lp.Uid "record")) [(false, x)]
let record_update x y z t = app (Lp.Id (Lp.Uid "record")) [(false, x); (false, y); (false, z); (false, t)]
let record_field_access x y z = app (Lp.Id (Lp.Uid "record")) [(false, x); (false, y); (false, z)]

(* Translation of the signature of syntax.lp into OCaml *)

let implicit x l =
  match x with
  | None -> l
  | Some x -> (true, x) :: l

let binary_op level is_left name x y = Lp.Infixapp (Either.Right level, is_left, x, Lp.Uid name, y)
let app x l = Lp.App(x,l)
let lp_id x = Lp.Id (Lp.Uid x)

let requireme = Lp.Dependency (true, true, Qid("B", Uid("syntax")))

(****************************************************************************************************)

let id = lp_id "ID"

let u = lp_id "U"
let thm x = app (lp_id "Thm") [(false, x)]

let t = lp_id "T"
let tau x = app (lp_id "τ") [(false, x)]

let set x = app (lp_id "Set") [(false, x)]
let ( * ) = binary_op 1 (Some true) "*"

let z_t = lp_id "Z_T"
let integer = lp_id "INTEGER"
let r_t = lp_id "R_T"
let real = lp_id "REAL"
let float_t = lp_id "FLOAT_T"
let float = lp_id "FLOAT"
let bool_t = lp_id "BOOL_T"
let bool = lp_id "BOOL"
let string_t = lp_id "STRING_T"
let string = lp_id "STRING"

let type_t x = app (lp_id "type_T") [(false, x)]

let list x = app (lp_id "list") [(false, x)]
let nil ?(t=None) () = app (lp_id "nil") (implicit t [])
let cons ?(t=None) x y = app (lp_id "cons") @@ implicit t [(false, x); (false, y)]
let typlist = lp_id "typlist"
let typnil x = app (lp_id "typnil") [(false, x)]
let typcons x y = app (lp_id "typcons") [(false, x); (false, y)]
let curryt x y = app (lp_id "curryT") [(false, x); (false, y)]

let curryu x = app (lp_id "curryU") [(false, x)]

let to_t x = app (lp_id "to_T") [(false, x)]

let struct_sig = lp_id "struct_sig"
let struct_nil x y = app (lp_id "struct_nil") [(false, x); (false, y)]
let struct_cons x y z = app (lp_id "struct_cons") [(false, x); (false, y); (false, z)]
let struct_t x y = app (lp_id "to_T") [(false, x); (false, y)]

let record_sig = lp_id "record_sig"
let record_nil ?(t=None) x y = app (lp_id "record_nil") @@ implicit t [(false, x); (false, y)]
let record_cons ?(t=None) x y = app (lp_id "record_cons") @@ implicit t [(false, x); (false, y)]

let struct_type x = app (lp_id "struct_type") [(false, x)]

let record_type x = app (lp_id "record_type") [(false, x)]

let accessible x y z = app (lp_id "accessible") [(false, x); (false, y); (false, z)]
let accessible_nil ?(id=None) ?(t=None) () = app (lp_id "accessible_nil") @@ implicit id @@ implicit t []
let accessible_cons ?(id=None) ?(t=None) ?(s=None) () = app (lp_id "accessible_cons") @@ implicit id @@ implicit t @@ implicit s []
let skip ?(id1=None) ?(t1=None) ?(id2=None) ?(t2=None) ?(s=None) x = app (lp_id "skip") @@ implicit id1 @@ implicit t1 @@ implicit id2 @@ implicit t2 @@ implicit s [(false, x)]

let trueu = lp_id "TRUE"
let falseu = lp_id "FALSE"
let imply = binary_op 1 (Some false) "⇒"
let equiv = binary_op 2 (Some false) "⇔"
let disj = binary_op 3 (Some false) "∨"
let conj = binary_op 4 (Some false) "∧"
let not x = app (lp_id "¬") [(false, x)]

let forall x t body =
  Lp.Binder (Lp.Uid "`∀", [(x,Some t)], body)
let exists x t body =
  Lp.Binder (Lp.Uid "`∃", [(x,Some t)], body)

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

let imax x = app (lp_id "imax") [(false, x)]
let rmax x = app (lp_id "rmax") [(false, x)]
let imin x = app (lp_id "imin") [(false, x)]
let rmin x = app (lp_id "rmin") [(false, x)]
let card x = app (lp_id "card") [(false, x)]
let dom x = app (lp_id "dom") [(false, x)]
let ran x = app (lp_id "ran") [(false, x)]
let pow x = app (lp_id "pow") [(false, x)]
let pow1 x = app (lp_id "pow1") [(false, x)]
let fin x = app (lp_id "fin") [(false, x)]
let fin1 x = app (lp_id "fin1") [(false, x)]
let union_gen x = app (lp_id "union_gen") [(false, x)]
let inter_gen x = app (lp_id "inter_gen") [(false, x)]
let seq x = app (lp_id "seq") [(false, x)]
let seq1 x = app (lp_id "seq1") [(false, x)]
let iseq x = app (lp_id "iseq") [(false, x)]
let iseq1 x = app (lp_id "iseq1") [(false, x)]
let minusi x = app (lp_id "minusi") [(false, x)]
let minusr x = app (lp_id "minusr") [(false, x)]
let inversion x = app (lp_id "~") [(false, x)]
let size x = app (lp_id "size") [(false, x)]
let perm x = app (lp_id "perm") [(false, x)]
let first x = app (lp_id "first") [(false, x)]
let last x = app (lp_id "last") [(false, x)]
let identity x = app (lp_id "id") [(false, x)]
let closure x = app (lp_id "closure") [(false, x)]
let closure1 x = app (lp_id "closure1") [(false, x)]
let tail x = app (lp_id "tail") [(false, x)]
let front x = app (lp_id "front") [(false, x)]
let rev x = app (lp_id "rev") [(false, x)]
let conc x = app (lp_id "conc") [(false, x)]
let succ x = app (lp_id "succ") [(false, x)]
let pred x = app (lp_id "pred") [(false, x)]
let rel x = app (lp_id "rel") [(false, x)]
let fnc x = app (lp_id "fnc") [(false, x)]
let real x = app (lp_id "real") [(false, x)]
let floor x = app (lp_id "floor") [(false, x)]
let ceiling x = app (lp_id "ceiling") [(false, x)]
let tree x = app (lp_id "tree") [(false, x)]
let btree x = app (lp_id "btree") [(false, x)]
let top x = app (lp_id "top") [(false, x)]
let sons x = app (lp_id "sons") [(false, x)]
let pref x = app (lp_id "pref") [(false, x)]
let post x = app (lp_id "post") [(false, x)]
let sizet x = app (lp_id "sizet") [(false, x)]
let mirror x = app (lp_id "mirror") [(false, x)]
let left_tree x = app (lp_id "left_tree") [(false, x)]
let right_tree x = app (lp_id "right_tree") [(false, x)]
let infix_tree x = app (lp_id "infix_tree") [(false, x)]
let bin_unary x = app (lp_id "bin_unary") [(false, x)]

let pair x y = app (lp_id "pair") [(false, x); (false, y)]
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
let headrestrict x y = app (lp_id "headrestrict") [(false, x); (false, y)]
let compose x y = app (lp_id "comp") [(false, x); (false, y)]
let overload = binary_op 160 (Some true) "<+"
let relation = binary_op 125 (Some true) "<->"
let tailinsert = binary_op 160 (Some true) "<-"
let domsubtract x y = app (lp_id "dom-") [(false, x); (false, y)]
let domrestrict x y = app (lp_id "domrestrict") [(false, x); (false, y)]
let partialinject = binary_op 125 (Some true) ">+>"
let totalinject = binary_op 125 (Some true) ">->"
let totalbiject = binary_op 125 (Some true) ">->>"
let directprod = binary_op 160 (Some true) "><"
let parallelprod = binary_op 160 (Some true) "par"
let union = binary_op 160 (Some true) "∪"
let tailrestrict x y = app (lp_id "tailrestrict") [(false, x); (false, y)]
let concat = binary_op 160 (Some true) "^"
let ( mod ) = binary_op 190 (Some true) "mod"
let map = binary_op 160 (Some true) "map"
let imagerestrict x y = app (lp_id "imagerestrict") [(false, x); (false, y)]
let imagesubtract x y = app (lp_id "image-") [(false, x); (false, y)]
let image x y = app (lp_id "image") [(false, x); (false, y)]
let eval x y = app (lp_id "eval") [(false, x); (false, y)]
let prj1 x y = app (lp_id "prj1") [(false, x); (false, y)]
let prj2 x y = app (lp_id "prj2") [(false, x); (false, y)]
let iterate x y = app (lp_id "iterate") [(false, x); (false, y)]
let const x y = app (lp_id "const") [(false, x); (false, y)]
let rank x y = app (lp_id "rank") [(false, x); (false, y)]
let father x y = app (lp_id "father") [(false, x); (false, y)]
let subtree x y = app (lp_id "subtree") [(false, x); (false, y)]
let arity x y = app (lp_id "arity") [(false, x); (false, y)]

let son x y z = app (lp_id "son") [(false, x); (false, y); (false, z)]
let bin_ternary x y z = app (lp_id "bin_ternary") [(false, x); (false, y); (false, z)]

let sequence x = app (lp_id "sequence") [(false, x)]
let extension x = app (lp_id "extension") [(false, x)]

let lambda h x y = app (lp_id "%") [(true, h); (false, x); (false, y)]
let isigma h x y = app (lp_id "iSIGMA") [(true, h); (false, x); (false, y)]
let rsigma h x y = app (lp_id "rSIGMA") [(true, h); (false, x); (false, y)]
let ipi h x y = app (lp_id "iPI") [(true, h); (false, x); (false, y)]
let rpi h x y = app (lp_id "rPI") [(true, h); (false, x); (false, y)]
let inter' h x y = app (lp_id "INTER") [(true, h); (false, x); (false, y)]
let union' h x y = app (lp_id "UNION") [(true, h); (false, x); (false, y)]
let comprehension h x = app (lp_id "comprehension") [(true, h); (false, x)]

let trueb = lp_id "true"
let falseb = lp_id "false"
let boolean_exp x = app (lp_id "Boolean_Exp") [(false, x)]

let emptyset x = app (lp_id "emptyset") [(false, x)]
let emptyseq x = app (lp_id "emptyseq") [(false, x)]
let cst x y = app (lp_id "cst") [(false, x); (false, y)]

let pos_int = lp_id "pos_int"
let xh = lp_id "xH"
let x0 x = app (lp_id "x0") [(false, x)]
let x1 x = app (lp_id "x1") [(false, x)]
let zero = lp_id "0"
let zpos x = app (lp_id "Zpos") [(false, x)]
let zneg x = app (lp_id "Zneg") [(false, x)]

let struct_exp x = app (lp_id "struct") [(false, x)]
let record x = app (lp_id "record") [(false, x)]
let record_update x y z t = app (lp_id "record") [(false, x); (false, y); (false, z); (false, t)]
let record_field_access x y z = app (lp_id "record") [(false, x); (false, y); (false, z)]

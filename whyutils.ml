open Why3

(* let config = Whyconf.init_config None *)
let (config, _) = Whyconf.Args.complete_initialization ()
let main = Whyconf.get_main config
let main = Whyconf.set_loadpath main ["."]
let env = Env.create_env (Whyconf.loadpath main)

let bool_theory = Env.read_theory env ["bool"] "Bool"
let int_theory = Env.read_theory env ["int"] "Int"
let real_theory = Env.read_theory env ["real"] "Real"
let powerreal_theory = Env.read_theory env ["real"] "PowerReal"
let realinfix_theory = Env.read_theory env ["real"] "RealInfix"
let fromint_theory = Env.read_theory env ["real"] "FromInt"
let truncate_theory = Env.read_theory env ["real"] "Truncate"
let power_theory = Env.read_theory env ["int"] "Power"
let computerdivision_theory = Env.read_theory env ["int"] "ComputerDivision"

let bbool_theory = Env.read_theory env ["bwhy_prelude"] "B_BOOL"
let interval_theory = Env.read_theory env ["bwhy_prelude"] "Interval"
let powerset_theory = Env.read_theory env ["bwhy_prelude"] "PowerSet"
let relation_theory = Env.read_theory env ["bwhy_prelude"] "Relation"
let image_theory = Env.read_theory env ["bwhy_prelude"] "Image"
let identity_theory = Env.read_theory env ["bwhy_prelude"] "Identity"
let inversedomran_theory = Env.read_theory env ["bwhy_prelude"] "InverseDomRan"
let function_theory = Env.read_theory env ["bwhy_prelude"] "Function"
let sequence_theory = Env.read_theory env ["bwhy_prelude"] "Sequence"
let isfinite_theory = Env.read_theory env ["bwhy_prelude"] "IsFinite"
let powerrelation_theory = Env.read_theory env ["bwhy_prelude"] "PowerRelation"
let restriction_theory = Env.read_theory env ["bwhy_prelude"] "Restriction"
let overriding_theory = Env.read_theory env ["bwhy_prelude"] "Overriding"
let composition_theory = Env.read_theory env ["bwhy_prelude"] "Composition"
let blist_theory = Env.read_theory env ["bwhy_prelude"] "BList"
let minmax_theory = Env.read_theory env ["bwhy_prelude"] "MinMax"
let projection_theory = Env.read_theory env ["bwhy_prelude"] "Projection"
let iteration_theory = Env.read_theory env ["bwhy_prelude"] "Iteration"
let generalized_theory = Env.read_theory env ["bwhy_prelude"] "Generalized"
let sumsigma_theory = Env.read_theory env ["bwhy_prelude"] "SumSigma"

(* theory Set *)
let set_theory = Env.read_theory env ["set"] "Set"
let set = Theory.ns_find_ts set_theory.Theory.th_export ["set"]
let mem = Theory.ns_find_ls set_theory.Theory.th_export ["mem"]
let equal = Theory.ns_find_ls set_theory.Theory.th_export ["infix =="]
let subset = Theory.ns_find_ls set_theory.Theory.th_export ["subset"]
let is_empty = Theory.ns_find_ls set_theory.Theory.th_export ["is_empty"]
let empty = Theory.ns_find_ls set_theory.Theory.th_export ["empty"]
let all = Theory.ns_find_ls set_theory.Theory.th_export ["all"]
let add = Theory.ns_find_ls set_theory.Theory.th_export ["add"]
let singleton = Theory.ns_find_ls set_theory.Theory.th_export ["singleton"]
let remove = Theory.ns_find_ls set_theory.Theory.th_export ["remove"]
let union = Theory.ns_find_ls set_theory.Theory.th_export ["union"]
let inter = Theory.ns_find_ls set_theory.Theory.th_export ["inter"]
let diff = Theory.ns_find_ls set_theory.Theory.th_export ["diff"]
let complement = Theory.ns_find_ls set_theory.Theory.th_export ["complement"]
let pick = Theory.ns_find_ls set_theory.Theory.th_export ["pick"]
let disjoint = Theory.ns_find_ls set_theory.Theory.th_export ["disjoint"]
let product = Theory.ns_find_ls set_theory.Theory.th_export ["product"]
let filter = Theory.ns_find_ls set_theory.Theory.th_export ["filter"]
let map = Theory.ns_find_ls set_theory.Theory.th_export ["map"]

(* theory Bool *)
let andb = Theory.ns_find_ls bool_theory.Theory.th_export ["andb"]
let orb = Theory.ns_find_ls bool_theory.Theory.th_export ["orb"]
let notb = Theory.ns_find_ls bool_theory.Theory.th_export ["notb"]
let xorb = Theory.ns_find_ls bool_theory.Theory.th_export ["xorb"]
let implb = Theory.ns_find_ls bool_theory.Theory.th_export ["implb"]

(* theory Int *)
(* let zero = Theory.ns_find_ls int_theory.Theory.th_export ["zero"] *)
(* let one = Theory.ns_find_ls int_theory.Theory.th_export ["one"] *)
let uminusi = Theory.ns_find_ls int_theory.Theory.th_export ["prefix -"]
let plusi = Theory.ns_find_ls int_theory.Theory.th_export ["infix +"]
let muli = Theory.ns_find_ls int_theory.Theory.th_export ["infix *"]
let lei = Theory.ns_find_ls int_theory.Theory.th_export ["infix <"]
let minusi = Theory.ns_find_ls int_theory.Theory.th_export ["infix -"]
let gei = Theory.ns_find_ls int_theory.Theory.th_export ["infix >"]
let leqi = Theory.ns_find_ls int_theory.Theory.th_export ["infix <="]
let geqi = Theory.ns_find_ls int_theory.Theory.th_export ["infix >="]

(* theory PowerReal *)
let pow = Theory.ns_find_ls powerreal_theory.Theory.th_export ["pow"]

(* theory RealInfix *)
let uminusr = Theory.ns_find_ls realinfix_theory.Theory.th_export ["prefix -."]
let plusr = Theory.ns_find_ls realinfix_theory.Theory.th_export ["infix +."]
let mulr = Theory.ns_find_ls realinfix_theory.Theory.th_export ["infix *."]
let ler = Theory.ns_find_ls realinfix_theory.Theory.th_export ["infix <."]
let minusr = Theory.ns_find_ls realinfix_theory.Theory.th_export ["infix -."]
let ger = Theory.ns_find_ls realinfix_theory.Theory.th_export ["infix >."]
let leqr = Theory.ns_find_ls realinfix_theory.Theory.th_export ["infix <=."]
let geqr = Theory.ns_find_ls realinfix_theory.Theory.th_export ["infix >=."]
let divr = Theory.ns_find_ls realinfix_theory.Theory.th_export ["infix /."]

(* theory FromInt *)
let from_int = Theory.ns_find_ls fromint_theory.Theory.th_export ["from_int"]

(* theory Truncate *)
let floor = Theory.ns_find_ls truncate_theory.Theory.th_export ["floor"]
let ceil = Theory.ns_find_ls truncate_theory.Theory.th_export ["ceil"]

(* theory Power *)
let power_int = Theory.ns_find_ls power_theory.Theory.th_export ["power"]

(* theory ComputerDivision *)
let div = Theory.ns_find_ls computerdivision_theory.Theory.th_export ["div"]
let ( mod ) = Theory.ns_find_ls computerdivision_theory.Theory.th_export ["mod"]

(* theory B_Bool *)
let b_bool = Theory.ns_find_ls bbool_theory.Theory.th_export ["b_bool"]

(* theory Interval *)
(* let string = Theory.ns_find_ls interval_theory.Theory.th_export ["string"] (* ??? *) *)
let integer = Theory.ns_find_ls interval_theory.Theory.th_export ["integer"]
let natural = Theory.ns_find_ls interval_theory.Theory.th_export ["natural"]
let bounded_int = Theory.ns_find_ls interval_theory.Theory.th_export ["bounded_int"]
let b_maxint32_value = Theory.ns_find_ls interval_theory.Theory.th_export ["b_maxint32_value"]
let b_minint32_value = Theory.ns_find_ls interval_theory.Theory.th_export ["b_maxint32_value"]
let natural1 = Theory.ns_find_ls interval_theory.Theory.th_export ["natural1"]
let nat = Theory.ns_find_ls interval_theory.Theory.th_export ["nat"]
let nat1 = Theory.ns_find_ls interval_theory.Theory.th_export ["nat1"]
let interval = Theory.ns_find_ls interval_theory.Theory.th_export ["mk"]
(* let apply = Theory.ns_find_ls function_theory.Theory.th_export ["apply"] *)

(* theory PowerSet *)

let power = Theory.ns_find_ls powerset_theory.Theory.th_export ["power"]
let non_empty_power = Theory.ns_find_ls powerset_theory.Theory.th_export ["non_empty_power"]

(* theory Relations *)

let relation = Theory.ns_find_ls relation_theory.Theory.th_export ["relation"]

(* theory Composition *)
let semicolon = Theory.ns_find_ls composition_theory.Theory.th_export ["semicolon"]
let direct_product = Theory.ns_find_ls composition_theory.Theory.th_export ["direct_product"]
let parallel_product = Theory.ns_find_ls composition_theory.Theory.th_export ["parallel_product"]

(* theory InverseDomRan *)
let inverse = Theory.ns_find_ls inversedomran_theory.Theory.th_export ["inverse"]
let dom = Theory.ns_find_ls inversedomran_theory.Theory.th_export ["dom"]
let ran = Theory.ns_find_ls inversedomran_theory.Theory.th_export ["ran"]

(* theory Image *)
let image = Theory.ns_find_ls image_theory.Theory.th_export ["image"]

(* theory Function *)
let partial_function = Theory.ns_find_ls function_theory.Theory.th_export ["infix +->"]
let total_function = Theory.ns_find_ls function_theory.Theory.th_export ["infix -->"]
let apply = Theory.ns_find_ls function_theory.Theory.th_export ["apply"]
let partial_injection = Theory.ns_find_ls function_theory.Theory.th_export ["infix >+>"]
let total_injection = Theory.ns_find_ls function_theory.Theory.th_export ["infix >->"]
let partial_surjection = Theory.ns_find_ls function_theory.Theory.th_export ["infix +->>"]
let total_surjection = Theory.ns_find_ls function_theory.Theory.th_export ["infix -->>"]
let partial_bijection = Theory.ns_find_ls function_theory.Theory.th_export ["infix >+>>"]
let total_bijection = Theory.ns_find_ls function_theory.Theory.th_export ["infix >->>"]
let to_relation = Theory.ns_find_ls function_theory.Theory.th_export ["to_relation"]
let to_function = Theory.ns_find_ls function_theory.Theory.th_export ["to_function"]

(* theory Restriction *)
let range_restriction = Theory.ns_find_ls restriction_theory.Theory.th_export ["infix |>"]
let range_substraction = Theory.ns_find_ls restriction_theory.Theory.th_export ["infix |>>"]
let domain_restriction = Theory.ns_find_ls restriction_theory.Theory.th_export ["infix <|"]
let domain_substraction = Theory.ns_find_ls restriction_theory.Theory.th_export ["infix <<|"]

(* theory Overriding *)
let overriding = Theory.ns_find_ls overriding_theory.Theory.th_export ["infix <+"]

(* theory Identity *)
let identity = Theory.ns_find_ls identity_theory.Theory.th_export ["id"]

(* theory Sequence *)
let seq_length = Theory.ns_find_ls sequence_theory.Theory.th_export ["seq_length"]
let size = Theory.ns_find_ls sequence_theory.Theory.th_export ["size"]
let seq = Theory.ns_find_ls sequence_theory.Theory.th_export ["seq"]
let seq1 = Theory.ns_find_ls sequence_theory.Theory.th_export ["seq1"]
let iseq = Theory.ns_find_ls sequence_theory.Theory.th_export ["iseq"]
let iseq1 = Theory.ns_find_ls sequence_theory.Theory.th_export ["iseq1"]
let perm = Theory.ns_find_ls sequence_theory.Theory.th_export ["perm"]
let head_restriction = Theory.ns_find_ls sequence_theory.Theory.th_export ["infix /|"]
let tail_restriction = Theory.ns_find_ls sequence_theory.Theory.th_export ["infix |/"]

(* theory BList *)
let insert_in_front = Theory.ns_find_ls blist_theory.Theory.th_export ["insert_in_front"]
let insert_at_tail = Theory.ns_find_ls blist_theory.Theory.th_export ["insert_at_tail"]
let tail = Theory.ns_find_ls blist_theory.Theory.th_export ["tail"]
let last = Theory.ns_find_ls blist_theory.Theory.th_export ["last"]
let first = Theory.ns_find_ls blist_theory.Theory.th_export ["first"]
let front = Theory.ns_find_ls blist_theory.Theory.th_export ["front"]
let concatenation = Theory.ns_find_ls blist_theory.Theory.th_export ["concatenation"]
let rev = Theory.ns_find_ls blist_theory.Theory.th_export ["rev"]
let conc = Theory.ns_find_ls blist_theory.Theory.th_export ["conc"]
let restriction_tail = Theory.ns_find_ls blist_theory.Theory.th_export ["restriction_tail"]
let restriction_head = Theory.ns_find_ls blist_theory.Theory.th_export ["restriction_head"]

(* theory IsFinite *)
let is_finite_subset = Theory.ns_find_ls isfinite_theory.Theory.th_export ["is_finite_subset"]
let finite_subsets = Theory.ns_find_ls isfinite_theory.Theory.th_export ["finite_subsets"]
let non_empty_finite_subsets = Theory.ns_find_ls isfinite_theory.Theory.th_export ["non_empty_finite_subsets"]
let card = Theory.ns_find_ls isfinite_theory.Theory.th_export ["card"]

(* theory PowerRelation *)
let times = Theory.ns_find_ls powerrelation_theory.Theory.th_export ["times"]
let relations = Theory.ns_find_ls powerrelation_theory.Theory.th_export ["relations"]

(* theory MinMax *)
let imin = Theory.ns_find_ls minmax_theory.Theory.th_export ["imin"]
let imax = Theory.ns_find_ls minmax_theory.Theory.th_export ["imax"]
let rmin = Theory.ns_find_ls minmax_theory.Theory.th_export ["rmin"]
let rmax = Theory.ns_find_ls minmax_theory.Theory.th_export ["rmax"]

(* theory Iteration *)
let iterate = Theory.ns_find_ls iteration_theory.Theory.th_export ["iterate"]
let closure = Theory.ns_find_ls iteration_theory.Theory.th_export ["closure"]
let closure1 = Theory.ns_find_ls iteration_theory.Theory.th_export ["closure1"]

(* theory Projection *)
let prj1 = Theory.ns_find_ls projection_theory.Theory.th_export ["prj1"]
let prj2 = Theory.ns_find_ls projection_theory.Theory.th_export ["prj2"]

(* theory Generalized *)
let generalized_union = Theory.ns_find_ls generalized_theory.Theory.th_export ["generalized_union"]
let generalized_inter = Theory.ns_find_ls generalized_theory.Theory.th_export ["generalized_inter"]

(* theory SumSigma *)
let sum = Theory.ns_find_ls sumsigma_theory.Theory.th_export ["sum"]
let sigma = Theory.ns_find_ls sumsigma_theory.Theory.th_export ["sigma"]

let theories = [bool_theory; int_theory; power_theory; computerdivision_theory; bbool_theory; interval_theory; powerset_theory; relation_theory; image_theory; identity_theory; inversedomran_theory; function_theory; sequence_theory; isfinite_theory; powerrelation_theory; restriction_theory; overriding_theory; composition_theory; blist_theory; minmax_theory; projection_theory; iteration_theory; generalized_theory; sumsigma_theory]

let new_task =
  let my_task = None in
  List.fold_left (fun t1 t2 -> Task.use_export t1 t2) my_task theories

let my_task = ref new_task

let unary_op =
  let f t x = Term.t_app_infer t [x] in
  function
  | "imax" -> f imax
  | "rmax" -> f rmax
  | "imin" -> f imin
  | "rmin" -> f rmin
  | "card" -> f card
  | "dom" -> f dom
  | "ran" -> f ran
  | "POW" -> f power
  | "POW1" -> f non_empty_power
  | "FIN" -> f finite_subsets
  | "FIN1" -> f non_empty_finite_subsets
  | "union" -> f generalized_union
  | "inter" -> f generalized_inter
  | "seq" -> f seq
  | "seq1" -> f seq1
  | "iseq" -> f iseq
  | "iseq1" -> f iseq1
  | "-i" -> f uminusi
  | "-r" -> f uminusr
  | "~" -> f inverse
  | "size" -> f size
  | "perm" -> f perm
  | "first" -> f first
  | "last" -> f last
  | "id" -> f identity
  | "closure" -> f closure
  | "closure1" -> f closure1
  | "tail" -> f tail
  | "front" -> f front
  | "rev" -> f rev
  | "conc" -> f conc
  | "succ" -> failwith "TODO function succ (fun x -> x + 1)"
  | "pred" -> failwith "TODO funciton pred (fun x -> x - 1)" (* STOP *)
  | "rel" -> f to_relation
  | "fnc" -> f to_function
  | "real" -> f from_int
  | "floor" -> f floor
  | "ceiling" -> f ceil
  | "tree" -> failwith "TODO tree"
  | "btree" -> failwith "TODO btree"
  | "top" -> failwith "TODO top"
  | "sons" -> failwith "TODO sons"
  | "prefix" -> failwith "TODO prefix"
  | "postfix" -> failwith "TODO postfix"
  | "sizet" -> failwith "TODO sizet"
  | "mirror" -> failwith "TODO mirror"
  | "left" -> failwith "TODO left"
  | "right" -> failwith "TODO right"
  | "infix" -> failwith "TODO infix"
  | "bin" -> failwith "TODO bin_unary"
  | x -> failwith ("Invalid unary expression : " ^ x)

let binary_op =
  let f t x y = Term.t_app_infer t [x;y] in
  let notf t x y = Term.t_not (Term.t_app_infer t [x;y]) in
  function
  | "=>" -> Term.t_implies
  | "<=>" -> Term.t_iff
  | ":" -> f mem
  | "/:" -> notf mem
  | "<:" -> f subset
  | "/<:" -> notf subset
  | "<<:" -> failwith "TODO strict included"
  | "/<<:" -> failwith "TODO not strict included"
  | "=" -> Term.t_equ
  | "/=" -> Term.t_neq
  (* integer comparison *)
  | ">=i" -> f geqi
  | ">i" -> f gei
  | "<i" -> f lei
  | "<=i" -> f leqi
  (* real comparison *)
  | ">=r" -> f geqr
  | ">r" -> f ger
  | "<r" -> f ler
  | "<=r" -> f leqr
  (* float comparison *)
  | ">=f" -> failwith "TODO >=f"
  | ">f" -> failwith "TODO >f"
  | "<f" -> failwith "TODO <f"
  | "<=f" -> failwith "TODO <=f"
  | "," -> fun x y -> Term.t_tuple [x;y]
  | "*i" -> f muli
  | "*r" -> f mulr
  | "*f" -> failwith "TODO *f"
  | "*s" -> f product
  | "**i" -> f power_int
  | "**r" -> f pow
  | "+i" -> f plusi
  | "+r" -> f plusr
  | "+f" -> failwith "TODO +f"
  | "+->" -> f partial_function
  | "+->>" -> f partial_surjection
  | "-i" -> f minusi
  | "-r" -> f minusr
  | "-f" -> failwith "TODO -f"
  | "-s" -> f diff
  | "-->" -> f total_function
  | "-->>" -> f total_surjection
  | "->" -> f insert_in_front
  | ".." -> f interval
  | "/i" -> f div
  | "/r" -> f divr
  | "/f" -> failwith "TODO /f"
  | "/\\" -> f inter
  | "/|\\" -> f head_restriction
  | ";" -> f semicolon
  | "<+" -> f overriding
  | "<->" -> f relations
  | "<-" -> f insert_at_tail
  | "<<|" -> f domain_substraction
  | "<|" -> f domain_restriction
  | ">+>" -> f partial_injection
  | ">->" -> f total_injection
  | ">->>" -> f total_bijection
  | "><" -> f direct_product
  | "||" -> f parallel_product
  | "\\/" -> f union
  | "\\|/" -> f tail_restriction
  | "^" -> f concatenation
  | "mod" -> f ( mod )
  | "|->" -> fun x y -> Term.t_tuple [x;y]
  | "|>" -> f range_restriction
  | "|>>" -> f range_substraction
  | "[" -> f image (* image of a relation *)
  | "(" -> f apply (* image of a function *)
  | "prj1" -> f prj1
  | "prj2" -> f prj2
  | "iterate" -> f iterate
  | "const" -> failwith "TODO const"
  | "rank" -> failwith "TODO rank"
  | "father" -> failwith "TODO father"
  | "subtree" -> failwith "TODO subtree"
  | "arity" -> failwith "TODO arity"
  | x -> failwith ("Invalid binary expression : " ^ x)

let ternary_op =
  function
  | "son" -> fun _ _ _ -> failwith "TODO son"
  | "bin" -> failwith "TODO bin_ternary"
  | x -> failwith ("Invalid ternary expression : " ^ x)

let rec my_fold f o =
  function
  | [] -> o
  | [x] -> x
  | x :: l -> f x (my_fold f o l)

let nary_op =
  function
  | "[" -> fun _ -> failwith "should not happen"
  | "{" -> fun _ -> failwith "should not happen"
  | "&" -> my_fold Term.t_and Term.t_true
  | "or" -> my_fold Term.t_or Term.t_false
  | x -> failwith ("Invalid n-ary expression : " ^ x)

let quantified_pred_op s args body =
  match s with
  | "!" -> Term.t_forall_close args [] body
  | "#" -> Term.t_exists_close args [] body
  | x -> failwith ("Invalid predicate quantifier : " ^ x)

let quantified_exp_op s =
  begin
    match s with
    | "%" -> fun x -> x
    | "iSIGMA" -> failwith "TODO quantified exp"
    | "rSIGMA" -> failwith "TODO quantified exp"
    | "iPI" -> failwith "TODO quantified exp"
    | "rPI" -> failwith "TODO quantified exp"
    | "INTER" -> failwith "TODO quantified exp"
    | "UNION" -> failwith "TODO quantified exp"
    | x -> failwith ("Invalid expression quantifier : " ^ x)
  end

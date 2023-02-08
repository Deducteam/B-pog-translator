/* Infix precedences:
1 - 10 propositions

15 predicates

125 arrow operators (-->, +->>, ...)

160 operations on sets (∪, <+, map, ...)
170 --

180-200 arithmetic operators
*/

// Identifiers
constant symbol ID : TYPE;

// Propositions
constant symbol U : TYPE;
constant symbol Thm : U → TYPE;

// B Types
constant symbol T : TYPE;
constant symbol τ : T → TYPE;

// Type constructors
constant symbol Set : T → T; // Pow
constant symbol * : T → T → T; // Product
notation * infix left 1;

// Base types
constant symbol Z_T : T;
constant symbol INTEGER : τ (Set Z_T);
constant symbol R_T : T;
constant symbol REAL : τ (Set R_T);
constant symbol FLOAT_T : T;
constant symbol FLOAT : τ (Set FLOAT_T);
constant symbol BOOL_T : T;
constant symbol BOOL : τ (Set BOOL_T);
constant symbol STRING_T : T;
constant symbol STRING : τ (Set STRING_T);

// Formal types
constant symbol type_T : ID → T; // ID_type
// Generic_Type does not exist

// Lists
constant symbol list : T → TYPE;
constant symbol nil [t] : list t;
constant symbol cons [t] : τ t → list t → list t;

// variable number of arguments (for curryfying lambda-abstractions)
constant symbol typlist : TYPE;
constant symbol typnil : T → typlist;
constant symbol typcons : T → typlist → typlist;
symbol curryT : typlist → T → TYPE;
rule curryT (typnil $t) $u ↪ τ $t → τ $u;
rule curryT (typcons $t $l) $u ↪ τ $t → curryT $l $u;

symbol curryU : typlist → TYPE;
rule curryU (typnil $t) ↪ τ $t → U;
rule curryU (typcons $t $l) ↪ τ $t → curryU $l;

symbol to_T : typlist → T;
rule to_T (typnil $t) ↪ $t;
rule to_T (typcons $t (typnil $u)) ↪ $t * $u;
rule to_T (typcons $t (typcons $u $v)) ↪ to_T (typcons ($t * $u) $v);

// Structures
// Note that the order of declaration is relevant in Atelier B, i.e. struct(a : A, b : B) ≠ struct (b : B, a : A)
// I do not check that all record labels are different (is this a problem?)
constant symbol struct_sig : TYPE;
constant symbol struct_nil : ID → T → struct_sig;
constant symbol struct_cons : ID → T → struct_sig → struct_sig;
constant symbol struct_T : struct_sig → T; // Struct_type

constant symbol record_sig : TYPE;
constant symbol record_nil [t] : ID → τ t → record_sig;
constant symbol record_cons [t] : ID → τ t → record_sig → record_sig;

symbol struct_type : record_sig → struct_sig;
rule struct_type (record_nil [Set $t] $i _) ↪ struct_nil $i $t;
rule struct_type (record_cons [Set $t] $i _ $tl) ↪ struct_cons $i $t (struct_type $tl);

symbol record_type : record_sig → struct_sig;
rule record_type (record_nil [$t] $i _) ↪ struct_nil $i $t;
rule record_type (record_cons [$t] $i _ $tl) ↪ struct_cons $i $t (record_type $tl);

constant symbol accessible : ID → T → struct_sig → TYPE;
constant symbol accessible_nil [id t] : accessible id t (struct_nil id t);
constant symbol accessible_cons [id t s] : accessible id t (struct_cons id t s);
constant symbol skip [id1 t1 id2 t2 s] : accessible id1 t1 s → accessible id1 t1 (struct_cons id2 t2 s);

/* Predicates */
// Propositional logic
constant symbol TRUE : U; // nullary Or
constant symbol FALSE : U; // nullary And
constant symbol ⇒ : U → U → U; // Imply
notation ⇒ infix right 1;
constant symbol ⇔ : U → U → U; // Equiv
notation ⇔ infix right 2;
constant symbol ∨ : U → U → U; // Or
notation ∨ infix right 3;
constant symbol ∧ : U → U → U; // And
notation ∧ infix right 4;
constant symbol ¬ : U → U; // Not

// Quantifiers
constant symbol ∀ [t] : (τ t → U) → U; // Forall
notation ∀ quantifier;
constant symbol ∃ [t] : (τ t → U) → U; // Exists
notation ∃ quantifier;

// Binary predicates
constant symbol ∈ [t] : τ t → τ (Set t) → U; // Belong
notation ∈ infix 15;
constant symbol notin [t] : τ t → τ (Set t) → U; // NotBelong
notation notin infix 15;
constant symbol ⊆ [t] : τ (Set t) → τ (Set t) → U; // Included
notation ⊆ infix 15;
constant symbol notincluded [t] : τ (Set t) → τ (Set t) → U; // NotIncluded
notation notincluded infix 15;
constant symbol ⊂ [t] : τ (Set t) → τ (Set t) → U; // StrictIncluded
notation ⊂ infix 15;
constant symbol notstrictincluded [t] : τ (Set t) → τ (Set t) → U; // NotStrictIncluded
notation notstrictincluded infix 15;
constant symbol = [t] : τ t → τ t → U; // Equal
notation = infix 15;
constant symbol ≠ [t] : τ t → τ t → U; // NotEqual
notation ≠ infix 15;
constant symbol ≥i : τ Z_T → τ Z_T → U; // IGeq
notation ≥i infix 15;
constant symbol >i : τ Z_T → τ Z_T → U; // IGreater
notation >i infix 15;
constant symbol <i : τ Z_T → τ Z_T → U; // ISmaller
notation <i infix 15;
constant symbol ≤i : τ Z_T → τ Z_T → U; // ILeq
notation ≤i infix 15;
constant symbol ≥r : τ R_T → τ R_T → U; // RGeq
notation ≥r infix 15;
constant symbol >r : τ R_T → τ R_T → U; // RGreater
notation >r infix 15;
constant symbol <r : τ R_T → τ R_T → U; // RSmaller
notation <r infix 15;
constant symbol ≤r : τ R_T → τ R_T → U; // RLeq
notation ≤r infix 15;
constant symbol ≥f : τ FLOAT_T → τ FLOAT_T → U; // FGeq
notation ≥f infix 15;
constant symbol >f : τ FLOAT_T → τ FLOAT_T → U; // FGreater
notation >f infix 15;
constant symbol <f : τ FLOAT_T → τ FLOAT_T → U; // FSmaller
notation <f infix 15;
constant symbol ≤f : τ FLOAT_T → τ FLOAT_T → U; // FLeq
notation ≤f infix 15;

// Unary operators
// Max does not exist
constant symbol imax : τ (Set Z_T) → τ Z_T; // IMax
constant symbol rmax : τ (Set R_T) → τ R_T; // RMax
// Min does not exist
constant symbol imin : τ (Set Z_T) → τ Z_T; // IMin
constant symbol rmin : τ (Set R_T) → τ R_T; // RMin
constant symbol card [t] : τ (Set t) → τ Z_T; // Card
constant symbol dom [t u] : τ (Set (t * u)) → τ (Set t); // Dom
constant symbol ran [t u] : τ (Set (t * u)) → τ (Set u); // Ran
constant symbol pow [t] : τ (Set t) → τ (Set (Set t)); // POW
constant symbol pow1 [t] : τ (Set t) → τ (Set (Set t)); // POW1
constant symbol fin [t] : τ (Set t) → τ (Set (Set t)); // FIN
constant symbol fin1 [t] : τ (Set t) → τ (Set (Set t)); // FIN1
constant symbol union_gen [t] : τ (Set (Set t)) → τ (Set t); // Union_gen
constant symbol inter_gen [t] : τ (Set (Set t)) → τ (Set t); // Union_gen
constant symbol seq [t] : τ (Set t) → τ (Set (Set (Z_T * t))); // Seq
constant symbol seq1 [t] : τ (Set t) → τ (Set (Set (Z_T * t))); // Seq1
constant symbol iseq [t] : τ (Set t) → τ (Set (Set (Z_T * t))); // ISeq
constant symbol iseq1 [t] : τ (Set t) → τ (Set (Set (Z_T * t))); // ISeq1
// Minus does not exist
constant symbol minusi : τ Z_T → τ Z_T; // IMinus
constant symbol minusr : τ R_T → τ R_T; // RMinus
constant symbol ~ [t u] : τ (Set (t * u)) → τ (Set (u * t)); // Inversion
constant symbol size [t] : τ (Set (Z_T * t)) → τ Z_T; // Size
constant symbol perm [t] : τ (Set t) → τ (Set (Set (Z_T * t))); // Perm
constant symbol first [t] : τ (Set (Z_T * t)) → τ t; // First
constant symbol last [t] : τ (Set (Z_T * t)) → τ t; // Last
constant symbol id [t] : τ (Set t) → τ (Set (t * t)); // Identity
constant symbol closure [t] : τ (Set (t * t)) → τ (Set (t * t)); // Closure
constant symbol closure1 [t] : τ (Set (t * t)) → τ (Set (t * t)); // Closure1
constant symbol tail [t] : τ (Set (Z_T * t)) → τ (Set (Z_T * t)); // Tail
constant symbol front [t] : τ (Set (Z_T * t)) → τ (Set (Z_T * t)); // Front
constant symbol rev [t] : τ (Set (Z_T * t)) → τ (Set (Z_T * t)); // Rev
constant symbol conc [t] : τ (Set (Z_T * Set (Z_T * t))) → τ (Set (Z_T * t)); // Conc
constant symbol succ : τ Z_T → τ Z_T; // Succ
constant symbol pred : τ Z_T → τ Z_T; // Pred
constant symbol rel [t u] : τ (Set (t * (Set u))) → τ (Set (t * u)); // Rel
constant symbol fnc [t u] : τ (Set (t * u)) → τ (Set (t * (Set u))); // Fnc
constant symbol real : τ Z_T → τ R_T; // Real
constant symbol floor : τ R_T → τ Z_T; // Floor
constant symbol ceiling : τ R_T → τ Z_T; // Ceiling
constant symbol tree [t] : τ (Set t) → τ (Set (Set (Set (Z_T * Z_T) * t))); // Tree
constant symbol btree [t] : τ (Set t) → τ (Set (Set (Set (Z_T * Z_T) * t))); // BTree
constant symbol top [t] : τ (Set (Set (Z_T * Z_T) * t)) → τ t; // BTree
constant symbol sons [t] : τ (Set (Set (Z_T * Z_T) * t)) → τ (Set (Z_T * (Set (Set (Z_T * Z_T) * t)))); // Sons
constant symbol pref [t] : τ (Set (Set (Z_T * Z_T) * t)) → τ (Z_T * t); // Prefix
constant symbol post [t] : τ (Set (Set (Z_T * Z_T) * t)) → τ (Z_T * t); // Postfix
constant symbol sizet [t] : τ (Set (Set (Z_T * Z_T) * t)) → τ Z_T; // Sizet
constant symbol mirror [t] : τ (Set (Set (Z_T * Z_T) * t)) → τ (Set (Set (Z_T * Z_T) * t)); // Mirror
constant symbol left_tree [t] : τ (Set (Set (Z_T * Z_T) * t)) → τ (Set (Set (Z_T * Z_T) * t)); // Left
constant symbol right_tree [t] : τ (Set (Set (Z_T * Z_T) * t)) → τ (Set (Set (Z_T * Z_T) * t)); // Right
constant symbol infix_tree [t] : τ (Set (Set (Z_T * Z_T) * t)) → τ (Set (Z_T * t)); // Infix
constant symbol bin_unary [t] : τ (Set (Set (Z_T * Z_T) * t)) → τ (Set (Set (Z_T * Z_T) * t)); // Bin_unary

// Binary operators
constant symbol pair [t u] : τ t → τ u → τ (t * u); // Pair
// Prod does not exist
constant symbol *_i : τ Z_T → τ Z_T → τ Z_T; // IProd
notation *_i infix left 190;
constant symbol *_r : τ R_T → τ R_T → τ R_T; // RProd
notation *_r infix left 190;
constant symbol *_f : τ FLOAT_T → τ FLOAT_T → τ FLOAT_T; // FProd
notation *_f infix left 190;
constant symbol *_s [t u] : τ (Set t) → τ (Set u) → τ (Set (t * u)); // SProd
notation *_s infix left 190;
// Exp does not exist
constant symbol **i : τ Z_T → τ Z_T → τ Z_T; // IExp
notation **i infix right 200;
constant symbol **r : τ R_T → τ R_T → τ R_T; // RExp
notation **r infix right 200;
// Plus does not exist
constant symbol +_i : τ Z_T → τ Z_T → τ Z_T; // IPlus
notation +_i infix left 180;
constant symbol +_r : τ R_T → τ R_T → τ R_T; // RPlus
notation +_r infix left 180;
constant symbol +_f : τ FLOAT_T → τ FLOAT_T → τ FLOAT_T; // FPlus
notation +_f infix left 180;
constant symbol +-> [t u] : τ (Set t) → τ (Set u) → τ (Set (Set (t * u))); // Partial
notation +-> infix left 125;
constant symbol +->> [t u] : τ (Set t) → τ (Set u) → τ (Set (Set (t * u))); // PartialSurj
notation +->> infix left 125;
// Minus does not exist
constant symbol -_i : τ Z_T → τ Z_T → τ Z_T; // IMinus
notation -_i infix left 180;
constant symbol -_r : τ R_T → τ R_T → τ R_T; // RMinus
notation -_r infix left 180;
constant symbol -_f : τ FLOAT_T → τ FLOAT_T → τ FLOAT_T; // FMinus
notation -_f infix left 180;
constant symbol -_s [t] : τ (Set t) → τ (Set t) → τ (Set t); // SMinus
notation -_s infix left 180;
constant symbol --> [t u] : τ (Set t) → τ (Set u) → τ (Set (Set (t * u))); // Total
notation --> infix left 125;
constant symbol -->> [t u] : τ (Set t) → τ (Set u) → τ (Set (Set (t * u))); // TotalSurj
notation -->> infix left 125;
constant symbol -> [t] : τ t → τ (Set (Z_T * t)) → τ (Set (Z_T * t)); // HeadInsert
notation -> infix left 160;
constant symbol -- : τ Z_T → τ Z_T → τ (Set Z_T); // Interval
notation -- infix left 170;
// Div does not exist
constant symbol div_i : τ Z_T → τ Z_T → τ Z_T; // IDiv
notation div_i infix left 190;
constant symbol div_r : τ R_T → τ R_T → τ R_T; // RDiv
notation div_r infix left 190;
constant symbol div_f : τ FLOAT_T → τ FLOAT_T → τ FLOAT_T; // FDiv
notation div_f infix left 190;
constant symbol ∩ [t] : τ (Set t) → τ (Set t) → τ (Set t); // Inter
notation ∩ infix left 160;
constant symbol headrestrict [t] : τ (Set (Z_T * t)) → τ Z_T → τ (Set (Z_T * t)); // HeadRestrict
constant symbol comp [t u v] : τ (Set (t * u)) → τ (Set (u * v)) → τ (Set (t * v)); // Compose
constant symbol <+ [t u] : τ (Set (t * u)) → τ (Set (t * u)) → τ (Set (t * u)); // Overload
notation <+ infix left 160;
constant symbol <-> [t u] : τ (Set t) → τ (Set u) → τ (Set (Set (t * u))); // Relation
notation <-> infix left 125;
constant symbol <- [t] : τ (Set (Z_T * t)) → τ t → τ (Set (Z_T * t)); // TailInsert
notation <- infix left 160;
constant symbol dom- [t u] : τ (Set t) → τ (Set (t * u)) → τ (Set (t * u)); // DomSubtract
constant symbol domrestrict [t u] : τ (Set t) → τ (Set (t * u)) → τ (Set (t * u)); // DomRestrict
constant symbol >+> [t u] : τ (Set t) → τ (Set u) → τ (Set (Set (t * u))); // PartialInject
notation >+> infix left 125;
constant symbol >-> [t u] : τ (Set t) → τ (Set u) → τ (Set (Set (t * u))); // TotalInject
notation >-> infix left 125;
// DoesntExist (>+>>) does not exist
constant symbol >->> [t u] : τ (Set t) → τ (Set u) → τ (Set (Set (t * u))); // TotalBiject
notation >->> infix left 125;
constant symbol >< [t u v] : τ (Set (t * u)) → τ (Set (t * v)) → τ (Set (t * (u * v))); // DirectProd
notation >< infix left 160;
constant symbol par [t u v w] : τ (Set (t * u)) → τ (Set (v * w)) → τ (Set ((t * v) * (u * w))); // ParallelProd
notation par infix left 160;
constant symbol ∪ [t] : τ (Set t) → τ (Set t) → τ (Set t); // Union
notation ∪ infix left 160;
constant symbol tailrestrict [t] : τ (Set (Z_T * t)) → τ Z_T → τ (Set (Z_T * t)); // TailRestrict
constant symbol ^ [t] : τ (Set (Z_T * t)) → τ (Set (Z_T * t)) → τ (Set (Z_T * t)); // Concat
notation ^ infix left 160;
constant symbol mod : τ Z_T → τ Z_T → τ Z_T; // Mod
notation mod infix left 190;
constant symbol map [t u] : τ t → τ u → τ (t * u); // MapLet
notation map infix left 160;
constant symbol imagerestrict [t u] : τ (Set (t * u)) → τ (Set u) → τ (Set (t * u)); // ImageRestrict
constant symbol image- [t u] : τ (Set (t * u)) → τ (Set u) → τ (Set (t * u)); // ImageSubtract
constant symbol image [t u] : τ (Set (t * u)) → τ (Set t) → τ (Set u); // Image
constant symbol eval [t u] : τ (Set (t * u)) → τ t → τ u; // Eval
// Doesntexist2 (<') does not exist
constant symbol prj1 [t u] : τ (Set t) → τ (Set u) → τ (Set ((t * u) * t)); // Prj1
constant symbol prj2 [t u] : τ (Set t) → τ (Set u) → τ (Set ((t * u) * u)); // Prj2
constant symbol iterate [t] : τ (Set (t * t)) → τ Z_T → τ (Set (t * t)); // Iterate
constant symbol const [t] : τ t → τ (Set (Z_T * Set (Set(Z_T * Z_T) * t))) → τ (Set (Set(Z_T * Z_T) * t)); // Const
constant symbol rank [t] : τ (Set (Set(Z_T * Z_T) * t)) → τ (Set (Z_T * Z_T)) → τ Z_T; // Rank
constant symbol father [t] : τ (Set (Set(Z_T * Z_T) * t)) → τ (Set (Z_T * Z_T)) → τ (Set (Z_T * Z_T)); // Father
constant symbol subtree [t] : τ (Set (Set(Z_T * Z_T) * t)) → τ (Set (Z_T * Z_T)) → τ (Set (Set (Z_T * Z_T) * t)); // Subtree
constant symbol arity [t] : τ (Set (Set(Z_T * Z_T) * t)) → τ (Set (Z_T * Z_T)) → τ Z_T; // Arity

// Ternary operators
constant symbol son [t] : τ (Set (Set (Z_T * Z_T) * t)) → τ (Set (Z_T * Z_T)) → τ Z_T → τ (Set (Z_T * Z_T)); // Son
constant symbol bin_ternary [t] : τ (Set (Set (Z_T * Z_T) * t)) → τ t → τ (Set (Set (Z_T * Z_T) * t)) → τ (Set (Set (Z_T * Z_T) * t)); // Bin_ternary

// Nary operators
constant symbol sequence [t] : list t → τ (Set (Z_T * t)); // Sequence
constant symbol extension [t] : list t → τ (Set t); // Extension

// Quantified operators
constant symbol % [t u] : curryU t → curryT t u → τ (Set (to_T t * u)); // Lambda
// SIGMA does not exist
constant symbol iSIGMA [t] : curryU t → curryT t Z_T → τ Z_T; // ISIGMA
constant symbol rSIGMA [t] : curryU t → curryT t R_T → τ R_T; // RSIGMA
// PI does not exist
constant symbol iPI [t] : curryU t → curryT t Z_T → τ Z_T; // IPI
constant symbol rPI [t] : curryU t → curryT t R_T → τ R_T; // RPI
constant symbol INTER [t u] : curryU t → curryT t (Set u) → τ (Set u); // INTER
constant symbol UNION [t u] : curryU t → curryT t (Set u) → τ (Set u); // UNION
constant symbol comprehension [t] : curryU t → τ (Set (to_T t)); // Quantified_Set

// Booleans
constant symbol true : τ BOOL_T; // true (Boolean_Literal)
constant symbol false : τ BOOL_T; // false (Boolean_Literal)
constant symbol Boolean_Exp : U → τ BOOL_T; // Boolean_Exp

// Other stuff
constant symbol emptyset t : τ (Set t); // EmptySet
constant symbol emptyseq t : τ (Set (Z_T * t)); // EmptySeq
constant symbol cst t : ID → τ (Set t); // Id_exp

// Integers
constant symbol pos_int : TYPE;
constant symbol xH : pos_int;
constant symbol x0 : pos_int → pos_int;
constant symbol x1 : pos_int → pos_int;
constant symbol 0' : τ Z_T;
constant symbol Zpos : pos_int → τ Z_T;
constant symbol Zneg : pos_int → τ Z_T;

// Records
constant symbol struct (r : record_sig) : τ (Set (struct_T (struct_type r))); // Struct_exp
constant symbol record (r : record_sig) : τ (struct_T (record_type r)); // Record
constant symbol record_update [t u] (r : τ (struct_T t)) (i : ID) : τ u → accessible i u t → τ (struct_T t); // Record_Update
constant symbol record_field_access [t u] (r : τ (struct_T t)) (i : ID) : accessible i u t → τ u; // Record_Field_Access

(* EXPRESSIONS *)

(** All program variables used in expressions are just strings. *)
type pvar = string ;;

(** All logical variables used in expressions are encoded as integers to uniquely identify them. *)
type lvar = int ;;

(** Logical variable counter to generate fresh new ones. *)
let logical_counter = ref 0 ;;

(** Generates a new unused logical variable. *)
let fresh_lvar () =
  let id = !logical_counter in
    incr logical_counter; id
;;

(** A variable can be a program variable or a logical variable. *)
type var = Pvar of pvar | Lvar of lvar ;;

(** A binary operator:
    - [Add]: addition.
    - [Sub]: subtraction.
    - [Mul]: multiplication. *)
type bin_op = Add | Sub | Mul ;;

(** Expressions:
    - [Ide (var)]: identifier of a variable.
    - [Int (int)]: an constant (integer).
    - [Binop (op, e1, e2)]: binary arithmetic operation between expressions. *)
type expr =
  | Ide of var
  | Int of int
  | Binop of bin_op * expr * expr
;;

(* PURE PREDICATES *)

(** A logical comparison operator:
    - [Eq]: equality.
    - [Neq]: inequality. *)
type comp_op =
  | Eq
  | Neq
;;

(** A pure logical predicate:
    - [TrueB]: the boolean constant true.
    - [Comp (op, e1, e2)]: comparison between expressions.
    - [PointsTo (e1, e2)]: *)
type pure_pred =
  | TrueB
  | Comp of comp_op * expr * expr
  | PointsToP of expr * expr
;;

(* SPATIAL PREDICATES *)

(** A spatial predicate in separation logic:
    - [TrueS]: spatial tautology (no constraints on the heap).
    - [PointsTo (e1, e1)]: location [e1] points to [e2].
    - [Freed e]: location [e] has been deallocated.
    - [List (e1, e2)]: list segment from location [e1] to location [e2].

    In separation logic, [TrueS] represents a neutral spatial heap
    that does not contradict the rest of the symbolic heap. NOTE:
    It is not emp, i.e. the neutral element of spatial conjunction. *)
type spat_pred =
  | TrueS
  | PointsTo of expr * expr
  | Freed of expr
  | List of expr * expr
;;

(* SYMBOLIC HEAPS *)

(** A symbolic heap is composed of:
    - [exists] : a list of existentially quantified logical variables.
    - [pure] : a list of pure predicates, combined via logical conjunction ([&]).
    - [spatial] : a list of spatial predicates, combined via separating conjunction ([*]). *)
type symb_heap = {
  exists: lvar list;
  pure : pure_pred list;
  spatial : spat_pred list;
}

(** The empty symbolic heap [ true | emp ]. *)
let empty_sh = {exists = []; pure = []; spatial = []} ;;

(** The symbolic heap with any spatial part [ true | true ]. *)
let true_sh = {exists = []; pure = []; spatial = [TrueS]} ;;
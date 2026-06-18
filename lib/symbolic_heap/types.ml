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

(** Compares identifier [ide1] with identifier [ide2]. The comparison is performed on terms of simplicity,
    a program variable is considered simpler than a logical variable*)
let compare_var ide1 ide2 =
  match ide1, ide2 with
    | Pvar _, Pvar _ -> 0
    | Pvar _, Lvar _ -> 1
    | Lvar _, Pvar _ -> -1
    | Lvar v1, Lvar v2 -> v1 - v2
;;

module VarOrd = struct
  type t = var
  let compare = compare_var
end

(** Set of variables. *)
module VarSet = Set.Make(VarOrd)

(** A binary operator:
    - [Add]: addition.
    - [Sub]: subtraction.
    - [Mul]: multiplication.
    BINARY OPERATIONS HAVE NOT BEEN IMPLEMENTED YET, AVOID USING THEM! *)
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

(** Compare expression [e1] with expression [e2]. The comparison is performed on terms of simplicity, 
    an integer is considered simpler than a program variable, which is simpler than a logical variable.
    
    BINARY OPERATION HAVE NOT BEEN IMPLEMENTED YET. *)
let compare_expr e1 e2 =
  match e1, e2 with
    | Ide ide1, Ide ide2 -> compare_var ide1 ide2
    | Ide _, Int _ -> -1
    | Ide _, Binop (_, _, _) -> failwith "Binary operations not implemented yet"
    | Int _, Ide _ -> 1
    | Int n1, Int n2 -> 0
    | Int _, Binop (_, _, _) -> failwith "Binary operations not implemented yet"
    | Binop (_, _, _), Ide _ -> failwith "Binary operations not implemented yet"
    | Binop (_, _, _), Int _ -> failwith "Binary operations not implemented yet"
    | Binop (_, _, _), Binop (_, _, _) -> failwith "Binary operations not implemented yet"
;;

module ExprOrd = struct
  type t = expr
  let compare = compare_expr
end

(** Set of expressions. *)
module ExprSet = Set.Make(ExprOrd)

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
    - [Comp (op, e1, e2)]: comparison between expressions. *)
type pure_pred =
  | TrueB
  | Comp of comp_op * expr * expr
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

(** The empty symbolic heap [[ true | emp ]]. *)
let empty_sh = {exists = []; pure = []; spatial = []} ;;

(** The symbolic heap with any spatial part [[ true | true ]]. *)
let true_sh = {exists = []; pure = []; spatial = [TrueS]} ;;
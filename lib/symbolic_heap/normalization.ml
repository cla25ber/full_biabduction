open Types
open Utils

open UnionFind

(** Terms define what is compared in predicates. They can be:
    - [Expr (e)]: expressions like integers, variables and binary operations between sub-expressions.
    - [Heap (e)]: the value to which location [e] points to, i.e. h(e).
    - [Bottom]: the empty location. 
    
    Spatial predicates like e1 -> e2 are viewed as h(e1) = e2, while e-/> as h(e) = bottom. *)
type term = 
  | Expr of expr
  | Heap of expr
  | Bottom
;;

(** Compare term [t1] with term [t2]. The comparison is performed on terms of simplicity, 
    bottom is considered simpler than an expression, which is simpler than a heap expression. *)
let compare_terms t1 t2 =
  match t1, t2 with
    | Expr e1, Expr e2 -> compare_expr e1 e2
    | Expr _, Heap _ -> 1
    | Expr _, Bottom -> -1
    | Heap _, Expr _ -> -1
    | Heap e1, Heap e2 -> compare_expr e1 e2
    | Heap _, Bottom -> -1
    | Bottom, Expr _ -> 1
    | Bottom, Heap _ -> 1
    | Bottom, Bottom -> 0
;;

module TermOrd = struct
  type t = term
  let compare = compare_terms
end

(** Set of terms. *)
module TermSet = Set.Make(TermOrd)

(** *)
type terms_node = {
  representant : term;
  terms : TermSet.t

}
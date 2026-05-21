open Types

(* EXPRESSIONS TERMS *)

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

(* CONSISTENCY GRAPH *)
(*  A graph whose nodes represent classes of equivalent terms, while edges indicate relations about said classes. *)
(*  The graph uses a union-find approach: when merging two classes we do not create a new node. Instead, 
    one becomes a sub-class of the other to fasten searching. *)

(** An edge in the consistency graph is composed of:
    - [node]: the node to which it is connected to.
    - [label] the relation between the node from which the edge starts towards the one it points to.*)
type edge = {
  node : int;
  label : comp_op
}

module EdgeOrd = struct
  type t = edge
  let compare = (fun e1 e2 -> e1.node - e2.node)
end

(** Set of edges. *)
module EdgeSet = Set.Make(EdgeOrd)

(** A node in the consistency graph is composed of:
    - [id]: the unique identifier of the node.
    - [parent]: the superclass of this node. If the node is a root node, then parent is itself.
    - [rank]: the height of the tree starting from this class.
    - [repr]: the representant of the class the node portrays.
    - [members]: all the terms that are in the same class, i.e. equivalent.
    - [edges]: all the outward edges from the node. *)
type node = {
  id : int;
  mutable parent : node;
  mutable rank : int;
  mutable repr : term;
  mutable members : TermSet.t;
  mutable edges: EdgeSet.t
}

(** A consistency graph is an hash table from a term to the node that contains it. *)
type graph = (term, node) Hashtbl.t ;;
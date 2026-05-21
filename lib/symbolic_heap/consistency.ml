open Types
open Constraints

(** Exception to be raised when the graph is not structured as it should. For example,
    when multiple nodes have the same id, or when classes are not disjoint.*)
exception UnstructuredGraph of string ;;

(** Returns the resulting relation when merging two nodes with relations [rel1] and [rel2]. *)
let resulting_relation rel1 rel2 =
  match rel1, rel2 with
    | Neq, Neq -> Some(Neq)
    | Neq, Eq -> None
    | Eq, Neq -> None
    | Eq, Eq -> Some(Eq)
;;

(** Returns the relation from node [n1] to node [n2].

    {i {b @Raise}} [UnstructuredGraph] exception when multiple nodes have the same id. *)
let node_relation n1 n2 =
  let edge_set = (EdgeSet.filter (fun edge -> edge.node = n2.id) n1.edges) in
    match EdgeSet.elements edge_set with
      | [] -> None
      | [edge] -> Some(edge.label)
      | _ -> raise (UnstructuredGraph "Multiple nodes with the same id.")
;;

(** Checks if a relation between node [n1] to node [n2] can cause an inconsistency.

    {i {b @Raise}} [UnstructuredGraph] exception when multiple nodes have the same id. *)
let valid_merge n1 n2 =
  let valid_relation rel = (
    match rel with
      | Some(rel') -> (
        match rel' with
          | Eq -> true
          | Neq -> false
      )
      | None -> true
  ) in (valid_relation (node_relation n1 n2)) && (valid_relation (node_relation n2 n1))
;;
(*
let add_node e

let build_consistency_graph pure_preds =
  let rec aux pure_preds' g =
    let add_pred pp g =
      match pp with
        | TrueB -> g
        | Comp(c_op, e1, e2) -> (
          match c_op with
            | Eq -> 
        )
              *)
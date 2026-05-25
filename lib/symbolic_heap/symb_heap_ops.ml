open Types
open Substitution
open Consistency

(** Merges symbolic heaps [sh1] and [sh2] by appending their respective list of existentially
    quantified variables, and their pure and spatial predicates, while being careful to 
    alpha-rename existentially quantified variables. No consistency checks are performed on
    the resulting heap. *)
let merge_symb_heap sh1 sh2 =
  let renamed_heap = (
    let rec aux lvar_list sh = (
      match lvar_list with
        | lv :: lv_list -> aux lv_list (if (List.mem lv sh.exists) then (alpha_rename sh lv) else sh)
        | [] -> sh
    ) in aux sh1.exists sh2
  ) in 
  {
    exists = sh1.exists @ renamed_heap.exists;
    pure = sh1.pure @ renamed_heap.pure;
    spatial = sh1.spatial @ renamed_heap.spatial
  }
;;

(** Creates the corresponding symbolic heap from pure predicates [pure_preds]. *)
let symbolic_heap_of_pure_preds pure_preds =
  {exists = []; pure = pure_preds; spatial = []}
;;

(** Creates the corresponding symbolic heap from spatial predicates [spat_preds]. *)
let symbolic_heap_of_spatial_preds spat_preds =
  {exists = []; pure = []; spatial = spat_preds}
;;

(** Checks whether spatial predicate [sp] represents an empty heap considering the information in [pure_preds].
    Given our current model only ls(e1,e2) with e1=e2 represent an empty heap. *)
let empty_predicate pure_preds sp =
		match sp with
			| List (e1, e2) -> (equal_expr pure_preds e1 e2)
			| _ -> false
;;

(** Checks whether spatial predicate [sp] is a [PointsTo] predicate. *)
let is_pointsto sp =
  match sp with
    | PointsTo (_, _) -> true
    | _ -> false
;;

(** Checks whether spatial predicate [sp] is a [Freed] predicate. *)
  let is_freed sp =
    match sp with
      | Freed _ -> true
      | _ -> false
;;

(** Checks whether spatial predicate [sp] is a [List] predicate. *)
let is_list sp =
  match sp with
    | List (_, _) -> true
    | _ -> false
;;

(** Returns the heap cell which spatial predicate [sp] describes. *)
let get_heap_cell sp =
  match sp with
    | PointsTo (e, _) -> Some e
    | Freed e -> Some e
    | List (e, _) -> Some e
    | TrueS -> None
;;

(** Returns true if spatial predicate [sp2] is a [PointsTo] predicate and describes the same heap cell
    as spatial predicate [sp1] according to the pure predicates [pure_preds]. *)
let same_heap_cell_pointsto pure_preds sp1 sp2 =
  match (get_heap_cell sp1), sp2 with
    | Some e1, PointsTo (e2, _) -> equal_expr pure_preds e1 e2
    | _ -> false
;;

(** Returns true if spatial predicate [sp2] is a [Freed] predicate and describes the same heap cell
    as spatial predicate [sp1] according to the pure predicates [pure_preds]. *)
let same_heap_cell_freed pure_preds sp1 sp2 =
  match (get_heap_cell sp1), sp2 with
    | Some e1, Freed e2 -> equal_expr pure_preds e1 e2
    | _ -> false
;;

(** Returns true if spatial predicate [sp2] is a [List] predicate and describes the same heap cell
    as spatial predicate [sp1] according to the pure predicates [pure_preds]. *)
let same_heap_cell_list pure_preds sp1 sp2 =
  match (get_heap_cell sp1), sp2 with
    | Some e1, List (e2, _) -> equal_expr pure_preds e1 e2
    | _ -> false
;;
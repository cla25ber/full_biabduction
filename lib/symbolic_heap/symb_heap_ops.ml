open Terms
open Substitution

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

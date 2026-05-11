(** Substitutes every element [el1] with [el2] of the list [list]. *)
let update_list list el1 el2 =
  List.map (fun v -> if (v = el1) then el2 else el1) list
;;
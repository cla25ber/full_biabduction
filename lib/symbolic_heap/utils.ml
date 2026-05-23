(** Substitutes every element [el1] with [el2] of the list [list]. *)
let update_list list el1 el2 =
  List.map (fun v -> if (v = el1) then el2 else el1) list
;;


let filter_with_flag pred list =
  List.fold_right (
    fun x (found, acc) ->
      if (pred x) then
        (true, x :: acc)
      else
        (found, acc)
  ) list (false, [])
;;

(** Returns the first element of the list [list] that satisfies the predicate [pred]
    together with the list without it. If no element satisfying [pred], [None] is returned.

  Da rendere tail recursive *)
let eliminate_first pred list =
  let rec aux list' =
    match list' with
      | [] -> None
      | el :: rest -> if (pred el) then Some (el, rest) else (
        let add_option el' (first, remaining) = (first, el' :: remaining) in
        Option.map (add_option el) (aux rest)
      )
  in aux list
;;

(** Takes a predicate [pred1] and a list [list1]. It finds the first element that satisfies the predicate and, using
    [pred2], a new specific predicate for the element foun is created. Then the first element of [list2] that satisfies
    the newly created predicate is searched. Finally, the quadruple containing the element found of the first list, the
    first list without it, the element found in the second list, and the second list without it. If the two were not found,
    [None] is returned. *)
let double_eliminate_first pred1 list1 pred2 list2 =
  let rec aux list =
    match list with
      | [] -> None
      | el :: rest -> (
        if (pred1 el) then (
          let res = eliminate_first (pred2 el) list2 in
          match res with
            | Some (el2, remaining2) -> Some (el, rest, el2, remaining2)
            | None -> None
        ) else (
          let add_option el' (found1, remaining1, found2, remaning2) = (found1, el' :: remaining1, found2, remaning2) in
          Option.map (add_option el) (aux rest)
        )
      )
  in aux list1
;;
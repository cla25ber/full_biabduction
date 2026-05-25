open Types

(** Using pure predicates [pure_preds], it checks whether expressions [e1] and [e2] are equal. *)
let equal_expr pure_preds e1 e2 =
  let rec closure checked to_check =
    match to_check with
      | [] -> false
      | e :: rest ->
        if (List.mem e checked) then (closure checked rest)
        else (
          (e = e2) || 
          let aux pp = (
            match pp with
              | Comp(Eq, e1', e2') -> (
                if (e1' = e && not (List.mem e2' (checked @ to_check))) then Some e2
                else if (e2' = e && not (List.mem e1' (checked @ to_check))) then Some e1 
                else None
              )
              | _ -> None
          ) in 
          let new_expr = List.filter_map aux pure_preds in
          closure (e :: checked) (rest @ new_expr)
        )
  in closure [] [e1]
;;

(** Using pure predicates [pure_preds], it checks whether spatial predicates [sp1] and [sp2] are equal. *)
let equal_spat_pred pure_preds sp1 sp2 =
  match sp1, sp2 with
    | (PointsTo (e1, e2), PointsTo (e3, e4)) -> (equal_expr pure_preds e1 e3) && (equal_expr pure_preds e2 e4)
    | (Freed e1, Freed e2) -> equal_expr pure_preds e1 e2
    | (List (e1, e2), List (e3, e4)) -> (equal_expr pure_preds e1 e3) && (equal_expr pure_preds e2 e4)
    | _ -> false
;;
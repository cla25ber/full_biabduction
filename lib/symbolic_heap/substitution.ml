open Terms

(** Substitutes every occurrence of [var1] with [var2] inside of expression [e]. *)
let rec subst_var_expr var1 var2 e =
  match e with
    | Ide v -> if (v = var1) then Ide(var2) else Ide(v)
    | Int n -> Int(n)
    | Binop (op, e1, e2) -> Binop(op, (subst_var_expr var1 var2 e1), (subst_var_expr var1 var2 e1))
;;

(** Substitutes every occurrence of [var1] with [var2] inside of pure predicate [pp]. *)
let subst_var_pure_pred var1 var2 pp =
  match pp with
    | TrueB -> TrueB
    | Comp (op, e1, e2) -> Comp(op, (subst_var_expr var1 var2 e1), (subst_var_expr var1 var2 e2))
    | PointsToP (e1, e2) -> PointsToP((subst_var_expr var1 var2 e1), (subst_var_expr var1 var2 e2))
;;

(** Substitutes every occurrence of [var1] with [var2] inside of spatial predicate [sp]. *)
let subst_var_spat_pred var1 var2 sp =
  match sp with
    | TrueS -> TrueS
    | PointsTo (e1, e2) -> PointsTo((subst_var_expr var1 var2 e1), (subst_var_expr var1 var2 e2))
    | Freed e -> Freed(subst_var_expr var1 var2 e)
    | List (e1, e2) -> List((subst_var_expr var1 var2 e1), (subst_var_expr var1 var2 e2))
;;

(** Substitutes every element [el1] with [el2] of the list [list]. *)
let update list el1 el2 =
  List.map (fun v -> if (v = el1) then el2 else el1) list
;;

(** Alpha-renames the logical variable [lvar] in the symbolic heap [sh]. If [lvar]
    is not present, the heap remains the same. *)
let alpha_rename sh lvar =
  if (not (List.mem lvar sh.exists)) then sh else (
    let new_lvar = fresh_lvar() in
      {exists = (update sh.exists lvar new_lvar); 
      pure = (List.map (subst_var_pure_pred (Lvar(lvar)) (Lvar(new_lvar))) sh.pure); 
      spatial = (List.map (subst_var_spat_pred (Lvar(lvar)) (Lvar(new_lvar))) sh.spatial)}
  )
;;

(** Collects all variables present in expression [e]. *)
let all_variables_expr e =
  let rec aux e' vars_list = (
    match e' with
      | Ide x -> x :: vars_list
      | Int n -> vars_list
      | Binop (op, e1, e2) -> (
        let vars1 = (aux e1 vars_list) in
          aux e2 vars1
      )
  ) in aux e []
;;

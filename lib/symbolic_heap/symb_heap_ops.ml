open Terms

(** Generates a new unused logical variable. *)
let fresh_lvar () =
  let id = !logical_counter in
    incr logical_counter; id
;;

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

let rename_existential existentials lvar1 lvar2 =
  List.map (fun v -> if (v = lvar1) then lvar2 else lvar1) existentials

let alpha_rename sh lvar =
  if (not (List.mem lvar sh.exists)) then sh else (
    let new_lvar = fresh_lvar() in
      {exists = (rename_existential sh.exists lvar new_lvar); 
      pure = (List.map (subst_var_pure_pred (Lvar(lvar)) (Lvar(new_lvar))) sh.pure); 
      spatial = (List.map (subst_var_spat_pred (Lvar(lvar)) (Lvar(new_lvar))) sh.spatial)}
  )
;;




(** Merges two symbolic heaps by appending their respective pure and spatial predicates,
    while being careful to alpha-rename existentially quantified variables. No consistency 
    checks are performed on the resulting heap. *)
let merge_symb_heap sh1 sh2 =
  failwith "To be implemented"
;;
open Types
open Utils

(** Checks whether variable [x] is a program or a logical variable. *)
let get_lvar x =
  match x with
    | Lvar n -> Some n
    | _ -> None
;;

(** Substitutes every occurrence of [var1] with [var2] inside of expression [e]. *)
let rec subst_var_expr var1 var2 e =
  match e with
    | Ide v -> if (v = var1) then Ide(var2) else Ide(v)
    | Int n -> Int(n)
    | Binop (op, e1, e2) -> Binop(op, (subst_var_expr var1 var2 e1), (subst_var_expr var1 var2 e2))
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

(** Alpha-renames the logical variable [lvar] in the symbolic heap [sh]. If [lvar]
    is not present, the heap remains the same. *)
let alpha_rename sh lvar =
  if (not (List.mem lvar sh.exists)) then sh else (
    let new_lvar = fresh_lvar() in
      {exists = (update_list sh.exists lvar new_lvar); 
      pure = (List.map (subst_var_pure_pred (Lvar(lvar)) (Lvar(new_lvar))) sh.pure); 
      spatial = (List.map (subst_var_spat_pred (Lvar(lvar)) (Lvar(new_lvar))) sh.spatial)}
  )
;;

(** Collects all variables present in expression [e] in a set, which is then returned. *)
let all_vars_expr e =
  let rec aux e' var_set = (
    match e' with
      | Ide x -> VarSet.add x var_set
      | Int n -> var_set
      | Binop (op, e1, e2) -> (
        let vars1 = (aux e1 var_set) in
          aux e2 vars1
      )
  ) in aux e VarSet.empty
;;

(** Collects all variables present in the list of pure predicates [pure_preds] in a set, which is then returned. *)
let all_vars_pure_preds pure_preds =
  let all_vars_atom_pure_pred pp =
    match pp with
      | TrueB -> VarSet.empty
      | Comp (_, e1, e2) -> VarSet.union (all_vars_expr e1) (all_vars_expr e2)
      | PointsToP (e1, e2) -> VarSet.union (all_vars_expr e1) (all_vars_expr e2)
  in List.fold_left (fun acc pp -> VarSet.union acc (all_vars_atom_pure_pred pp)) VarSet.empty pure_preds
;;

(** Collects all variables present in the list of spatial predicates [spat_preds] in a set, which is then returned. *)
let all_vars_spatial_preds spat_preds =
  let all_vars_atom_spat_pred sp =
    match sp with
      | TrueS -> VarSet.empty
      | PointsTo (e1, e2) -> VarSet.union (all_vars_expr e1) (all_vars_expr e2)
      | Freed e -> all_vars_expr e
      | List (e1, e2) -> VarSet.union (all_vars_expr e1) (all_vars_expr e2)
  in List.fold_left (fun acc sp -> VarSet.union acc (all_vars_atom_spat_pred sp)) VarSet.empty spat_preds
;;

(** Collects all variables present in the symbolic heap [sh] in a set, which is then returned. *)
let all_variables_symb_heap sh =
  VarSet.union (all_vars_pure_preds sh.pure) (all_vars_spatial_preds sh.spatial)
;;

(** Collects all free variables present in the symbolic heap [sh] in a set, which is then returned. *)
let free_variables_symb_heap sh =
  let set_of_list lst = List.fold_left (fun acc x -> VarSet.add x acc) VarSet.empty lst in
    let quantified_vars =  set_of_list (List.map (fun v -> Lvar(v)) sh.exists) in
      VarSet.diff (all_variables_symb_heap sh) quantified_vars
;;

(** Collets all free logical variables present in the symbolic heap [sh] in a set, which is then returned. *)
let free_logical_variables_symb_heap sh =
  let free_vars = free_variables_symb_heap sh in
  List.filter_map get_lvar (VarSet.elements free_vars)
;;
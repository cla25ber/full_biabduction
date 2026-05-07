open String

open Terms

(** A variable gets formatted according to its type:
    - [Pvar]: program variables gets formatted according to their name.
    - [Lvar]: logical variables gets formatted as _v[i] where [i] is their unique identifier.
*)
let format_var x =
  match x with
    | Pvar(x) -> x
    | Lvar(v) -> "_v" ^ (string_of_int v)
;;

(** Converts a [bin_op] to its symbolic string representation.
    - [Add] : "=".
    - [Sub] : "/=".
    - [Mul] : "•". *)
let format_binop op =
  match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "•"
;;

(** Pretty-prints an expression. *)
let rec format_expr e =
  match e with
    | Ide(id) -> format_var id
    | Int(n) -> string_of_int n
    | Binop(op, e1, e2) -> concat " " [(format_expr e1); (format_binop op); (format_expr e2)]
;;

(** Converts a [comp_op] to its symbolic string representation.
    - [Eq] : "=".
    - [Neq] : "/=". *)
let format_comp_op op =
    match op with
    | Eq -> "="
    | Neq -> "/="
;;

(** Pretty-prints a list of pure predicates as a string, joined with [&]. *)
let format_pure_pred pure_preds =
  let format_atom pp =
    match pp with
      | TrueB -> "true"
      | Comp (op, e1, e2) -> concat "" [(format_expr e1); (format_comp_op op); (format_expr e2)]
      | PointsTo (e1, e2) -> concat "" [(format_expr e1); "->"; (format_expr e2)]
  in
    match pure_preds with
      | [] -> "true"
      | preds -> concat " & " (List.map format_atom preds)
;;

(** Pretty-prints a list of spatial predicates as a string, joined with [*]. *)
let format_spat_pred spat_preds =
  let format_atom spat_pred =
    match spat_pred with
      | TrueS -> "true"
      | PointsTo (e1, e2) -> concat "" [(format_expr e1); "->"; (format_expr e2)]
      | Freed e -> concat "" [(format_expr e); "-/>"]
      | List (e1, e2) -> concat "" ["ls("; (format_expr e1); ","; (format_expr e2); ")"]
  in
    match spat_preds with
      | [] -> "emp"
      | preds -> concat " * " (List.map format_atom preds)
;;

(** Pretty-prints a symbolic heap [sh] in the format:
    [[pure_predicates | spatial_predicates]]. *)
let format_symb_heap sh =
  concat " " ["["; (format_pure_pred sh.pure); "|"; (format_spat_pred sh.spatial); "]"]
;;
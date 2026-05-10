open Types

(** Substitutes every element [el1] with [el2] of the list [list]. *)
let update list el1 el2 =
  List.map (fun v -> if (v = el1) then el2 else el1) list
;;

(** Transforms the integer [v] into the logical variable it represents. *)
let var_of_lvar v = Lvar(v);;

(** Transforms the string [x] into the program variable it represents. *)
let var_of_pvar x = Pvar(x);;

(** Compare expression [e1] with expression [e2]. The comparison is performed on terms of simplicity, 
    an integer is considered simpler than a program variable, which is simpler than a logical variable.
    
    BINARY OPERATION ARE NOT IMPLEMENTED YET. *)
let compare_expr e1 e2 =
  match e1, e2 with
    | Ide ide1, Ide ide2 -> (
      match ide1, ide2 with
        | Pvar _, Pvar _ -> 0
        | Pvar _, Lvar _ -> 1
        | Lvar _, Pvar _ -> -1
        | Lvar v1, Lvar v2 -> v1 - v2
    )
    | Ide _, Int _ -> -1
    | Ide _, Binop (_, _, _) -> failwith "Binary operations not implemented yet"
    | Int _, Ide _ -> 1
    | Int n1, Int n2 -> 0
    | Int _, Binop (_, _, _) -> failwith "Binary operations not implemented yet"
    | Binop (_, _, _), Ide _ -> failwith "Binary operations not implemented yet"
    | Binop (_, _, _), Int _ -> failwith "Binary operations not implemented yet"
    | Binop (_, _, _), Binop (_, _, _) -> failwith "Binary operations not implemented yet"
;;
open Types

(* REMEMBER TO CALL RANDOM.SELF_INIT SOMEWHERE. *)

(** Maximum integer allowed for expressions. *)
let max_int = 256 ;;

(** Generates a random integer used for expressions. *)
let rand_int () = Random.int max_int ;; 

(** Selects a random variable from [var_pool]. *)
let rand_var var_pool =
  var_pool.(Random.int (Array.length var_pool))
;;

(** Number of possible expressions in the assertion language (integers and identifiers). *)
let n_expr = 2 ;;

(** Generates a random expression using the variables from [var_pool]. *)
let rand_expr var_pool =
  match Random.int n_expr with
    | 0 -> Ide (rand_var var_pool)
    | 1 -> Int (rand_int ())
    | _ -> failwith "Unrecognized expression"
;;

(** Number of possible non-vacuous pure predicates in the assertion language (= and ≠). *)
let n_ppred = 2 ;;

(** Generates a random pure predicate using the variables from [var_pool].  *)
let rand_ppred var_pool =
  match Random.int n_ppred with
    | 0 -> Comp (Eq, Ide(rand_var var_pool), rand_expr var_pool)
    | 1 -> Comp (Neq, Ide(rand_var var_pool), rand_expr var_pool)
    | _ -> failwith "Unrecognized pure predicate"
;;

(** Generates a random list of [n_preds] pure predicates using the variables in [var_pool]. *)
let rec generate_ppreds n_preds var_pool =
  if (n_preds = 0) then [] else (rand_ppred var_pool)::(generate_ppreds (n_preds-1) var_pool)
;;

(** Number of possible spatial predicates in the assertion language (points-to, freed and list). *)
let n_spred = 3 ;;

(** Generates a random spatial predicate located at [current_var] using the variables from [var_pool]. *)
let rand_spred current_var var_pool =
  match Random.int n_spred with
    | 0 -> PointsTo (Ide(current_var), rand_expr var_pool)
    | 1 -> Freed (Ide(current_var))
    | 2 -> List (Ide(current_var), rand_expr var_pool)
    | _ -> failwith "Unrecognized spatial predicate"
;;

(** Generates a random list of [n_preds] spatial predicates using the variables in [var_pool].
  To avoid trivial inconsistencies, the location variable of the spatial predicate,
  i.e [current_var], cycles through [var_pool]. *)
let rec generate_spreds n_preds var_pool current =
  if (n_preds = 0) then [] else
    let pred = (rand_spred var_pool.(current) var_pool) in
    pred::(generate_spreds (n_preds-1) var_pool ((current+1) mod (Array.length var_pool)))
;;

exception InvalidHeapArgument of string ;;

(** Generates a random heap with [p_size] pure predicates and [s_size] spatial predicates.
  [v_size] fresh variables will be used for generations. *)
let rand_heap v_size p_size s_size =
  if (v_size <= 0) then raise (InvalidHeapArgument "The number of variables must be positive.");
  if (p_size < 0) then raise (InvalidHeapArgument "The number of pure predicates must be non-negative.");
  if (s_size < 0) then raise (InvalidHeapArgument "The number of spatial predicates must be non-negative.");

  let var_pool = Array.init v_size (fun _ -> Lvar(fresh_lvar())) in
  {
    exists = [];
    pure = generate_ppreds p_size var_pool;
    spatial = generate_spreds s_size var_pool 0
  }
;;
open Types
open Utils

(** Exception used for invalid input in constructing random heaps. *)
exception InvalidHeapArgument of string ;;

(** Maximum integer allowed for expressions. *)
let max_int = 256 ;;

(** Generates a random integer used for expressions. *)
let rand_int () = Random.int max_int ;; 

(** Generates a pool of variable of size [v_size]. 

    @raise InvalidHeapArgument if [v_size] is not a positive number greater than 0. *)
let generate_var_pool v_size = 
  if (v_size <= 0) then raise (InvalidHeapArgument "The number of variables must be positive.");

  Array.init v_size (fun _ -> Lvar(fresh_lvar()))
;;

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

(** Generates a random heap with [p_size] pure predicates and [s_size] spatial predicates,
    using variables from [var_pool]. 
    
    @raise InvalidHeapArgument if [p_size] or [s_size] are negative numbers. *)
let rand_heap var_pool p_size s_size =
  if (p_size < 0) then raise (InvalidHeapArgument "The number of pure predicates must be non-negative.");
  if (s_size < 0) then raise (InvalidHeapArgument "The number of spatial predicates must be non-negative.");

  {
    exists = [];
    pure = generate_ppreds p_size var_pool;
    spatial = generate_spreds s_size var_pool 0
  }
;;

(** Randomically modifies the pure predicate [pp] using variables from [var_pool]. *)
let modify_pure_pred pp var_pool = 
  match pp with
    | TrueB -> TrueB
    | Comp (Eq, e1, _) -> Comp (Eq, e1, (rand_expr var_pool))
    | Comp (Neq, e1, _) -> Comp (Neq, e1, (rand_expr var_pool))
;;

(** Randomically modifies the spatial predicate [sp] using variables from [var_pool]. *)
let modify_spat_pred pp var_pool = 
  match pp with
    | TrueS -> TrueS
    | PointsTo (e1, e2) -> (
      match Random.int 4 with
        | 0 | 1 -> PointsTo (e1, rand_expr var_pool)
        | 2 -> List(e1, e2)
        | 3 -> List(e1, rand_expr var_pool)
        | _ -> failwith "Impossible case"
    )
    | Freed (e) -> Freed(e)
    | List (e1, e2) -> (
      match Random.int 4 with
        | 0 | 1 -> List (e1, rand_expr var_pool)
        | 2 -> PointsTo(e1, e2)
        | 3 -> PointsTo(e1, rand_expr var_pool)
        | _ -> failwith "Impossible case"
    )
;;

(** Generates two symbolic heaps using the predicates present in the symbolic heap [heap], making them more similar
    to each other depending on the value of [simil]. The variables used are taken from [var_pool]. 
    
    @raise InvalidHeapArgument if [simil] is not a value between 0 and 1 (included). *)
let generate_smaller heap simil var_pool =
  if ((simil < 0.0) || (simil > 1.0)) then raise (InvalidHeapArgument "Similarity value must be between 0 and 1 included.");

  let rec select_pure_preds p_preds = (
    match p_preds with
      | [] -> ([], [])
      | pp :: preds ->
        if ((Random.float 1.0) <= simil) then (
          match Random.int 2 with
            | 0 -> cons_option_tuple (Some pp, Some pp) (select_pure_preds preds)
            | 1 -> select_pure_preds preds
            | _ -> failwith "Impossible case"
        ) else (
          let pp1 = (
            match Random.int 3 with
              | 0 -> Some pp
              | 1 -> None
              | 2 -> Some (modify_pure_pred pp var_pool)
              | _ -> failwith "Impossible case"
          ) in
          let pp2 = (
            match Random.int 3 with
              | 0 -> Some pp
              | 1 -> None
              | 2 -> Some (modify_pure_pred pp var_pool)
              | _ -> failwith "Impossible case"
          ) in
          cons_option_tuple (pp1, pp2) (select_pure_preds preds)
        )
  ) in
  let rec select_spat_preds s_preds = (
    match s_preds with
      | [] -> ([], [])
      | sp :: preds ->
        if ((Random.float 1.0) <= simil) then (
          match Random.int 2 with
            | 0 -> cons_option_tuple (Some sp, Some sp) (select_spat_preds preds)
            | 1 -> select_spat_preds preds
            | _ -> failwith "Impossible case"
        ) else (
          let sp1 = (
            match Random.int 3 with
              | 0 -> Some sp
              | 1 -> None
              | 2 -> Some (modify_spat_pred sp var_pool)
              | _ -> failwith "Impossible case"
          ) in
          let sp2 = (
            match Random.int 3 with
              | 0 -> Some sp
              | 1 -> None
              | 2 -> Some (modify_spat_pred sp var_pool)
              | _ -> failwith "Impossible case"
          ) in
          cons_option_tuple (sp1, sp2) (select_spat_preds preds)
        )
  ) in
  let (pure_preds1, pure_preds2) = select_pure_preds heap.pure in
  let (spat_preds1, spat_preds2) = select_spat_preds heap.spatial in
  ({
    exists = [];
    pure = pure_preds1;
    spatial = spat_preds1
  },
  {
    exists = [];
    pure = pure_preds2;
    spatial = spat_preds2
  })
;;
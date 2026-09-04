open Symbolic_heap.Formatting
open Symbolic_heap.Random_heaps
open Full_biabduction.Fullbiabduction
open Full_biabduction.Rules

(** Usage message for the command. *)
let usage_msg = "Usage: random_experiment2 <v_size> <p_size1> <s_size1> <p_size2> <s_size2> [options]" ;;

let time_setting = ref false ;;
let verbose_setting = ref false ;;
let args = ref [] ;;

(** Command options:
  - [--time] shows execution time of the experiment.
  - [--verbose] shows the generated heaps and solution found by the algorithm. *)
let options = [
  ("--time", Arg.Set time_setting, "Show execution time");
  ("--verbose", Arg.Set verbose_setting, "Show generated heaps and solution")
] ;;

(**Collects the arguments from the command line. *)
let anon_arg arg =
  args := arg :: !args
;;

let () = Random.self_init () ;;

let () =
  try

    Arg.parse options anon_arg usage_msg;

    let args = List.rev !args in

    match args with
    | [v; p1; s1; p2; s2] ->
      let v_size = int_of_string v in
      let p_size1 = int_of_string p1 in
      let s_size1 = int_of_string s1 in
      let p_size2 = int_of_string p2 in
      let s_size2 = int_of_string s2 in

      (* Generates variable pool *)
      let var_pool = generate_var_pool v_size in

      (* Generates heaps *)
      let heap1 = rand_heap var_pool p_size1 s_size1 in
      let heap2 = rand_heap var_pool p_size2 s_size2 in

      if !verbose_setting then (
        print_endline "\nInput:";
        print_endline ("   Heap1:  " ^ (format_symb_heap heap1));
        print_endline ("   Heap2:  " ^ (format_symb_heap heap2));
        print_endline "\nRules applied:";
      );

      let start = Unix.gettimeofday() in

      let result = (full_biabduction ~verbose:!verbose_setting ruleSet1 heap1 heap2) in

      let finish = Unix.gettimeofday() in

      if !verbose_setting then (
        print_endline (format_fullbiabduction_result result);
      );

      print_endline "\nResults of the experiment:";
      
      (
      match result with
        | Some res -> print_endline (String.concat "\n" 
        [
          "   Success: yes";
          "   Antiframe:";
          "      Pure predicates:    " ^ string_of_int (List.length res.antiframe.pure);
          "      Spatial predicates: " ^ string_of_int (List.length res.antiframe.spatial);
          "      Refinements:        " ^ string_of_int (List.length res.refinements1);
          "   Frame:";
          "      Pure predicates:    " ^ string_of_int (List.length res.frame.pure);
          "      Spatial predicates: " ^ string_of_int (List.length res.frame.spatial);
          "      Refinements:        " ^ string_of_int (List.length res.refinements2);
        ])
        | None -> print_endline "   Success: no"
      );

      if !time_setting then (
        Printf.printf "   Execution time: %.6f seconds\n" (finish -. start)
      );

      print_endline ""

    | _ ->
      Printf.eprintf "Expected: <v_size> <p_size1> <s_size1> <p_size2> <s_size2>\n";
      exit 1

  with 
    |InvalidHeapArgument msg -> 
      Printf.eprintf "%s\n" msg;
      exit 1
    | Failure msg ->
      Printf.eprintf "%s\n" msg;
      exit 1
;;
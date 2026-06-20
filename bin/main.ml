open Symbolic_heap.Types
open Symbolic_heap.Formatting
open Full_biabduction.Fullbiabduction
open Full_biabduction.Rules

let x = Ide(Pvar("x")) ;;
let y = Ide(Pvar("y")) ;;
let z = Ide(Pvar("z")) ;;
let w = Ide(Pvar("w")) ;;
let l = Ide(Lvar(fresh_lvar())) ;;


(* EXAMPLE 4.1 *)

let sh1 = 
    {exists = []; 
    pure = []; 
    spatial = [PointsTo(x, y); PointsTo(y, z);]}
;;
let sh2 = 
  {exists = []; 
  pure = []; 
  spatial = [PointsTo(x, w);]} 
;;


(* EXAMPLE 4.2 *)

let sh3 = 
    {exists = []; 
    pure = []; 
    spatial = [List(x, y)]}
;;
let sh4 = 
  {exists = []; 
  pure = []; 
  spatial = [PointsTo(x, l);]} 
;;


(* EXAMPLE 4.3 *)

let sh5 = 
    {exists = []; 
    pure = []; 
    spatial = [PointsTo(x, y); List(y,z)]}
;;
let sh6 = 
  {exists = []; 
  pure = []; 
  spatial = [List(x,y); PointsTo(y, z);]} 
;;

(* EXAMPLES EXECUTION *)

let () = 
  print_endline "\nExample 4.1:";
  print_endline "\nInput:";
  print_endline ("   Heap1:  " ^ (format_symb_heap sh1));
  print_endline ("   Heap2:  " ^ (format_symb_heap sh2));
  (*print_endline (String.concat " " ["  "; (format_symb_heap sh1); "◁ ▷"; (format_symb_heap sh2)]);*)
  print_endline "\nRules applied:";
  print_endline (format_fullbiabduction_result (full_biabduction ~verbose:true ruleSet1 sh1 sh2));
  print_endline "";

  print_endline "\nExample 4.2:";
  print_endline "\nInput:";
  print_endline ("   Heap1:  " ^ (format_symb_heap sh3));
  print_endline ("   Heap2:  " ^ (format_symb_heap sh4));
  (*print_endline (String.concat " " ["  "; (format_symb_heap sh3); "◁ ▷"; (format_symb_heap sh4)]);*)
  print_endline "\nRules applied:";
  print_endline (format_fullbiabduction_result (full_biabduction ~verbose:true ruleSet1 sh3 sh4));
  print_endline "";

  print_endline "\nExample 4.3:";
  print_endline "\nInput:";
  print_endline ("   Heap1:  " ^ (format_symb_heap sh5));
  print_endline ("   Heap2:  " ^ (format_symb_heap sh6));
  (*print_endline (String.concat " " ["  "; (format_symb_heap sh5); "◁ ▷"; (format_symb_heap sh6)]);*)
  print_endline "\nRules applied:";
  print_endline (format_fullbiabduction_result (full_biabduction ~verbose:true ruleSet1 sh5 sh6));
  print_endline ""
;;

(* ◁ ▷ *)
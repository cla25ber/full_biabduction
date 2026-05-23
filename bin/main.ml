open Symbolic_heap.Types
open Full_biabduction.Fullbiabduction
open Full_biabduction.Rules

let sh1 = {
  exists = [];
  pure = [];
  spatial = [PointsTo(Ide(Pvar("x")), Ide(Pvar("y"))); List(Ide(Pvar("y")), Ide(Pvar("z")))]
} ;;
let sh2 = {
  exists = [];
  pure = [];
  spatial = [List(Ide(Pvar("x")), Ide(Pvar("y"))); PointsTo(Ide(Pvar("y")), Ide(Pvar("z")))]
} ;;

let () = 
  match full_biabduction ruleSet1 sh1 sh2 with
    | None -> print_endline "Bruh"
    | Some res -> print_endline (format_fullbiabduction_result res)
;;
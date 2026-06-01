open Symbolic_heap.Types
open Full_biabduction.Fullbiabduction
open Full_biabduction.Rules

(*let sh1 = {
  exists = [];
  pure = [];
  spatial = [PointsTo(Ide(Pvar("x")), Ide(Pvar("y"))); List(Ide(Pvar("y")), Ide(Pvar("z")))]
} ;;
let sh2 = {
  exists = [];
  pure = [];
  spatial = [List(Ide(Pvar("x")), Ide(Pvar("y"))); PointsTo(Ide(Pvar("y")), Ide(Pvar("z")))]
} ;;*)

let sh3 = {
  exists = [];
  pure =[Comp(Eq, Ide(Pvar("y")), Ide(Lvar(0)))];
  spatial = [PointsTo(Ide(Pvar("v")), Ide(Lvar(0)))]
} ;;

let sh4 = {
  exists = [];
  pure = [];
  spatial = [PointsTo(Ide(Pvar("y")), Ide(Lvar(1)))]
} ;;

let sh5 = {
  exists = [];
  pure =[Comp(Eq, Ide(Pvar("y")), Ide(Lvar(0)))];
  spatial = [PointsTo(Ide(Pvar("v")), Ide(Lvar(0))); Freed(Ide(Pvar("y")))]
} ;;

let sh6 = {
  exists = [];
  pure = [Comp(Eq, Ide(Pvar("y")), Ide(Lvar(2)))];
  spatial = []
} ;;

let sh7 = {
  exists = [];
  pure =[Comp(Eq, Ide(Lvar(0)), Ide(Lvar(2)))];
  spatial = [PointsTo(Ide(Pvar("y")), Ide(Lvar(3))); PointsTo(Ide(Pvar("v")), Ide(Lvar(0))); Freed(Ide(Lvar(2)))]
} ;;

let sh8 = {
  exists = [];
  pure = [];
  spatial = [PointsTo(Ide(Pvar("v")), Ide(Lvar(4)))]
} ;;


let () = 
  print_endline (format_fullbiabduction_result (full_biabduction ruleSet1 sh3 sh4));
  print_endline "";
  print_endline (format_fullbiabduction_result (full_biabduction ruleSet1 sh5 sh6));
  print_endline "";
  print_endline (format_fullbiabduction_result (full_biabduction ruleSet1 sh7 sh8));
;;

let sh9 = {
  exists = [];
  pure =[];
  spatial = [PointsTo(Ide(Pvar("v")), Ide(Lvar(0)))]
} ;;

let sh10 = {
  exists = [];
  pure = [];
  spatial = [PointsTo(Ide(Pvar("y")), Ide(Lvar(1)))]
} ;;

let sh11 = {
  exists = [];
  pure =[Comp(Eq, Ide(Pvar("y")), Ide(Lvar(100)))];
  spatial = [PointsTo(Ide(Pvar("v")), Ide(Lvar(0)))]
} ;;

let sh12 = {
  exists = [];
  pure = [];
  spatial = [Freed(Ide(Pvar("y")))]
} ;;

let sh13 = {
  exists = [];
  pure =[Comp(Eq, Ide(Pvar("y")), Ide(Lvar(100)))];
  spatial = [PointsTo(Ide(Pvar("y")), Ide(Lvar(2))); PointsTo(Ide(Pvar("v")), Ide(Lvar(0)))]
} ;;

let sh14 = {
  exists = [];
  pure = [Comp(Eq, Ide(Pvar("y")), Ide(Lvar(3)))];
  spatial = [PointsTo(Ide(Pvar("v")), Ide(Lvar(3)))]
} ;;

let () = 
  print_endline (format_fullbiabduction_result (full_biabduction ruleSet1 sh9 sh10));
  print_endline "";
  print_endline (format_fullbiabduction_result (full_biabduction ruleSet1 sh11 sh12));
  print_endline "";
  print_endline (format_fullbiabduction_result (full_biabduction ruleSet1 sh13 sh14));
  print_endline "";
;;
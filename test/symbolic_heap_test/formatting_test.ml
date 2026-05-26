open Symbolic_heap.Types
open Symbolic_heap.Formatting

open OUnit2

let assert_string_equal expected actual =
  assert_equal
    ~printer:(fun s -> s)
    expected
    actual
;;

let test_expr_format _ =
  let e1 = Int 2 in
  let e2 = Ide (Pvar "x") in
  let e3 = Ide (Lvar 0) in
  let e4 = Ide (Lvar 1) in
  let e5 = Binop(Add, e1, Binop(Sub, Binop(Mul, e2, e3), e2)) in

  assert_string_equal "2" (format_expr e1);
  assert_string_equal "x" (format_expr e2);
  assert_string_equal "_v0" (format_expr e3);
  assert_string_equal "_v1" (format_expr e4);
  assert_string_equal "(2 + ((x • _v0) - x))" (format_expr e5)
;;

let test_pure_format _ =
  let e1 = Int 2 in
  let e3 = Ide (Lvar 0) in
  let e4 = Ide (Lvar 1) in

  let pp1 = TrueB in
  let pp2 = Comp (Eq, e1, e3) in
  let pp3 = Comp (Neq, Ide (Pvar "x"), e4) in
  let pp4 = PointsToP (Ide (Pvar "y"), e1) in

  assert_string_equal "true" (format_pure_pred []);
  assert_string_equal "true" (format_pure_pred [pp1]);
  assert_string_equal "x≠_v1 & 2=_v0 & true" (format_pure_pred [pp3; pp2; pp1]);
  assert_string_equal "y~2 & x≠_v1" (format_pure_pred [pp4; pp3])
;;

let test_spatial_format _ =
  let x = Ide (Pvar "x") in
  let y = Ide (Pvar "y") in
  let z = Ide (Pvar "z") in
  let w = Ide (Pvar "w") in

  let e1 = Int 2 in

  let sp1 = TrueS in
  let sp2 = PointsTo (x, e1) in
  let sp3 = List (y, z) in
  let sp4 = Freed w in

  assert_string_equal "emp" (format_spat_pred []);
  assert_string_equal "x→2" (format_spat_pred [sp2]);
  assert_string_equal "ls(y,z) * w↓" (format_spat_pred [sp3; sp4]);
  assert_string_equal "w↓ * x→2 * true" (format_spat_pred [sp4; sp2; sp1])
;;

let test_symb_heap_format _ =
  let x = Ide(Pvar("x")) in
  let y = Ide(Pvar("y")) in
  let z = Ide(Pvar("z")) in
  let w = Ide(Pvar("w")) in
  let e = Ide(Pvar("e")) in
  let f = Ide(Pvar("f")) in
  let k = Ide(Pvar("k")) in
  let v0 = Ide(Lvar 0) in
  let v1 = Ide(Lvar 1) in

  let sh1 = 
    {exists = []; 
    pure = [Comp(Eq, Int 2, Int(2))]; 
    spatial = [PointsTo(x, Int(5))]} in
  let sh2 = 
    {exists = []; 
    pure = [Comp(Neq, v0, Int(2))]; 
    spatial = [Freed(y); PointsTo(x, Int(5))]} in
  let sh3 = 
    {exists = [1]; 
    pure = [Comp(Eq, e, f); Comp(Neq, v1, Int(42))]; 
    spatial = [PointsTo(x, Int(6)); PointsTo(e, Int(5)); PointsTo(y, Int(7))]} in
  let sh4 = 
    {exists = []; 
    pure = [Comp(Eq, Int 2, Ide(Lvar 2))]; 
    spatial = [PointsTo(w, Int(5)); PointsTo(z, Int(5)); List(k, Int(7))]} in
  let sh5 = 
    {exists = [0; 1]; 
    pure = [Comp(Neq, Ide(Pvar "x"), v1)]; 
    spatial = [PointsTo(v0, Int(5)); Freed(f)]} in

  assert_string_equal "[ true | emp ]" (format_symb_heap empty_sh);
  assert_string_equal "[ true | true ]" (format_symb_heap true_sh);
  assert_string_equal "[ 2=2 | x→5 ]" (format_symb_heap sh1);
  assert_string_equal "[ _v0≠2 | y↓ * x→5 ]" (format_symb_heap sh2);
  assert_string_equal "∃(_v1).[ e=f & _v1≠42 | x→6 * e→5 * y→7 ]" (format_symb_heap sh3);
  assert_string_equal "[ 2=_v2 | w→5 * z→5 * ls(k,7) ]" (format_symb_heap sh4);
  assert_string_equal "∃(_v0,_v1).[ x≠_v1 | _v0→5 * f↓ ]" (format_symb_heap sh5)
;;

let suite =
  "Format tests" >::: [
    "expr_format" >:: test_expr_format;
    "pure_format" >:: test_pure_format;
    "spatial_format" >:: test_spatial_format;
    "symbolic_heap_format" >:: test_symb_heap_format;
  ]

let () =
  run_test_tt_main suite
;;

(*
print_endline "\nTesting symbolic heaps unions";;
print_endline ("sh1 + sh2 = "^(format_symb_heap (merge_symb_heap sh1 sh2)));;
print_endline ("sh3 + sh5 = "^(format_symb_heap (merge_symb_heap sh3 sh5)));;
print_endline ("sh4 + sh6 = "^(format_symb_heap (merge_symb_heap sh4 sh6)));;
*)
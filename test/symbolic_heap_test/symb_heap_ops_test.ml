open Symbolic_heap.Types
open Symbolic_heap.Symb_heap_ops
open Symbolic_heap.Formatting

open OUnit2

let assert_symb_heap_equal expected actual =
  assert_equal
    ~printer:format_symb_heap
    expected
    actual
;;

let assert_expr_equal expected actual =
  assert_equal
    ~printer:format_expr
    expected
    actual
;;

let test_merge_heap _ =
  let x = Ide(Pvar("x")) in
  let y = Ide(Pvar("y")) in
  let z = Ide(Pvar("z")) in
  let w = Ide(Pvar("w")) in
  let e = Ide(Pvar("e")) in
  let f = Ide(Pvar("f")) in
  let k = Ide(Pvar("k")) in
  let v0 = Ide(Lvar(fresh_lvar ())) in
  let v1 = Ide(Lvar(fresh_lvar ())) in

  let sh1 = 
    {exists = []; 
    pure = [Comp(Eq, Int 2, Int(2))]; 
    spatial = [PointsTo(x, Int(5))]} in
  let sh2 = 
    {exists = [1]; 
    pure = [Comp(Neq, v0, Int(2))]; 
    spatial = [Freed(y); PointsTo(x, Int(5))]} in
  let sh3 = 
    {exists = [1]; 
    pure = [Comp(Eq, e, f); Comp(Neq, v1, Int(42))]; 
    spatial = [PointsTo(x, Int(6)); PointsTo(v0, Int(5)); PointsTo(y, Int(7))]} in
  let sh4 = 
    {exists = []; 
    pure = [Comp(Eq, Int 2, v0)]; 
    spatial = [PointsTo(w, Int(5)); PointsTo(z, Int(5)); List(k, Int(7))]} in
  let sh5 = 
    {exists = [0; 1]; 
    pure = [Comp(Neq, Ide(Pvar "x"), v1)]; 
    spatial = [PointsTo(v0, Int(5)); Freed(f)]} in

  assert_symb_heap_equal empty_sh (merge_symb_heap empty_sh empty_sh);
  assert_symb_heap_equal true_sh (merge_symb_heap true_sh empty_sh);
  assert_symb_heap_equal {sh1 with spatial = [PointsTo(x, Int(5)); TrueS]} (merge_symb_heap sh1 true_sh);
  assert_symb_heap_equal {exists = [1];
                          pure = [Comp(Eq, Int 2, Int(2)); Comp(Neq, v0, Int(2))];
                          spatial = [PointsTo(x, Int(5)); Freed(y); PointsTo(x, Int(5))]} 
                        (merge_symb_heap sh1 sh2);
  assert_symb_heap_equal {exists = [0; 1];
                          pure = [Comp(Eq, Int 2, Int(2)); Comp(Neq, Ide(Pvar "x"), v1)];
                          spatial = [PointsTo(x, Int(5)); PointsTo(v0, Int(5)); Freed(f)]} 
                        (merge_symb_heap sh1 sh5);
  assert_symb_heap_equal {exists = [1; 2];
                          pure = [Comp(Neq, v0, Int(2)); Comp(Eq, e, f); Comp(Neq, Ide(Lvar(2)), Int(42))];
                          spatial = [Freed(y); PointsTo(x, Int(5)); PointsTo(x, Int(6)); PointsTo(v0, Int(5)); PointsTo(y, Int(7))]} 
                        (merge_symb_heap sh2 sh3);
  assert_symb_heap_equal {exists = [1; 3; 4];
                          pure = [Comp(Neq, v0, Int(2)); Comp(Neq, Ide(Pvar "x"), Ide(Lvar(4)))];
                          spatial = [Freed(y); PointsTo(x, Int(5)); PointsTo(Ide(Lvar(3)), Int(5)); Freed(f)]} 
                        (merge_symb_heap sh2 sh5);
  assert_symb_heap_equal {exists = [1];
                          pure = [Comp(Eq, e, f); Comp(Neq, v1, Int(42)); Comp(Eq, Int 2, v0)];
                          spatial = [PointsTo(x, Int(6)); PointsTo(v0, Int(5)); PointsTo(y, Int(7)); PointsTo(w, Int(5)); PointsTo(z, Int(5)); List(k, Int(7))]} 
                        (merge_symb_heap sh3 sh4);
  assert_symb_heap_equal {exists = [1; 5; 6];
                          pure = [Comp(Eq, e, f); Comp(Neq, v1, Int(42)); Comp(Neq, Ide(Pvar "x"), Ide(Lvar(6)))];
                          spatial = [PointsTo(x, Int(6)); PointsTo(v0, Int(5)); PointsTo(y, Int(7)); PointsTo(Ide(Lvar(5)), Int(5)); Freed(f)]} 
                        (merge_symb_heap sh3 sh5);
  assert_symb_heap_equal {exists = [7; 1];
                          pure = [Comp(Eq, Int 2, v0); Comp(Neq, Ide(Pvar "x"), v1)];
                          spatial = [PointsTo(w, Int(5)); PointsTo(z, Int(5)); List(k, Int(7)); PointsTo(Ide(Lvar(7)), Int(5)); Freed(f)]} 
                        (merge_symb_heap sh4 sh5);
;;

let test_heap_cell _ =
  let x = Ide (Pvar "x") in
  let y = Ide (Pvar "y") in
  let z = Ide (Pvar("z")) in
  let w = Ide (Pvar("w")) in
  let v0 = Ide (Lvar(0)) in
  let v1 = Ide (Lvar(1)) in

  let pp0 = [Comp(Eq, x, x)] in
  let pp1 = [Comp(Eq, x, y)] in
  let pp2 = [Comp(Eq, y, x)] in
  let pp3 = [Comp(Eq, x, y); Comp(Eq, y, z)] in
  let pp4 = [Comp(Eq, v0, x); Comp(Eq, x, v1)] in
  let pp5 = [Comp(Eq, v0, x); Comp(Eq, v1, x)] in
  let pp6 = [Comp(Neq, x, y)] in
  let pp7 = [Comp(Eq, v0, x); Comp(Eq, y, x); Comp(Eq, z, y); Comp(Eq, z, v1)] in
  let pp8 = [Comp(Eq, x, y); Comp(Eq, z, y); Comp(Neq, w, z); Comp(Eq, v0, v1); Comp(Eq, w, v1)] in

  let sp1 = PointsTo(x, y) in
  let sp2 = PointsTo(x, z) in
  let sp3 = PointsTo(y, z) in
  let sp4 = List(x, y) in
  let sp5 = Freed x in
  let sp6 = Freed v0 in
  let sp7 = List(y, v0) in
  let sp8 = PointsTo(y, v1) in
  let sp9 = PointsTo(v0, v1) in
  let sp10 = PointsTo(x, v0) in
  let sp11 = PointsTo(x, w) in
  let sp12 = PointsTo(z, v1) in 
  let sp13 = Freed v1 in

  assert (same_heap_cell_pointsto pp0 sp1 sp1);
  assert (same_heap_cell_pointsto [] sp1 sp1);
  assert (same_heap_cell_pointsto [] sp1 sp2);
  assert (not (same_heap_cell_pointsto [] sp2 sp3));
  assert (same_heap_cell_pointsto pp1 sp2 sp3);
  assert (same_heap_cell_pointsto pp2 sp2 sp3);
  assert (not (same_heap_cell_pointsto [] sp1 sp4));
  assert (not (same_heap_cell_pointsto [] sp1 sp5));
  assert (same_heap_cell_freed [] sp1 sp5);
  assert (same_heap_cell_list [] sp1 sp4);
  assert (same_heap_cell_pointsto pp3 sp1 sp12);
  assert (same_heap_cell_freed pp4 sp5 sp6);
  assert (not (same_heap_cell_pointsto pp6 sp1 sp3));
  assert (not (same_heap_cell_pointsto pp6 sp4 sp7));
  assert (same_heap_cell_freed pp5 sp6 sp13);
  assert (same_heap_cell_pointsto pp7 sp8 sp9);
  assert (same_heap_cell_pointsto pp8 sp1 sp10);
  assert (same_heap_cell_pointsto pp8 sp1 sp11);
  assert (same_heap_cell_pointsto pp8 sp10 sp12);
;;

let suite =
  "Symbolic heap operations tests" >::: [
    "merging_symboic_heaps" >:: test_merge_heap;
    "heap_cell_recognition" >:: test_heap_cell
  ]

let () =
  run_test_tt_main suite
;;
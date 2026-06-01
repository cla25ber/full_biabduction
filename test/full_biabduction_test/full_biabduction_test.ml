open Symbolic_heap.Types
open Symbolic_heap.Symb_heap_ops

open Full_biabduction.Rules
open Full_biabduction.Fullbiabduction

open OUnit2

let assert_fullbiabduction_result_equal expected actual =
  assert_equal
    ~printer:format_fullbiabduction_result
    expected
    actual
;;

let test_full_biabduction _ =
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
    pure = []; 
    spatial = [Freed(y); PointsTo(k, Int(5)); List(w, z)]} in
  let sh2 = 
    {exists = []; 
    pure = []; 
    spatial = [Freed(y); PointsTo(x, Int(5)); Freed(f)]} in
  let sh3 = 
    {exists = []; 
    pure = [Comp(Eq, e, f); Comp(Neq, v1, Int(42))]; 
    spatial = [PointsTo(x, Int(6)); List(v0, Int(25)); PointsTo(y, Int(67)); Freed(k)]} in
  let sh4 = 
    {exists = []; 
    pure = [Comp(Eq, Int 2, v0)]; 
    spatial = [List(w, Int(3)); PointsTo(z, Int(1)); Freed(k); Freed(e)]} in
  let sh5 = 
    {exists = []; 
    pure = []; 
    spatial = [PointsTo(x, y); List(w, y); PointsTo(e, k)]} in
  let sh6 = 
    {exists = []; 
    pure = []; 
    spatial = [PointsTo(x, w); PointsTo(e, f); List(f, k)]} in
  let sh7 =
    {exists = []; 
    pure = []; 
    spatial = [List(x, y)]} in
  let sh8 = 
    {exists = []; 
    pure = []; 
    spatial = [List(x, y); PointsTo(e, w)]} in
  let sh9 =
    {exists = []; 
    pure = []; 
    spatial = [PointsTo(x, z); List(e, f)]} in
  let sh10 =
    {exists = []; 
    pure = []; 
    spatial = [PointsTo(x, z); PointsTo(z, k)]} in

  let result = {
    refinements1 = [];
    antiframe = empty_sh;
    refinements2 = [];
    frame = empty_sh
  } in

  assert_fullbiabduction_result_equal (Some result) (full_biabduction ruleSet1 empty_sh empty_sh);
  assert_fullbiabduction_result_equal (Some {result with
                                              antiframe = symbolic_heap_of_spatial_preds [PointsTo(x, Int(5)); Freed(f)];
                                              frame = symbolic_heap_of_spatial_preds [PointsTo(k, Int(5)); List(w, z)]}
                                      ) (full_biabduction ruleSet1 sh1 sh2);
  assert_fullbiabduction_result_equal (Some {result with
                                              antiframe = { empty_sh with
                                                pure = [Comp(Eq, Int 2, v0)];
                                                spatial = [List(w, Int(3)); PointsTo(z, Int(1)); Freed(e)]};
                                              frame = { empty_sh with
                                                pure = [Comp(Eq, e, f); Comp(Neq, v1, Int(42))];
                                                spatial = [PointsTo(x, Int(6)); List(v0, Int(25)); PointsTo(y, Int(67))]}}
                                      ) (full_biabduction ruleSet1 sh3 sh4);
  assert_fullbiabduction_result_equal (Some {result with
                                              antiframe =  symbolic_heap_of_pure_preds [Comp(Eq, y, w); Comp(Eq, k, f); Comp(Eq, k, f); Comp(Eq, y, w)];
                                              frame = symbolic_heap_of_pure_preds [Comp(Eq, y, w); Comp(Eq, k, f); Comp(Eq, k, f); Comp(Eq, y, w)]}
                                      ) (full_biabduction ruleSet1 sh5 sh6);
  assert_fullbiabduction_result_equal (Some result) (full_biabduction ruleSet1 sh7 sh7);
  assert_fullbiabduction_result_equal (Some result) (full_biabduction ruleSet2 sh7 sh7);
  assert_fullbiabduction_result_equal (Some {
                                              antiframe = { empty_sh with
                                                pure = [Comp(Neq, x, y); Comp(Neq, e, f); Comp(Neq, e, f); Comp(Neq, x, y)];
                                                spatial = [List(w, f)]};
                                              refinements1 = [PointsToP(x, z)];
                                              frame = { empty_sh with
                                                pure = [Comp(Neq, x, y); Comp(Neq, e, f); Comp(Neq, e, f); Comp(Neq, x, y)];
                                                spatial = [List(z, y)]};
                                              refinements2 = [PointsToP(e,w)]}
                                      ) (full_biabduction ruleSet1 sh8 sh9);
  assert_fullbiabduction_result_equal (Some { result with
                                              antiframe = symbolic_heap_of_pure_preds [Comp(Neq, x, y); Comp(Neq, z, y); Comp(Neq, z, y); Comp(Neq, x, y)];
                                              refinements1 = [PointsToP(x, z); PointsToP(z, k)];
                                              frame = { empty_sh with
                                                pure = [Comp(Neq, x, y); Comp(Neq, z, y); Comp(Neq, z, y); Comp(Neq, x, y)];
                                                spatial = [List(k, y); PointsTo(e, w)]}}
                                      ) (full_biabduction ruleSet1 sh8 sh10);
;;

let suite =
  "Full BiAbduction tests" >::: [
    "Full_BiAbduction" >:: test_full_biabduction;
  ]

let () =
  run_test_tt_main suite
;;
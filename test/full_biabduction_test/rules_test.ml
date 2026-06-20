open Symbolic_heap.Types
open Symbolic_heap.Symb_heap_ops

open Full_biabduction.Rules

open OUnit2

let assert_rule_result_equal expected actual =
  assert_equal
    ~printer:format_rule_result
    expected
    actual
;;

let test_base_emp _ =
  let x = Ide(Pvar("x")) in
  let v0 = Ide(Lvar(fresh_lvar ())) in
  let v1 = Ide(Lvar(fresh_lvar ())) in

  let sh1 = 
    {exists = []; 
    pure = [Comp(Eq, Int 2, Int(2))]; 
    spatial = []} in
  let sh2 = 
    {exists = []; 
    pure = [Comp(Neq, v0, Int(2))]; 
    spatial = []} in
  let sh3 = 
    {exists = []; 
    pure = [Comp(Neq, v1, Int(42))]; 
    spatial = [PointsTo(x, Int(6))]} in
  
  let result ={
    refinements1 = [];
    heap1 = empty_sh;
    antiframe = empty_sh;
    refinements2 = [];
    heap2 = empty_sh;
    frame = empty_sh;
    axiom = true;
    name = "base-emp"
  } in

  assert_rule_result_equal (Some result) (base_emp empty_sh empty_sh);
  assert_rule_result_equal None (base_emp true_sh empty_sh);
  assert_rule_result_equal None (base_emp true_sh true_sh); 
  assert_rule_result_equal (Some {result with antiframe = sh2; frame = sh1}) (base_emp sh1 sh2); 
  assert_rule_result_equal None (base_emp sh1 sh3); 
;;

let test_remove _ =
  let x = Ide(Pvar("x")) in
  let y = Ide(Pvar("y")) in
  let v0 = Ide(Lvar(fresh_lvar ())) in

  let sh1 = 
    {exists = []; 
    pure = [Comp(Eq, x, y)]; 
    spatial = [List(x, y)]} in
  let sh2 = 
    {exists = []; 
    pure = [Comp(Neq, v0, Int(2))]; 
    spatial = [Freed(y); PointsTo(x, Int(5))]} in

  let result ={
    refinements1 = [];
    heap1 = empty_sh;
    antiframe = empty_sh;
    refinements2 = [];
    heap2 = empty_sh;
    frame = empty_sh;
    axiom = false;
    name = "removeL"
  } in

  assert_rule_result_equal None (removeL empty_sh sh1);
  assert_rule_result_equal None (removeL true_sh sh1); 
  assert_rule_result_equal None (removeR sh1 empty_sh);
  assert_rule_result_equal None (removeR sh1 true_sh); 
  assert_rule_result_equal (Some {result with heap1 = {sh1 with spatial = []}}) (removeL sh1 empty_sh);
  assert_rule_result_equal (Some {result with heap1 = {sh1 with spatial = []}; heap2 = sh2}) (removeL sh1 sh2); 
  assert_rule_result_equal (Some {result with heap2 = {sh1 with spatial = []}; name = "removeR"}) (removeR empty_sh sh1);
  assert_rule_result_equal (Some {result with heap2 = {sh1 with spatial = []}; heap1 = sh2; name = "removeR"}) (removeR sh2 sh1);
;;

let test_match _ =
  let x = Ide(Pvar("x")) in
  let y = Ide(Pvar("y")) in
  let k = Ide(Pvar("k")) in
  let v0 = Ide(Lvar(fresh_lvar ())) in

  let sh1 = 
    {exists = []; 
    pure = []; 
    spatial = [Freed(y); PointsTo(x, k)]} in
  let sh2 = 
    {exists = []; 
    pure = [Comp(Neq, v0, Int(2))]; 
    spatial = [Freed(y); PointsTo(x, Int(5))]} in
  let sh3 = 
    {exists = []; 
    pure = [Comp(Neq, k, Int(5))]; 
    spatial = [Freed(y); PointsTo(x, Int(5))]} in

  let result ={
    refinements1 = [];
    heap1 = empty_sh;
    antiframe = empty_sh;
    refinements2 = [];
    heap2 = empty_sh;
    frame = empty_sh;
    axiom = false;
    name = "pointsto-match"
  } in

  assert_rule_result_equal None (freed_match empty_sh sh1);
  assert_rule_result_equal None (pt_match empty_sh sh1);
  assert_rule_result_equal None (freed_match true_sh sh1); 
  assert_rule_result_equal None (pt_match true_sh sh1);
  assert_rule_result_equal (Some {result with 
                                    heap1 = {sh1 with spatial = [Freed(y)]; pure = Comp(Eq, k, Int(5)) :: sh1.pure}; 
                                    heap2 = {sh2 with spatial = [Freed(y)]; pure = Comp(Eq, k, Int(5)) :: sh2.pure}; 
                                    antiframe = symbolic_heap_of_pure_preds [Comp(Eq, k, Int(5))];
                                    frame = symbolic_heap_of_pure_preds [Comp(Eq, k, Int(5))]}
                            ) (pt_match sh1 sh2);
  assert_rule_result_equal (Some {result with 
                                    heap1 = {sh1 with spatial = [PointsTo(x, k)]}; 
                                    heap2 = {sh2 with spatial = [PointsTo(x, Int(5))]};
                                    name = "freed-match"}
                            ) (freed_match sh1 sh2); 
  assert_rule_result_equal (Some {result with 
                                    heap1 = {sh1 with spatial = [Freed(y)]; pure = Comp(Eq, k, Int(5)) :: sh1.pure}; 
                                    heap2 = {sh2 with spatial = [Freed(y)]; pure = Comp(Eq, k, Int(5)) :: sh3.pure};
                                    antiframe = symbolic_heap_of_pure_preds [Comp(Eq, k, Int(5))];
                                    frame = symbolic_heap_of_pure_preds [Comp(Eq, k, Int(5))]}
                            ) (pt_match sh1 sh3); 
;;

let test_ls_start _ =
  let x = Ide(Pvar("x")) in
  let y = Ide(Pvar("y")) in
  let z = Ide(Pvar("z")) in
  let v0 = Ide(Lvar(fresh_lvar ())) in

  let sh1 = 
    {exists = []; 
    pure = []; 
    spatial = [PointsTo(x, Int(5))]} in
  let sh2 = 
    {exists = []; 
    pure = [Comp(Neq, v0, Int(2))]; 
    spatial = [Freed(y); List(x, z)]} in
  let sh3 = 
    {exists = []; 
    pure = []; 
    spatial = [PointsTo(x, Int(42)); List(v0, z)]} in
  let sh4 = 
    {exists = []; 
    pure = []; 
    spatial = [List(x, y); PointsTo(v0, Int(25))]} in

  let result ={
    refinements1 = [];
    heap1 = empty_sh;
    antiframe = empty_sh;
    refinements2 = [];
    heap2 = empty_sh;
    frame = empty_sh;
    axiom = false;
    name = "ls-startL"
  } in

  assert_rule_result_equal None (ls_startL empty_sh sh1);
  assert_rule_result_equal None (ls_startR empty_sh sh1);
  assert_rule_result_equal None (ls_startL true_sh sh1); 
  assert_rule_result_equal None (ls_startR true_sh sh1);
  assert_rule_result_equal None (ls_startL sh1 sh2);
  assert_rule_result_equal (Some {result with 
                                    heap1 = {sh1 with spatial = []; pure = Comp(Neq, x, z) :: sh1.pure}; 
                                    heap2 = {sh2 with spatial = [List(Int(5), z); Freed(y)]; pure = Comp(Neq, x, z) :: sh2.pure}; 
                                    antiframe = symbolic_heap_of_pure_preds [Comp(Neq, x, z)];
                                    frame = symbolic_heap_of_pure_preds [Comp(Neq, x, z)];
                                    refinements1 = [];
                                    refinements2 = [PointsToP(x, Int(5))];
                                    name = "ls-startR"}
                            ) (ls_startR sh1 sh2);
  assert_rule_result_equal (Some {result with 
                                    heap2 = {sh1 with spatial = []; pure = Comp(Neq, x, z) :: sh1.pure}; 
                                    heap1 = {sh2 with spatial = [List(Int(5), z); Freed(y)]; pure = Comp(Neq, x, z) :: sh2.pure}; 
                                    antiframe = symbolic_heap_of_pure_preds [Comp(Neq, x, z)];
                                    frame = symbolic_heap_of_pure_preds [Comp(Neq, x, z)];
                                    refinements2 = [];
                                    refinements1 = [PointsToP(x, Int(5))]}
                            ) (ls_startL sh2 sh1); 
  assert_rule_result_equal (Some {result with 
                                    heap1 = {sh1 with spatial = [List(Int(25), z); PointsTo(x, Int(42))]; pure = Comp(Neq, v0, z) :: sh3.pure}; 
                                    heap2 = {sh2 with spatial = [List(x, y)]; pure = Comp(Neq, v0, z) :: sh4.pure}; 
                                    antiframe = symbolic_heap_of_pure_preds [Comp(Neq, v0, z)];
                                    frame = symbolic_heap_of_pure_preds [Comp(Neq, v0, z)];
                                    refinements1 = [PointsToP(v0, Int(25))];
                                    refinements2 = []}
                            ) (ls_startL sh3 sh4);
  assert_rule_result_equal (Some {result with 
                                    heap1 = {sh1 with spatial = [List(v0, z)]; pure = Comp(Neq, x, y) :: sh3.pure}; 
                                    heap2 = {sh2 with spatial = [List(Int(42), y); PointsTo(v0, Int(25))]; pure = Comp(Neq, x, y) :: sh4.pure}; 
                                    antiframe = symbolic_heap_of_pure_preds [Comp(Neq, x, y)];
                                    frame = symbolic_heap_of_pure_preds [Comp(Neq, x, y)];
                                    refinements1 = [];
                                    refinements2 = [PointsToP(x, Int(42))];
                                    name = "ls-startR"}
                            ) (ls_startR sh3 sh4);
;;

let test_ls_end _ =
  let x = Ide(Pvar("x")) in
  let y = Ide(Pvar("y")) in
  let z = Ide(Pvar("z")) in
  let v0 = Ide(Lvar(fresh_lvar ())) in

  let sh1 = 
    {exists = []; 
    pure = []; 
    spatial = [List(x, Int(5))]} in
  let sh2 = 
    {exists = []; 
    pure = [Comp(Neq, v0, Int(2))]; 
    spatial = [Freed(y); List(x, z)]} in

  let result ={
    refinements1 = [];
    heap1 = empty_sh;
    antiframe = empty_sh;
    refinements2 = [];
    heap2 = empty_sh;
    frame = empty_sh;
    axiom = false;
    name = "ls-endL"
  } in

  assert_rule_result_equal None (ls_endL empty_sh sh1);
  assert_rule_result_equal None (ls_endR empty_sh sh1);
  assert_rule_result_equal None (ls_endL true_sh sh1); 
  assert_rule_result_equal None (ls_endR true_sh sh1);
  assert_rule_result_equal (Some {result with 
                                    heap1 = {sh1 with spatial = [List(z, Int(5))]}; 
                                    heap2 = {sh2 with spatial = [Freed(y)]}}
                            ) (ls_endL sh1 sh2);
  assert_rule_result_equal (Some {result with 
                                    heap2 = {sh2 with spatial = [List(Int(5), z); Freed(y)]};
                                    name = "ls-endR"}
                            ) (ls_endR sh1 sh2);
;;

let test_missing _ =
  let x = Ide(Pvar("x")) in
  let y = Ide(Pvar("y")) in
  let k = Ide(Pvar("k")) in
  let v0 = Ide(Lvar(fresh_lvar ())) in

  let sh1 = 
    {exists = []; 
    pure = []; 
    spatial = [PointsTo(x, Int(5))]} in
  let sh2 = 
    {exists = []; 
    pure = [Comp(Neq, v0, Int(2))]; 
    spatial = [Freed(y); PointsTo(k, Int(5))]} in

  let result ={
    refinements1 = [];
    heap1 = empty_sh;
    antiframe = empty_sh;
    refinements2 = [];
    heap2 = empty_sh;
    frame = empty_sh;
    axiom = false;
    name = "missingL"
  } in

  assert_rule_result_equal None (missingR empty_sh sh1);
  assert_rule_result_equal None (missingL sh1 empty_sh);
  assert_rule_result_equal (Some {result with
                                    heap2 = sh2;
                                    frame = symbolic_heap_of_spatial_preds [PointsTo(x, Int(5))];
                                    name = "missingR"}
                            ) (missingR sh1 sh2);
  assert_rule_result_equal (Some {result with 
                                    heap1 = sh1;
                                    heap2 = {sh2 with spatial = [PointsTo(k, Int(5))]};
                                    antiframe = symbolic_heap_of_spatial_preds [Freed(y)]}
                            ) (missingL sh1 sh2); 
;;

let suite =
  "Full BiAbduction rules tests" >::: [
    "base_emp" >:: test_base_emp;
    "remove" >:: test_remove;
    "match" >:: test_match;
    "ls_start" >:: test_ls_start;
    "ls_end" >:: test_ls_end;
    "missing" >:: test_missing
  ]

let () =
  run_test_tt_main suite
;;
open Symbolic_heap.Types
open Symbolic_heap.Formatting
open Symbolic_heap.Substitution

open OUnit2

let assert_expr_equal expected actual =
  assert_equal
    ~printer:format_expr
    expected
    actual
;;

let assert_pure_equal expected actual =
  assert_equal
    ~printer:format_pure_pred
    expected
    actual
;;

let test_expr_substitution _ =
  let x = Pvar "x" in
  let y = Pvar "y" in
  let n = Pvar "new" in
  let v0 = Lvar(0) in
  let v1 = Lvar(1) in

  let e1 = Int 2 in
  let e2 = Ide x in
  let e3 = Ide (v0) in
  let e4 = Ide (v1) in
  let e5 = Binop(Add, e1, Binop(Sub, Binop(Mul, e2, e3), e2)) in

  assert_expr_equal e1 (subst_var_expr x v0 e1);
  assert_expr_equal (Ide y) (subst_var_expr x y e2);
  assert_expr_equal (Ide v1) (subst_var_expr v0 v1 e3);
  assert_expr_equal e4 (subst_var_expr v0 x e4);
  assert_expr_equal (Binop(Add, e1, Binop(Sub, Binop(Mul, (Ide n), e3), (Ide n)))) (subst_var_expr x n e5)
;;
(*
let test_pure_substitution _ =
  let e1 = Int 2 in
  let e3 = Ide (Lvar(fresh_lvar())) in
  let e4 = Ide (Lvar(fresh_lvar())) in

  let pp1 = TrueB in
  let pp2 = Comp (Eq, e1, e3) in
  let pp3 = Comp (Neq, Ide (Pvar "x"), e4) in
  let pp4 = PointsToP (Ide (Pvar "y"), e1) in

  assert_string_equal "true" (format_pure_pred []);
  assert_string_equal "true" (format_pure_pred [pp1]);
  assert_string_equal "x/=_v1 & 2=_v0 & true" (format_pure_pred [pp3; pp2; pp1]);
  assert_string_equal "y->2 & x/=_v1" (format_pure_pred [pp4; pp3])
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
  assert_string_equal "x->2" (format_spat_pred [sp2]);
  assert_string_equal "ls(y,z) * w-/>" (format_spat_pred [sp3; sp4]);
  assert_string_equal "w-/> * x->2 * true" (format_spat_pred [sp4; sp2; sp1])
;;

let test_symb_heap_format _ =
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
    {exists = []; 
    pure = [Comp(Neq, v0, Int(2))]; 
    spatial = [Freed(y); PointsTo(x, Int(5))]} in
  let sh3 = 
    {exists = [1]; 
    pure = [Comp(Eq, e, f); Comp(Neq, v1, Int(42))]; 
    spatial = [PointsTo(x, Int(6)); PointsTo(e, Int(5)); PointsTo(y, Int(7))]} in
  let sh4 = 
    {exists = []; 
    pure = [Comp(Eq, Int 2, Ide(Lvar(fresh_lvar())))]; 
    spatial = [PointsTo(w, Int(5)); PointsTo(z, Int(5)); List(k, Int(7))]} in
  let sh5 = 
    {exists = [0; 1]; 
    pure = [Comp(Neq, Ide(Pvar "x"), v1)]; 
    spatial = [PointsTo(v0, Int(5)); Freed(f)]} in

  assert_string_equal "[ true | emp ]" (format_symb_heap empty_sh);
  assert_string_equal "[ true | true ]" (format_symb_heap true_sh);
  assert_string_equal "[ 2=2 | x->5 ]" (format_symb_heap sh1);
  assert_string_equal "[ _v0/=2 | y-/> * x->5 ]" (format_symb_heap sh2);
  assert_string_equal "∃(_v1).[ e=f & _v1/=42 | x->6 * e->5 * y->7 ]" (format_symb_heap sh3);
  assert_string_equal "[ 2=_v2 | w->5 * z->5 * ls(k,7) ]" (format_symb_heap sh4);
  assert_string_equal "∃(_v0,_v1).[ x/=_v1 | _v0->5 * f-/> ]" (format_symb_heap sh5);
;;
*)
let suite =
  "Substitution tests" >::: [
    "expr_substitution" >:: test_expr_substitution;
    (*"pure_predicates_substitution" >:: test_pure_substitution*)
  ]

let () =
  run_test_tt_main suite
;;
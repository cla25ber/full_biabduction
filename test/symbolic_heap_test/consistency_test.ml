open Symbolic_heap.Types
open Symbolic_heap.Consistency

open OUnit2

let test_equal_expr _ =
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

  assert (equal_expr pp0 x x);
  assert (equal_expr [] x x);
  assert (equal_expr [] v0 v0);
  assert (equal_expr pp1 x y);
  assert (not (equal_expr [] x y));
  assert (equal_expr pp2 x y);
  assert (equal_expr pp3 x z);
  assert (equal_expr pp4 v0 v1);
  assert (equal_expr pp5 v0 v1);
  assert (not (equal_expr pp6 x y));
  assert (equal_expr pp7 v0 v1);
  assert (not (equal_expr pp8 x v1));
  assert (not (equal_expr pp8 x w));
  assert (equal_expr pp8 v0 w);
;;

let test_equal_spatial _ =
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
  let sp7 = PointsTo(y, v0) in
  let sp8 = PointsTo(y, v1) in
  let sp9 = PointsTo(v0, v1) in
  let sp10 = PointsTo(x, v0) in
  let sp11 = PointsTo(x, w) in
  let sp12 = PointsTo(z, v1) in 

  assert (equal_spat_pred pp0 sp1 sp1);
  assert (equal_spat_pred [] sp1 sp1);
  assert (equal_spat_pred pp1 sp2 sp3);
  assert (not (equal_spat_pred [] sp1 sp2));
  assert (not (equal_spat_pred [] sp2 sp3));
  assert (not (equal_spat_pred [] sp1 sp4));
  assert (equal_spat_pred pp2 sp2 sp3);
  assert (equal_spat_pred pp3 sp1 sp3);
  assert (equal_spat_pred pp4 sp5 sp6);
  assert (equal_spat_pred pp5 sp7 sp8);
  assert (not (equal_spat_pred pp6 sp1 sp4));
  assert (not (equal_spat_pred pp6 sp2 sp3));
  assert (equal_spat_pred pp7 sp8 sp9);
  assert (not (equal_spat_pred pp8 sp1 sp10));
  assert (not (equal_spat_pred pp8 sp1 sp11));
  assert (equal_spat_pred pp8 sp10 sp12);
;;

let suite =
  "Consistency tests" >::: [
    "expression_equality" >:: test_equal_expr;
    "spatial_predicates_equality" >:: test_equal_spatial
  ]

let () =
  run_test_tt_main suite
;;
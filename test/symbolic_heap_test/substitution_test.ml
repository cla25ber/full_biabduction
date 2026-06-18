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

let assert_spatial_equal expected actual =
  assert_equal
    ~printer:format_spat_pred
    expected
    actual
;;
let assert_symb_heap_equal expected actual =
  assert_equal
    ~printer:format_symb_heap
    expected
    actual
;;

let assert_varset_equal expected actual =
  assert_equal
    ~cmp:VarSet.equal
    ~printer:(fun s -> String.concat " " (List.map format_var (VarSet.elements s)))
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

let test_pure_substitution _ =
  let x = Pvar "x" in
  let y = Pvar "y" in
  let v0 = Lvar(0) in
  let v1 = Lvar(1) in

  let e1 = Int 2 in
  let e2 = Ide x in
  let e3 = Ide (v0) in
  let e4 = Ide (v1) in
  let e5 = Binop(Sub, Binop(Mul, e2, e3), e2) in

  let pp1 = TrueB in
  let pp2 = Comp (Eq, e1, e3) in
  let pp3 = Comp (Neq, e5, e4) in

  assert_pure_equal [pp1] [subst_var_pure_pred x y pp1];
  assert_pure_equal [Comp(Eq, e1, e2)] [subst_var_pure_pred v0 x pp2];
  assert_pure_equal [pp2] [subst_var_pure_pred x y pp2];
  assert_pure_equal [Comp (Neq, Binop(Sub, Binop(Mul, Ide(y), e3), Ide(y)), e4)] [subst_var_pure_pred x y pp3]
;;

let test_spatial_substitution _ =
  let x = Pvar "x" in
  let y = Pvar "y" in
  let v0 = Lvar(0) in
  let v1 = Lvar(1) in

  let e1 = Int 2 in
  let e2 = Ide x in
  let e3 = Ide (v0) in
  let e4 = Ide (v1) in
  let e5 = Binop(Add, Binop(Mul, e2, e3), e2) in

  let sp1 = TrueS in
  let sp2 = PointsTo (e1, e3) in
  let sp3 = List (e5, e4) in
  let sp4 = Freed (Ide y) in

  assert_spatial_equal [sp1] [subst_var_spat_pred x y sp1];
  assert_spatial_equal [PointsTo(e1, e2)] [subst_var_spat_pred v0 x sp2];
  assert_spatial_equal [sp2] [subst_var_spat_pred x y sp2];
  assert_spatial_equal [List (Binop(Add, Binop(Mul, Ide(y), e3), Ide(y)), e4)] [subst_var_spat_pred x y sp3];
  assert_spatial_equal [Freed (Ide v1)] [(subst_var_spat_pred y v1 sp4)]
;;

let test_alpha_rename _ =
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

  assert_symb_heap_equal empty_sh (alpha_rename empty_sh 0);
  assert_symb_heap_equal true_sh (alpha_rename true_sh 1);
  assert_symb_heap_equal sh1 (alpha_rename sh1 2);
  assert_symb_heap_equal sh1 (alpha_rename sh1 0);
  assert_symb_heap_equal sh2 (alpha_rename sh2 0);
  assert_symb_heap_equal {sh2 with exists = [2]} (alpha_rename sh2 1);
  assert_symb_heap_equal {sh3 with exists = [3]; pure = [Comp(Eq, e, f); Comp(Neq, Ide(Lvar(3)), Int(42))]} (alpha_rename sh3 1);
  assert_symb_heap_equal sh3 (alpha_rename sh3 0);
  assert_symb_heap_equal sh4 (alpha_rename sh4 42);
  assert_symb_heap_equal {sh5 with exists = [4; 1]; spatial = [PointsTo(Ide(Lvar(4)), Int(5)); Freed(f)]} (alpha_rename sh5 0);
  assert_symb_heap_equal {sh5 with exists = [0; 5]; pure = [Comp(Neq, Ide(Pvar "x"), Ide(Lvar(5)))]} (alpha_rename sh5 1);
  assert_symb_heap_equal {exists = [6; 7]; 
                          spatial = [PointsTo(Ide(Lvar(6)), Int(5)); Freed(f)];
                          pure =[Comp(Neq, Ide(Pvar "x"), Ide(Lvar(7)))]} 
                        (alpha_rename (alpha_rename sh5 0) 1);
;;

let test_collect_variables _ =
  let x = Pvar "x" in
  let y = Pvar "y" in
  let v0 = Lvar(fresh_lvar()) in
  let v1 = Lvar(fresh_lvar()) in
  let e = Pvar "e" in
  let f = Pvar "f" in

  let e1 = Int 2 in
  let e2 = Ide x in
  let e3 = Ide (v0) in
  let e4 = Ide (v1) in
  let e5 = Binop(Add, Binop(Mul, e2, e3), e2) in
  
  let pp1 = TrueB in
  let pp2 = Comp (Eq, e1, e3) in
  let pp3 = Comp (Neq, e5, e4) in

  let sp1 = TrueS in
  let sp2 = PointsTo (e1, e3) in
  let sp3 = List (e5, e4) in
  let sp4 = Freed (Ide y) in

  let sh1 = 
    {exists = []; 
    pure = [Comp(Eq, Int 2, Int(2))]; 
    spatial = [PointsTo(e2, Int(5))]} in
  let sh2 = 
    {exists = [1]; 
    pure = [Comp(Neq, e3, Int(2))]; 
    spatial = [Freed(Ide y); PointsTo(e2, Int(5))]} in
  let sh3 = 
    {exists = [1]; 
    pure = [Comp(Eq, Ide e, Ide f); Comp(Neq, e4, Int(42))]; 
    spatial = [PointsTo(e2, e1); PointsTo(Ide e, Int(5)); PointsTo(Ide y, Int(7))]} in
  let sh4 = 
    {exists = []; 
    pure = [Comp(Eq, Int 2, e4)]; 
    spatial = [PointsTo(Ide (Pvar "w"), Int(5)); PointsTo(Ide (Pvar "z"), Int(5)); List(Ide (Pvar "k"), Int(7))]} in
  let sh5 = 
    {exists = [0; 1]; 
    pure = [Comp(Neq, e2, e4)]; 
    spatial = [PointsTo(e3, Int(5)); Freed(Ide f)]} in

  assert_varset_equal (VarSet.of_list []) (all_vars_expr e1);
  assert_varset_equal (VarSet.of_list [x]) (all_vars_expr e2);
  assert_varset_equal (VarSet.of_list [v0]) (all_vars_expr e3);
  assert_varset_equal (VarSet.of_list [v1]) (all_vars_expr e4);
  assert_varset_equal (VarSet.of_list [x; v0]) (all_vars_expr e5);

  assert_varset_equal (VarSet.of_list []) (all_vars_pure_preds []);
  assert_varset_equal (VarSet.of_list []) (all_vars_pure_preds [pp1]);
  assert_varset_equal (VarSet.of_list [v0; x; v1]) (all_vars_pure_preds [pp2; pp3; pp2]);
  assert_varset_equal (VarSet.of_list [v0]) (all_vars_pure_preds [pp2]);
  assert_varset_equal (VarSet.of_list [v0; x; v1]) (all_vars_pure_preds [pp1; pp2; pp3]);

  assert_varset_equal (VarSet.of_list []) (all_vars_spatial_preds []);
  assert_varset_equal (VarSet.of_list []) (all_vars_spatial_preds [sp1]);
  assert_varset_equal (VarSet.of_list [v0; x; v1]) (all_vars_spatial_preds [sp2; sp3; sp2]);
  assert_varset_equal (VarSet.of_list [v0; y]) (all_vars_spatial_preds [sp4; sp2]);
  assert_varset_equal (VarSet.of_list [v0; x; v1; y]) (all_vars_spatial_preds [sp1; sp2; sp3; sp4]);

  assert_varset_equal (VarSet.of_list []) (all_variables_symb_heap empty_sh);
  assert_varset_equal (VarSet.of_list []) (all_variables_symb_heap true_sh);
  assert_varset_equal (VarSet.of_list [x]) (all_variables_symb_heap sh1);
  assert_varset_equal (VarSet.of_list [v0; x; y]) (all_variables_symb_heap sh2);
  assert_varset_equal (VarSet.of_list [e; f; v1; x; y]) (all_variables_symb_heap sh3);
  assert_varset_equal (VarSet.of_list [v1; Pvar "w"; Pvar "z"; Pvar "k"]) (all_variables_symb_heap sh4);
  assert_varset_equal (VarSet.of_list [v0; x; v1; f]) (all_variables_symb_heap sh5);

  assert_varset_equal (VarSet.of_list []) (free_variables_symb_heap empty_sh);
  assert_varset_equal (VarSet.of_list []) (free_variables_symb_heap true_sh);
  assert_varset_equal (VarSet.of_list [x]) (free_variables_symb_heap sh1);
  assert_varset_equal (VarSet.of_list [v0; x; y]) (free_variables_symb_heap sh2);
  assert_varset_equal (VarSet.of_list [e; f; x; y]) (free_variables_symb_heap sh3);
  assert_varset_equal (VarSet.of_list [v1; Pvar "w"; Pvar "z"; Pvar "k"]) (free_variables_symb_heap sh4);
  assert_varset_equal (VarSet.of_list [x; f]) (free_variables_symb_heap sh5)
;;

let suite =
  "Substitution tests" >::: [
    "expr_substitution" >:: test_expr_substitution;
    "pure_predicates_substitution" >:: test_pure_substitution;
    "spatial_predicates_substitution" >:: test_spatial_substitution;
    "alpha_renaming" >:: test_alpha_rename;
    "collecting_variables" >:: test_collect_variables
  ]

let () =
  run_test_tt_main suite
;;
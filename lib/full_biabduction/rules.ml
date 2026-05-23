open Symbolic_heap.Types
open Symbolic_heap.Substitution
open Symbolic_heap.Symb_heap_ops
open Symbolic_heap.Utils

type rule_result = {
	refinements1 : pure_pred list;
	heap1 : symb_heap;
	antiframe : symb_heap;
	refinements2 : pure_pred list;
	heap2 : symb_heap;
	frame : symb_heap;
  axiom : bool
} ;;

type rule = symb_heap -> symb_heap -> rule_result option ;;

let base_emp sh1 sh2 =
	match sh1.spatial, sh2.spatial with
		| [], [] -> ( 
			let result = {
				refinements1 = [];
				heap1 = empty_sh;
				antiframe = (symbolic_heap_of_pure_preds sh2.pure);
				refinements2 = [];
				heap2 = empty_sh;
				frame = (symbolic_heap_of_pure_preds sh1.pure);
        axiom = true
			} in
			Some result
		)
		| _ -> None
;;

let removeL sh1 sh2 =
  let all_pure_preds = sh1.pure @ sh2.pure in
  let res = eliminate_first (empty_predicate all_pure_preds) sh1.spatial in
	match res with
    | None -> None
    | Some (_, preds) ->
      let result = {
        refinements1 = [];
        heap1 = {sh1 with spatial = preds};
        antiframe = empty_sh;
        refinements2 = [];
        heap2 = sh2;
        frame = empty_sh;
        axiom = false
      } in
      Some result
;;

let removeR sh1 sh2 =
  let all_pure_preds = sh1.pure @ sh2.pure in
  let res = eliminate_first (empty_predicate all_pure_preds) sh2.spatial in
	match res with
    | None -> None
    | Some (_, preds) ->
      let result = {
        refinements1 = [];
        heap1 = sh1;
        antiframe = empty_sh;
        refinements2 = [];
        heap2 = {sh2 with spatial = preds};
        frame = empty_sh;
        axiom = false
      } in
      Some result
;;

let freed_match sh1 sh2 =
  let all_pure_preds = sh1.pure @ sh2.pure in
  let res = double_eliminate_first is_freed sh1.spatial (same_heap_cell_freed all_pure_preds) sh2.spatial in
  match res with
    | None -> None
    | Some (_, preds1, _, preds2) -> 
      let result = {
        refinements1 = [];
        heap1 = {sh1 with spatial = preds1};
        antiframe = empty_sh;
        refinements2 = [];
        heap2 = {sh2 with spatial = preds2};
        frame = empty_sh;
        axiom = false
      } in
      Some result
;;

let pt_match sh1 sh2 =
  let all_pure_preds = sh1.pure @ sh2.pure in
  let res = double_eliminate_first is_pointsto sh1.spatial (same_heap_cell_pointsto all_pure_preds) sh2.spatial in
  match res with
    | None -> None
    | Some (PointsTo(_, e1), preds1, PointsTo(_ ,e2), preds2) -> 
      let equality = (symbolic_heap_of_pure_preds [Comp(Eq, e1, e2)]) in
      let add_equality = merge_symb_heap equality in
      let result = {
        refinements1 = [];
        heap1 = add_equality {sh1 with spatial = preds1};
        antiframe = equality;
        refinements2 = [];
        heap2 = add_equality {sh2 with spatial = preds2};
        frame = equality;
        axiom = false
      } in
      Some result
    | _ -> failwith "Unexpected beheviour in pt_match."
;;

let ls_startL sh1 sh2 =
  let all_pure_preds = sh1.pure @ sh2.pure in
  let res = double_eliminate_first is_list sh1.spatial (same_heap_cell_pointsto all_pure_preds) sh2.spatial in
  match res with
    | None -> None
    | Some (List (e, e1), preds1, PointsTo (_, e2), preds2) -> 
      let remaining_list_pred = List(e2, e1) in
      let inequality = (symbolic_heap_of_pure_preds [Comp(Neq, e, e1)]) in
      let add_inequality = merge_symb_heap inequality in
      let result = {
        refinements1 = [PointsToP(e, e2)];
        heap1 = add_inequality {sh1 with spatial = (remaining_list_pred :: preds1)};
        antiframe = inequality;
        refinements2 = [];
        heap2 = add_inequality {sh2 with spatial = preds2};
        frame = inequality;
        axiom = false
      } in
      Some result
    | _ -> failwith "Unexpected beheviour in ls_startL."
;;

let ls_startR sh1 sh2 =
  let all_pure_preds = sh1.pure @ sh2.pure in
  let res = double_eliminate_first is_pointsto sh1.spatial (same_heap_cell_list all_pure_preds) sh2.spatial in
  match res with
    | None -> None
    | Some (PointsTo (_, e1), preds1, List (e, e2), preds2) -> 
      let remaining_list_pred = List(e1, e2) in
      let inequality = (symbolic_heap_of_pure_preds [Comp(Neq, e, e2)]) in
      let add_inequality = merge_symb_heap inequality in
      let result = {
        refinements1 = [];
        heap1 = add_inequality {sh1 with spatial = preds1};
        antiframe = inequality;
        refinements2 = [PointsToP(e, e1)];
        heap2 = add_inequality {sh2 with spatial = (remaining_list_pred :: preds2)};
        frame = inequality;
        axiom = false
      } in
      Some result
    | _ -> failwith "Unexpected beheviour in ls_startR."
;;

let ls_endL sh1 sh2 =
  let all_pure_preds = sh1.pure @ sh2.pure in
  let res = double_eliminate_first is_list sh1.spatial (same_heap_cell_list all_pure_preds) sh2.spatial in
  match res with
    | None -> None
    | Some (List (_, e1), preds1, List (_, e2), preds2) -> 
      let remaining_list_pred = List(e2, e1) in
      let result = {
        refinements1 = [];
        heap1 = {sh1 with spatial = (remaining_list_pred :: preds1)};
        antiframe = empty_sh;
        refinements2 = [];
        heap2 = {sh2 with spatial = preds2};
        frame = empty_sh;
        axiom = false
      } in
      Some result
    | _ -> failwith "Unexpected beheviour in ls_endL."
;;

let ls_endR sh1 sh2 =
  let all_pure_preds = sh1.pure @ sh2.pure in
  let res = double_eliminate_first is_list sh1.spatial (same_heap_cell_list all_pure_preds) sh2.spatial in
  match res with
    | None -> None
    | Some (List (_, e1), preds1, List (_, e2), preds2) -> 
      let remaining_list_pred = List(e1, e2) in
      let result = {
        refinements1 = [];
        heap1 = {sh1 with spatial =  preds1};
        antiframe = empty_sh;
        refinements2 = [];
        heap2 = {sh2 with spatial = (remaining_list_pred :: preds2)};
        frame = empty_sh;
        axiom = false
      } in
      Some result
    | _ -> failwith "Unexpected beheviour in ls_endR."
;;

let missingL sh1 sh2 =
  match sh1.spatial with
    | sp :: preds ->
      let result = {
        refinements1 = [];
        heap1 = {sh1 with spatial = preds};
        antiframe = empty_sh;
        refinements2 = [];
        heap2 = sh2;
        frame = symbolic_heap_of_spatial_preds [sp];
        axiom = false
      } in
      Some result
    | [] -> None
;;

let missingR sh1 sh2 =
  match sh2.spatial with
    | sp :: preds ->
      let result = {
        refinements1 = [];
        heap1 = sh1;
        antiframe = symbolic_heap_of_spatial_preds [sp];
        refinements2 = [];
        heap2 = {sh2 with spatial = preds};
        frame = empty_sh;
        axiom = false
      } in
      Some result
    | [] -> None
;;

let ruleSet1:rule list = [base_emp; removeL; removeR; freed_match; pt_match; ls_startL; ls_startR; ls_endL; missingL; missingR] ;;

let ruleSet2:rule list = [base_emp; removeL; removeR; freed_match; pt_match; ls_startL; ls_startR; ls_endR; missingL; missingR] ;;
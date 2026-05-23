open Symbolic_heap.Types
open Symbolic_heap.Symb_heap_ops
open Symbolic_heap.Formatting
open Symbolic_heap.Utils
open Rules

type full_biabduction_result = {
  refinements1 : pure_pred list;
  antiframe : symb_heap;
  refinements2 : pure_pred list;
  frame : symb_heap
}

let rec full_biabduction (rules:rule list) sh1 sh2 =
  match apply_until_result rules sh1 sh2 0 with
    | None -> None
    | Some res ->
      if (res.axiom) then
        let result = {
          refinements1 = res.refinements1;
          antiframe = res.antiframe;
          refinements2 =res.refinements2;
          frame = res.frame
        } in Some result
      else
        let next_results = full_biabduction rules res.heap1 res.heap2 in
        match next_results with
          | None -> None
          | Some abduced_facts ->
            let result = {
              refinements1 = res.refinements1 @ abduced_facts.refinements1;
              antiframe = merge_symb_heap res.antiframe abduced_facts.antiframe;
              refinements2 = res.refinements2 @ abduced_facts.refinements2;
              frame = merge_symb_heap res.frame abduced_facts.frame
            } in Some result
;;

let format_fullbiabduction_result (res:full_biabduction_result) =
  (String.concat " " ["("^(format_pure_pred res.refinements1)^")"; "&"; "("^(format_symb_heap res.antiframe)^")"]) ^
  "\n" ^
  (String.concat " " ["("^(format_pure_pred res.refinements2)^")"; "&"; "("^(format_symb_heap res.frame)^")"])
;;
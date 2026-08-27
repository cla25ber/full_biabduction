open Symbolic_heap.Formatting
open Symbolic_heap.Random_heaps


let () = Random.self_init () ;;

print_endline (format_symb_heap (rand_heap 30 5 15)) ;;

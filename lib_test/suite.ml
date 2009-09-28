open OUnit
open Printf

let suites = [
  Simple.suite;
  Tuple.suite;
  Variant.suite;
  Alltypes.suite;
  Foreign.suite;
  Recursive.suite;
  Array_simple.suite;
  Foreign_and_variant.suite;
  Foreign_tuple.suite;
  List_simple.suite;
  List_foreign.suite;
  List_tuple.suite;
]

let _ =
  run_test_tt_main ("ORM" >::: (List.flatten suites))

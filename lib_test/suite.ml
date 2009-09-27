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
]

let _ =
  run_test_tt_main ("ORM" >::: (List.flatten suites))

open OUnit
open Printf

let suites = [
  Simple.suite;
  Tuple.suite;
  Variant.suite;
  Alltypes.suite;
  Foreign.suite;
]

let _ =
  run_test_tt_main ("ORM test" >::: (List.flatten suites))

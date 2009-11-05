open OUnit
open Printf

let suites = [
  All.suite
]

let _ =
  let s = suites in
  run_test_tt_main ("ORM" >::: (List.flatten s))

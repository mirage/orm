open OUnit
open Printf

let suites = [
  Simple.suite;
  Tuple.suite;
  Variant.suite;
  Variant_nested.suite;
  Alltypes.suite;
  Foreign.suite;
  Recursive.suite;
  Array_simple.suite;
  Foreign_and_variant.suite;
  Foreign_tuple.suite;
  List_simple.suite;
  List_foreign.suite;
  List_tuple.suite;
  List_list.suite;
  Nested_tuple.suite;
  Nested_option.suite;
  Record_mutate.suite;
  List_mutate.suite;
  Recursive_mutate.suite;
  Photo.suite;
]

let slow_suites = [
  Stress.suite
]

let _ =
  let s = try 
   if Sys.getenv "SLOW" <> "" then
      slow_suites
     else suites
   with Not_found -> suites in
  run_test_tt_main ("ORM" >::: (List.flatten s))

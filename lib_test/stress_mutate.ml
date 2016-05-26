(*pp camlp4orf *)

type t = { a : int; mutable b : int } with orm

open OUnit
open Printf
open Test_utils

let name = "stress_mutate.db"

let test_mutate () =
  let db = t_init name in
  let t1 = { a=1; b=0 } in
  for i= 0 to 4000 do
    t1.b <- i;
    t_save db t1;
  done

let suite = [
  "stress_mutate" >:: test_mutate;
]

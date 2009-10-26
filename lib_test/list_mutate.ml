(*pp camlp4o -I ../lib -I `ocamlfind query type-conv` pa_type_conv.cmo pa_orm.cma *)

TYPE_CONV_PATH "List_mutate"

type x = {
  foo: int;
  mutable bar: (string * string) list
} with orm(
    debug: leak,all
  )

open Printf
open Orm
open Test_utils
open OUnit

let name = "list_mutate.db" 

let test_mutate_basic () =
  let db = open_db init name in
  let l = [ "foo1","bar1"; "foo2","bar2" ] in
  let t1 = {foo=1; bar=l } in
  x_save db t1;
  let l = ("foo3","bar3") :: l in
  t1.bar <- l;
  x_save db t1;
  match x_get db with
  |[t] -> "eq" @? (t.bar = ["foo3","bar2"; "foo1","bar1"; "foo2","bar2" ])
  |[] -> failwith "no x"
  |x -> failwith (sprintf "too many x: %d" (List.length x))

let suite = [
  "list_mutate_basic" >:: test_mutate_basic;
]

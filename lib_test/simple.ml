(*pp camlp4o -I ../lib -I `ocamlfind query type-conv` pa_type_conv.cmo pa_sql_orm.cma *)

TYPE_CONV_PATH "Simple"
open Printf

type x = {
  foo: int;
  bar: string
} with
persist()

open Orm
open OUnit
open Test_utils

let name = "simple.db"

let x = { foo = (Random.int 100); bar="hello world" }

let test_init () =
  ignore(open_db init name);
  ignore(open_db ~rm:false init name);
  ignore(open_db ~rm:false init name)

let test_save () =
  let db = open_db init name in
  x_save db x

let test_update () =
  let db = open_db init name in
  x_save db x;
  x_save db x

let test_get () =
  let db = open_db ~rm:false init name in
  let i = x_get db in
  "2 in db" @? (List.length i = 1);
  let i = List.hd i in
  "values match" @? (i.foo = x.foo && (i.bar = x.bar))

let test_save_get () =
  let db = open_db ~rm:false init name in
  x_save db x;
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "structurally equal after get" @? ( x == i)

let suite = [
  "simple_init" >:: test_init;
  "simple_save" >:: test_save;
  "simple_update" >:: test_update;
  "simple_update" >:: test_update;
  "simple_get" >:: test_get;
]


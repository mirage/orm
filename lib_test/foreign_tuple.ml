(*pp camlp4o -I ../lib -I `ocamlfind query type-conv` pa_type_conv.cmo pa_orm.cma *)

TYPE_CONV_PATH "Foreign_tuple"

type s = {
  foo: string;
  bar: int64;
}
and x = {
  first: (string * int64 * s);
  second: s;
  third: int;
}
with orm ()

open Printf
open Orm
open OUnit
open Test_utils

let s = { foo="f1"; bar=59L }
let x = { first=("first",3434L,s); second=s; third=99 }

let name = "foreign_tuple.db"

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
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "values match" @? (i.first = x.first && (i.second = x.second))

let test_save_get () =
  let db = open_db init name in
  x_save db x;
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  match i with
  [i] ->
    "structural values equal" @? ( x = i);
    "physical values equal" @? ( x == i)
  |_ -> assert false

let suite = [
  "foreign_tuple_init" >:: test_init;
  "foreign_tuple_save" >:: test_save;
  "foreign_tuple_update" >:: test_update;
  "foreign_tuple_get" >:: test_get;
  "foreign_tuple_save_get" >:: test_save_get;
]

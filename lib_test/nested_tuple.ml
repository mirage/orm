(*pp camlp4o -I ../lib -I `ocamlfind query type-conv` pa_type_conv.cmo pa_orm.cma *)

TYPE_CONV_PATH "Nested_tuple"

type x = {
  foo: (int32 * int64 * string * (string * bool option));
  bar: string * char;
}
with persist()

open Orm
open OUnit
open Test_utils

let x = {foo=(5l, 10L, "tt", ("xx", Some true)) ; bar=("hello",'w') } 
let name = "nested_tuple.db"

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
  "values match" @? (i = x)

let test_save_get () =
  let db = open_db init name in
  x_save db x;
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "structural values equal" @? ( x = i);
  "physical values equal" @? ( x == i)

let suite = [
  "nested_tuple_init" >:: test_init;
  "nested_tuple_save" >:: test_save;
  "nested_tuple_update" >:: test_update;
  "nested_tuple_update" >:: test_update;
  "nested_tuple_get" >:: test_get;
  "nested_tuple_save_get" >:: test_save_get;
]


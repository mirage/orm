(*pp camlp4o -I .. -I `ocamlfind query type-conv` pa_type_conv.cmo pa_sql_orm.cma *)

TYPE_CONV_PATH "Variant"

type s = 
  |Foo
  |Bar of int
  |Xyz of string
and
x = {
  foo: s;
  bar: s;
}
with persist()

open Printf
open Orm
open OUnit
open Test_utils

let string_of_s = function
  |Foo -> "Foo" 
  |Bar i -> "Bar " ^ (string_of_int i) 
  |Xyz z -> "Xyz " ^ z

let string_of_x x =
  sprintf "foo=%s bar=%s" (string_of_s x.foo) (string_of_s x.bar)

let name = "variant.db"

let x1 = { foo=Foo; bar=(Bar 1) }
let x2 = { foo=(Xyz "hello"); bar=Foo }

let test_init () =
  ignore(open_db init name);
  ignore(open_db ~rm:false init name);
  ignore(open_db ~rm:false init name)

let test_save () =
  let db = open_db init name in
  x_save db x1;
  x_save db x2

let test_update () =
  let db = open_db init name in
  x_save db x1;
  x_save db x2;
  x_save db x1;
  x_save db x2

let test_get () =
  let db = open_db ~rm:false init name in
  let i = x_get db in
  "2 in db" @? (List.length i = 2);
  match i with 
  | [a2;a1] ->
    "x1 values match" @? (a1.foo = x1.foo && (a1.bar = x1.bar));
    "x2 values match" @? (a2.foo = x2.foo && (a2.bar = x2.bar))
  | _ -> assert false

let test_save_get () =
  let db = open_db init name in
  x_save db x1;
  let i = x_get db in
  match x_get db with
  [i] -> "structurally equal after get" @? ( x1 == i)
  |_ -> assert false

let suite = [
  "variant_init" >:: test_init;
  "variant_save" >:: test_save;
  "variant_update" >:: test_update;
  "variant_update" >:: test_update;
  "variant_get" >:: test_get;
  "variant_save_get" >:: test_save_get;
]


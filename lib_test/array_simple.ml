TYPE_CONV_PATH "Array_simple"

type s = {
  foo: int array;
  bar: string
} with
orm()

open Printf
open Orm
open OUnit
open Test_utils

let name = "array_simple.db"
let t1 = { foo=[|1|]; bar="t1" }
let t2 = { foo=[|1;2;3|]; bar="t2" }
let t3 = { foo=[||]; bar="t3" }

let test_init () =
  ignore(open_db init name);
  ignore(open_db ~rm:false init name);
  ignore(open_db ~rm:false init name)

let test_save () =
  let db = open_db init name in
  s_save db t1;
  s_save db t2;
  s_save db t3

let test_update () =
  let db = open_db init name in
  s_save db t1;
  s_save db t2;
  s_save db t3;
  s_save db t1;
  s_save db t2;
  s_save db t3

let test_get () =
  let db = open_db ~rm:false init name in
  let i = s_get db in
  "3 in db" @? (List.length i = 3);
  let i = List.hd i in
  "values match" @? (i.bar = t3.bar && (i.foo = t3.foo))

let test_save_get () =
  let db = open_db init name in
  s_save db t3;
  let i = s_get db in
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "structural values equal" @? ( t3 = i);
  "physical values equal" @? ( t3 == i)

let test_delete () =
  let db = open_db init name in
  s_save db t1;
  s_save db t2;
  s_save db t3;
  "3 in db" @? (List.length (s_get db) = 3);
  s_delete db t2;
  "2 in db" @? (List.length (s_get db) = 2);
  (match s_get db with
  [a3;a1] -> "equal" @? (a3=t3 && a1=t1)
  |_ -> assert false);
  s_delete db t1;
  s_delete db t3;
  "0 in db" @? (List.length (s_get db) = 0)

let suite = [
  "array_simple_init" >:: test_init;
  "array_simple_save" >:: test_save;
  "array_simple_update" >:: test_update;
  "array_simple_get" >:: test_get;
  "array_simple_save_get" >:: test_save_get;
  "array_simple_delete" >:: test_delete;
]



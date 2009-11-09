TYPE_CONV_PATH "List_foreign"

type s = {
  foo: int;
  bar: string
} 
and x = {
  one: s list;
}
with
orm()

let t1 = {foo=1; bar="t1"}
let t2 = {foo=2; bar="t2"}
let x1 = {one=[t1;t2;t1] }
let x2 = {one=[]}

let name = "list_foreign.db"

open Orm
open OUnit
open Test_utils

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
  "2 x in db" @? (List.length (x_get db) = 2);
  "2 s in db" @? (List.length (s_get db) = 2)

let test_save_get () =
  let db = open_db init name in
  x_save db x1;
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "structural values equal" @? ( x1 = i);
  "physical values equal" @? ( x1 == i)

let suite = [
  "list_foreign_init" >:: test_init;
  "list_foreign_save" >:: test_save;
  "list_foreign_update" >:: test_update;
  "list_foreign_get" >:: test_get;
  "list_foreign_save_get" >:: test_save_get;
]



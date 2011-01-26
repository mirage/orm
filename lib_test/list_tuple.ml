TYPE_CONV_PATH "List_tuple"

type x = {
  foo: (int * char list) list;
  bar: string
} with orm

open OUnit
open Test_utils

let x1 = {foo=[(1,['x';'y'])] ; bar="hello world" }
let x2 = {foo=[(2,[]); (3,['a';'b';'c']); (4,['a'])] ; bar="world hello" }
let x3 = {foo=[] ; bar="world hello" }

let name = "list_tuple.db"

let test_init () =
  ignore(open_db x_init name);
  ignore(open_db ~rm:false x_init name);
  ignore(open_db ~rm:false x_init name)

let test_save () =
  let db = open_db x_init name in
  x_save db x1;
  x_save db x2;
  x_save db x3

let test_update () =
  let db = open_db x_init name in
  x_save db x1;
  x_save db x2;
  x_save db x2;
  x_save db x1;
  x_save db x2;
  x_save db x3

let test_get () =
  let db = open_db ~rm:false x_init name in
  "3 x in db" @? (List.length (x_get db) = 3)

let test_save_get () =
  let db = open_db x_init name in
  x_save db x1;
  x_save db x2;
  x_save db x3;
  match x_get db with 
  |[a1;a2;a3] ->
    "structural values equal" @? ( x1 = a1);
    "physical values equal" @? ( x1 == a1);
    "structural values equal" @? ( x2 = a2);
    "physical values equal" @? ( x2 == a2);
    "structural values equal" @? ( x3 = a3);
    "physical values equal" @? ( x3 == a3)
  |_ -> assert false

let suite = [
  "list_tuple_init" >:: test_init;
  "list_tuple_save" >:: test_save;
  "list_tuple_update" >:: test_update;
  "list_tuple_get" >:: test_get;
  "list_tuple_save_get" >:: test_save_get;
]


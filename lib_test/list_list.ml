TYPE_CONV_PATH "List_list"

type x = {
  foo: int list list;
  bar: string
} with orm

open OUnit
open Test_utils

let name = "list_list.db"

let x1 = {foo=[[1]]; bar="t1" }
let x2 = {foo=[[2;3;4];[6;7]] ;bar="t2"}
let x3 = {foo=[]; bar="t3" }

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
  "list_list_init" >:: test_init;
  "list_list_save" >:: test_save;
  "list_list_update" >:: test_update;
  "list_list_update" >:: test_update;
  "list_list_get" >:: test_get;
  "list_list_save_get" >:: test_save_get;
]

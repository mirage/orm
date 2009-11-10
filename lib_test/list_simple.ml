TYPE_CONV_PATH "List_simple"

type x = {
  foo: string list;
  bar: (char * string) list;
  pla: bool;
} with orm(debug:none)

open OUnit
open Test_utils

let name = "list_simple.db"
let x1 = { foo=["a1";"a2";"a3";"a4"]; bar=[ ('a',"AA"); ('b',"BB"); ('c',"CC") ] ; pla=true }

let test_init () =
  ignore(open_db x_init name);
  ignore(open_db ~rm:false x_init name);
  ignore(open_db ~rm:false x_init name)

let test_save () =
  let db = open_db x_init name in
  x_save db x1

let test_update () =
  let db = open_db x_init name in
  x_save db x1;
  x_save db x1

let test_get () =
  let db = open_db ~rm:false x_init name in
  "1 x in db" @? (List.length (x_get db) = 1)

let test_save_get () =
  let db = open_db x_init name in
  x_save db x1;
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "structural values equal" @? ( x1 = i);
  "physical values equal" @? ( x1 == i)

let suite = [
  "list_simple_init" >:: test_init;
  "list_simple_save" >:: test_save;
  "list_simple_update" >:: test_update;
  "list_simple_update" >:: test_update;
  "list_simple_get" >:: test_get;
  "list_simple_save_get" >:: test_save_get;
]


TYPE_CONV_PATH "List_share"

type x = {
  foo: string list;
} with orm(debug:all)

open OUnit
open Test_utils

let name = "list_share.db"
let x1 = { foo=["a";"p";"w"] }
let x2 = { foo=["g";"m";"p"] }
let x3 = { foo=["m";"p";"z"] }
let x4 = { foo=["p"] }

let test_init () =
  ignore(open_db x_init name);
  ignore(open_db ~rm:false x_init name);
  ignore(open_db ~rm:false x_init name)

let test_save () =
  let db = open_db x_init name in
  x_save db x1;
  x_save db x2;
  x_save db x3;
  x_save db x4

let test_update () =
  let db = open_db x_init name in
  x_save db x1;
  x_save db x1;
  x_save db x2;
  x_save db x2;
  x_save db x3;
  x_save db x3;
  x_save db x4;
  x_save db x4

let test_get () =
  let db = open_db ~rm:false x_init name in
  "4 x in db" @? (List.length (x_get db) = 4)

let test_save_get () =
  let db = open_db x_init name in
  x_save db x1;
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "structural values equal" @? ( x1 = i);
  "physical values equal" @? ( x1 == i)

let suite = [
  "list_share_init" >:: test_init;
  "list_share_save" >:: test_save;
  "list_share_update" >:: test_update;
  "list_share_get" >:: test_get;
  "list_share_save_get" >:: test_save_get; 
]


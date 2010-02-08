TYPE_CONV_PATH "Large_string"

open Printf

type x = {
  name: string;
  body: string
} with orm

open OUnit
open Test_utils

let name = "large_string.db"
let size = 10000000
let x1 = { name = "x1"; body = String.make size 'x' }
let x2 = { name = "x2"; body = String.make size 'y' }
let x3 = { name = "x3"; body = String.make size 'z' }

let test_init () =
  ignore(open_db x_init name);
  ignore(open_db ~rm:false x_init name);
  ignore(open_db ~rm:false x_init name)

let test_save () =
  let db = open_db x_init name in
  let _ = open_db ~rm:false x_init_read_only name in
  x_save db x1;
  x_save db x2;
  x_save db x3

let test_update () =
  let db = open_db x_init name in
  List.iter (fun i ->
    x_save db x1;
    x_save db x2;
    x_save db x3;
  ) [1;2;3;4;5;6;7;8;9;10]

let suite = [
  "large_string_init" >:: test_init;
  "large_string_save" >:: test_save;
  "large_string_update" >:: test_update;
]


TYPE_CONV_PATH "Object_simple"

open Printf

type x = <
  foo: int;
  bar: string
> with
  orm()

open OUnit
open Test_utils

let name = "object_simple.db"

let r = Random.int 100
let x b = object
  method foo = r
  method bar="hello "^b
end

let x1 = x "world"
let x2 = x "sky"

let test_init () =
  ignore(open_db x_init name);
  ignore(open_db ~rm:false x_init name);
  ignore(open_db ~rm:false x_init name)

let test_id () =
  let db = open_db x_init name in
   x_save db x1;
   let i = x_id db x1 in
   "id is 1" @? (i = 1L);
   assert_raises ~msg:"test_id_not_found" Not_found 
    (fun () -> x_id db x2)
   
let test_save () =
  let db = open_db x_init name in
  let _ = open_db ~rm:false x_init_read_only name in
  x_save db x1;
  x_save db x2

let test_update () =
  let db = open_db x_init name in
  x_save db x1;
  x_save db x1

let test_get () =
  let db = open_db ~rm:false x_init name in
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "values match" @? (i#foo = x1#foo && (i#bar = x1#bar))

let suite = [
  "object_simple_init" >:: test_init;
  "object_simple_id" >:: test_id;
  "object_simple_save" >:: test_save;
  "object_simple_update" >:: test_update;
  "object_simple_get" >:: test_get;
]


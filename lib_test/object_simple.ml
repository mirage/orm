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

let x b = object
  method foo = (Random.int 100)
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
  "2 in db" @? (List.length i = 1);
  let i = List.hd i in
  "values match" @? (i#foo = x1#foo && (i#bar = x1#bar))

let suite = [
  "simple_init" >:: test_init;
  "simple_id" >:: test_id;
  "simple_save" >:: test_save;
  "simple_update" >:: test_update;
  "simple_get" >:: test_get;
]


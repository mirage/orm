TYPE_CONV_PATH "Simple"

open Printf

module A = struct
  type x = {
    foo: int;
    bar: string
  } with
  orm(
    unique: x<foo,bar>, x<bar>;
    debug: all;
    dot: "simple.dot"
  )
end

module B = struct
  type x = {
    foo: int64;
  } with orm (debug: all)
end

open A
open OUnit
open Test_utils

let name = "simple.db"

let x = { foo = (Random.int 100); bar="hello world" }

let test_init () =
  ignore(open_db x_init name);
  ignore(open_db ~rm:false x_init name);
  ignore(open_db ~rm:false x_init name)

let test_id () =
  let db = open_db x_init name in
   x_save db x;
   let i = x_id db x in
   "id is 1" @? (i = 1L);
   let x2 = { foo=100; bar="x2222" } in
   assert_raises ~msg:"test_id_not_found" Not_found 
    (fun () -> x_id db x2)

let test_save () =
  let db = open_db x_init name in
  x_save db x

let test_update () =
  let db = open_db x_init name in
  x_save db x;
  x_save db x

let test_subtype () =
  let db = open_db ~rm:false B.x_init name in
  let i = B.x_get db in
  "2 in db" @? (List.length i = 1);
  let i = List.hd i in
  "values match" @? (i.B.foo = Int64.of_int x.foo)

let test_get () =
  let db = open_db ~rm:false x_init name in
  let i = x_get db in
  "2 in db" @? (List.length i = 1);
  let i = List.hd i in
  "values match" @? (i.foo = x.foo && (i.bar = x.bar))

let test_save_get () =
  let db = open_db init name in
  x_save db x;
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "structurally equal after get" @? ( x == i)

let test_delete () =
  let db = open_db ~rm:false x_init name in
  let x1 = match x_get db with [x] -> x |_ -> assert false in
  let x2 = { foo = (Random.int 100); bar="x2" } in
  let x3 = { foo = (Random.int 100); bar="x3" } in
  "1 in db" @? (List.length (x_get db) = 1);
  x_delete db x1;
  "0 in db" @? (List.length (x_get db) = 0);
  x_save db x1;
  x_save db x2;
  x_save db x3;
  "3 in db" @? (List.length (x_get db) = 3);
  x_delete db x2;
  "2 in db" @? (List.length (x_get db) = 2);
  match x_get db with
  [a3;a1] -> "equal" @? (a3=x3 && a1=x1)
  |_ -> assert false
  
let suite = [
  "simple_init" >:: test_init;
  "simple_id" >:: test_id;
  "simple_save" >:: test_save;
  "simple_update" >:: test_update;
  "simple_get" >:: test_get;
  "simple_subtype" >:: test_subtype;
  "simple_save_get" >:: test_save_get;
  "simple_delete" >:: test_delete;
]


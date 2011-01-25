TYPE_CONV_PATH "Simple"

open Printf

type x = {
  foo: int;
  bar: string
} with
  orm

open OUnit
open Test_utils

let name = "simple.db"

let x = { foo = (Random.int 100); bar="hello world" }
let x2 = { foo = (Random.int 100); bar="bye world" }

let test_init () =
  ignore(open_db x_init name);
  ignore(open_db ~rm:false x_init name);
  ignore(open_db ~rm:false x_init name)

let test_id () =
  let db = open_db x_init name in
   x_save db x;
   let i = x_id db x in
   "id is 1" @? (ORMID_x.to_int64 i = 1L);
   let x' = x_get_by_id (`Eq i) db in
   "bar eq" @? (x'.bar = x.bar);
   let x2 = { foo=100; bar="x2222" } in
   assert_raises ~msg:"test_id_not_found" Not_found (fun () -> x_id db x2);
   assert_raises ~msg:"test_id_not_found2" Not_found (fun () -> x_get_by_id ~id:(`Eq (ORMID_x.of_int64 5L)) db)

let test_save () =
  let db = open_db x_init name in
  let _ = open_db ~rm:false x_init_read_only name in
  x_save db x;
  x_save db x

let test_update () =
  let db = open_db x_init name in
  x_save db x;
  x_save db x

let test_subtype () =
  let module A = struct
    type x = {
      foo: int64;
    } with orm 
  end in
  let db = open_db ~rm:false A.x_init_read_only name in
  let i = A.x_get ~foo:(`Eq (Int64.of_int x.foo)) db in
  "2 in db" @? (List.length i = 1);
  let i = List.hd i in
  "values match" @? (i.A.foo = Int64.of_int x.foo)

let test_get () =
  let db = open_db x_init name in
  x_save db x;
  x_save db x2;
  let i = x_get ~bar:(`Eq "hello world") db in
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "values match" @? (i.foo = x.foo && (i.bar = x.bar))

let test_custom_get () =
  let db = open_db x_init name in
  x_save db x;
  x_save db x2;
  let nb = ref 0 in
  let subs = ref [] in
  let i = x_get ~custom:(fun x -> let s = String.sub x.bar 3 2 in subs := s :: !subs; if s="lo" then (incr nb; true) else false) db in
  (Printf.sprintf "1 in db (nb=%d, subs={%s})" !nb (String.concat ", " !subs)) @? (List.length i = 1)

let test_save_get () =
  let db = open_db x_init name in
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
  "simple_custom_get" >:: test_custom_get;
  "simple_subtype" >:: test_subtype;
  "simple_save_get" >:: test_save_get;
  "simple_delete" >:: test_delete;
]


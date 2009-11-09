TYPE_CONV_PATH "Recursive"

type s = { 
  t1: string;
  mutable t2: x option
}
and x = {
  x1: s option;
  mutable x2: char
}
with orm ( debug: all; dot: "recursive.dot" )

open Orm
open Test_utils
open OUnit
open Printf

let ps s = eprintf "{ t1=%s t2=%c }\n" s.t1 (match s.t2 with |None -> '?' |Some x -> x.x2)
let name = "recursive.db"

let rec vt = { t1= "hello"; t2=(Some vx) }
and vx = { x1=(Some vt); x2='z' }

let test_init () =
  ignore(open_db init name);
  ignore(open_db ~rm:false init name);
  ignore(open_db ~rm:false init name)

let test_save () =
  let db = open_db init name in
  x_save db vx

let test_update () =
  let db = open_db init name in
  x_save db vx;
  s_save db vt;
  x_save db vx;
  s_save db vt

let test_get () =
  let db = open_db ~rm:false init name in
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "values match" @? (i.x2 = vx.x2)

let test_save_get () =
  let db = open_db init name in
  x_save db vx;
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "physical values equal" @? ( vx == i)

let test_delete () = 
  let db = open_db init name in
  x_save db vx;
  "1 x in db" @? (List.length (x_get db) = 1);
  "1 s in db" @? (List.length (s_get db) = 1);
  x_delete db vx;
  "0 x in db" @? (List.length (x_get db) = 0);
  "1 s in db" @? (List.length (s_get db) = 1)

let suite = [
  "recursive_init" >:: test_init;
  "recursive_save" >:: test_save;
  "recursive_update" >:: test_update;
  "recursive_get" >:: test_get;
  "recursive_save_get" >:: test_save_get;
  "recursive_delete" >:: test_delete;
]


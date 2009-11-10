TYPE_CONV_PATH "Foreign_and_variant"

type v =
| Alice
| Bob of int
| Charlie of (int * int64)
and s = {
  foo: string;
  bar: int64;
  xyz: v;
} and x = {
  first: s;
  second: s;
  third: int;
} with orm

open OUnit
open Test_utils

let name = "foreign_and_variant.db"

let v = Charlie (1002,1003L) 
let s = { foo="s1"; bar=99L; xyz=v } 
let x = { first=s; second=s; third=99 }

let test_init () =
  ignore(open_db v_init name);
  ignore(open_db ~rm:false s_init name);
  ignore(open_db ~rm:false x_init name)

let test_save () =
  let db = open_db x_init name in
  x_save db x

let test_update () =
  let db = open_db x_init name in
  x_save db x;
  x_save db x

let test_get () =
  let db = open_db ~rm:false x_init name in
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  match i with 
  | [a] ->
    "values match" @? (a.first = x.first)
  | _ -> assert false

let test_save_get () =
  let db = open_db x_init name in
  x_save db x;
  match x_get db with
  [i] -> "structurally equal after get" @? ( x == i)
  |_ -> assert false

let suite = [
  "foreign_and_variant_init" >:: test_init;
  "foreign_and_variant_save" >:: test_save;
  "foreign_and_variant_update" >:: test_update;
  "foreign_and_variant_update" >:: test_update;
  "foreign_and_variant_get" >:: test_get;
  "foreign_and_variant_save_get" >:: test_save_get;
]



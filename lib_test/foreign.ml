(*pp camlp4o -I ../lib -I `ocamlfind query type-conv` pa_type_conv.cmo pa_orm.cma *)

TYPE_CONV_PATH "Foreign"

type t = {
  foo: string;
  bar: int64;
  mutable xyz: char;
}
and x = {
  first: t;
  mutable second: t;
  third: int;
}
with orm (
 debug:sql;
 unique: t<xyz>, t<bar>;
 index: x<first,second>
)

let name = "foreign.db"
let s1 = { foo="hello"; bar=100L; xyz='a' }
let s2 = { foo="world"; bar=200L; xyz='z' }
let x = { first=s1; second=s2; third=111 }

open Orm
open Test_utils
open OUnit

let test_init () =
  ignore(open_db init name);
  ignore(open_db ~rm:false init name);
  ignore(open_db ~rm:false init name)

let test_save () =
  let db = open_db init name in
  x_save db x

let test_update () =
  let db = open_db init name in
  x_save db x;
  x_save db x

let test_get () =
  let db = open_db ~rm:false init name in
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "values match" @? (i.first = x.first && (i.second = x.second))

let test_save_get () =
  let db = open_db init name in
  x_save db x;
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  match i with
  [i] ->
    "structural values equal" @? ( x = i);
    "physical values equal" @? ( x == i)
  |_ -> assert false

let suite = [
  "foreign_init" >:: test_init;
  "foreign_save" >:: test_save;
  "foreign_update" >:: test_update;
  "foreign_get" >:: test_get;
  "foreign_save_get" >:: test_save_get;
]



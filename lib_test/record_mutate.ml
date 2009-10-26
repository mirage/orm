(*pp camlp4o -I ../lib -I `ocamlfind query type-conv` pa_type_conv.cmo pa_orm.cma *)

TYPE_CONV_PATH "Record_mutate"

type t = {
  foo: string;
  mutable bar: string option
} with orm(
    debug: all;
    unique: t<foo>
  )

open Printf
open Orm
open Test_utils
open OUnit

let name = "record_mutate.db" 

let test_mutate_option () =
  let db = open_db init name in
  let t1 = { foo="foo"; bar=None } in
  t_save db t1;
  t1.bar <- Some "bar";
  t_save db t1

let suite = [
  "record_mutate_option" >:: test_mutate_option;
]

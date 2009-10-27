(*pp camlp4o -I ../lib -I `ocamlfind query type-conv` pa_type_conv.cmo pa_orm.cma *)

TYPE_CONV_PATH "Record_mutate"

type x = {
  mutable foo: string;
  mutable bar: string option
} with orm(
    debug: leak,all;
    unique: x<foo>
  )

open Printf
open Orm
open Test_utils
open OUnit

module H = Hashtbl.Make (
  struct 
    type t = x
    let equal = (==)
    let compare = (==)
    let hash _ = 0
  end )

let name = "record_mutate.db" 

let test_mutate_nodb () =
  let t1 = { foo="foo"; bar=None } in
  let r = ref t1 in
  "phys eq" @? (!r == t1);
  t1.bar <- Some "bar";
  "phys eq after mutate" @? (!r == t1)

let test_mutate_nodb_hash () =
  let t1 = { foo="foo"; bar=None } in
  let h = H.create 1 in
  for i = 0 to 10000; do
    H.add h { foo=(sprintf "foo%d" i); bar=None } (Random.int64 1000L)
  done;
  H.add h t1 1L;
  "in hash1" @? (H.find h t1 = 1L);
  t1.bar <- Some "bar";
  "in hash2" @? (H.find h t1 = 1L)

let test_mutate_basic () =
  let db = open_db init name in
  let t1 = { foo="foo"; bar=None } in
  x_save db t1;
  t1.foo <- "foo2";
  x_save db t1

(* same as previous, but changes bar *)
let test_mutate_option () =
  let db = open_db init name in
  let t1 = { foo="foo"; bar=None } in
  x_save db t1;
  t1.bar <- Some "bar";
  x_save db t1

let suite = [
  "record_mutate_nodb" >:: test_mutate_nodb;
  "record_mutate_nodb_hash" >:: test_mutate_nodb_hash;
  "record_mutate_basic" >:: test_mutate_basic;
  "record_mutate_option" >:: test_mutate_option;
]

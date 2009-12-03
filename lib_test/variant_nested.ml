TYPE_CONV_PATH "Variant_nested"

type n =
  | Non of int64
  | NTwo
  | Nthree of string
  | Nfour of x
and x =
  | XONE
  | Xtwo of n
  | Xthree of int
and t = {
  foo: x;
  bar: n;
  xyz: char;
} with orm

open OUnit
open Test_utils

let t1 = {foo = XONE; bar = Nfour (Xthree 34); xyz = 'a' }
let t2 = {foo = Xtwo (Nfour XONE) ; bar = Nfour (Xthree 12) ;xyz = 'b' }
let t3 = {foo = Xtwo (Nfour (Xthree 32)) ; bar = Nfour XONE ; xyz = 'X' }

let name = "variant_nested.db"

let test_save () =
  let db = open_db t_init name in
  t_save db t1;
  t_save db t2;
  t_save db t3

let test_subtype () =
  let module A = struct
  type n =
      | Non of int64
      | NTwo
      | Nthree of string
      | Nfour of x
    and x = 
      | XONE
      | Xtwo of n
      | Xthree of int64
    and t = {
      bar: n;
      xyz: int64;
    } with orm
  end in
  let db = open_db ~rm:false A.t_init_read_only name in
  let ts = A.t_get db in
  "3 in db" @? (List.length ts = 3);
  let t = List.hd (List.filter (fun t -> t.A.bar = A.Nfour A.XONE) ts) in
  "value match" @? (t.A.xyz = 88L)

let suite = [
  "variant_nested_save" >:: test_save;
  "variant_nested_subtype" >:: test_subtype
]

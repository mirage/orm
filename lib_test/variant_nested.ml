TYPE_CONV_PATH "Variant_nested"

module A = struct
  type n =
  | Non of int64
  | Ntwo
  | Nthree of string
  | Nfour of x
  and x = 
  | Xone
  | Xtwo of n
  | Xthree of int
  and t = {
    bar: n;
    xyz: int64;
  } with orm()
end

module B = struct
  type n =
  | Nfour of x
  and x =
  | Xone
  | Xtwo of n
  | Xthree of int
  and t = {
    foo: x;
    bar: n;
    xyz: int;
  } with orm()
end

open B
open Orm
open OUnit
open Test_utils

let t1 = {foo = Xone; bar = Nfour (Xthree 34); xyz = 12 }
let t2 = {foo = Xtwo (Nfour Xone) ; bar = Nfour (Xthree 12) ;xyz = 66 }
let t3 = {foo = Xtwo (Nfour (Xthree 32)) ; bar = Nfour Xone ; xyz = 88 }

let name = "variant_nested.db"

let test_save () =
  let db = open_db init name in
  t_save db t1;
  t_save db t2;
  t_save db t3

let test_subtype () =
  let db = open_db ~rm:false A.Orm.init name in
  let ts = A.Orm.t_get db in
  "3 in db" @? (List.length ts = 3);
  let t = List.hd (List.filter (fun t -> t.A.bar = A.Nfour A.Xone) ts) in
  "value match" @? (t.A.xyz = 88L)

let suite = [
  "variant_nested_save" >:: test_save;
  "variant_nested_subtype" >:: test_subtype
]

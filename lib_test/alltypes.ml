(*pp camlp4o -I ../lib -I `ocamlfind query type-conv` pa_type_conv.cmo pa_orm.cma *)

TYPE_CONV_PATH "All_types"
open Printf
open OUnit
open Test_utils

let name = "alltypes.db"

type x = {
  one: char;
  two: string;
  three: int;
  four: int32;
  five: bool;
  six: int64;
  seven: unit;
  eight: string option;
  nine: float;
  ten: (int * string);
  eleven: string list;
  twelve: (char * int32 * unit) option;
  thirteen: (char * (string * int64) option);
} with orm()

let name = "alltypes.db"
let x = { one='a'; two="foo"; three=1; four=2l; 
     five=true; six=3L; seven=(); eight=(Some "bar");
     nine=6.9; ten=(100,"hello"); eleven=["aa";"bb";"cc"];
     twelve=(Some ('t',9l,())); thirteen=('d', (Some ("abc",999L))) }
(*
  List.iter
    (fun t -> Printf.printf "found <one=%c> where two='foo'\n%!" t#one)
    (Orm.t_get ~two:(`Eq "foo") db);
  List.iter
    (fun t -> Printf.printf "found <one=%c> where two contains 'o'\n%!" t#one)
    (Orm.t_get ~two:(`Contains "o") db)
*)

open Orm
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
  "2 in db" @? (List.length i = 1);
  let i = List.hd i in
  "values match" @? (i.six = x.six)

let test_save_get () =
  let db = open_db init name in
  x_save db x;
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "structural values equal" @? ( x = i);
  "physical values equal" @? ( x == i)

let suite = [
  "alltypes_init" >:: test_init;
  "alltypes_save" >:: test_save;
  "alltypes_update" >:: test_update;
  "alltypes_get" >:: test_get;
  "alltypes_save_get" >:: test_save_get;
]


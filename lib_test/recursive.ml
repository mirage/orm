TYPE_CONV_PATH "Recursive"

type t = { 
  t1: string;
  t2: x option
} and x = {
  x1: t option;
  x2: char
} with orm

open Test_utils
open OUnit

let name = "recursive.db"

let rec vt = { t1= "hello"; t2=(Some vx) }
and vx = { x1=(Some vt); x2='z' }

let test_init () =
  ignore(open_db t_init name);
  ignore(open_db ~rm:false x_init name);
  ignore(open_db ~rm:false x_init name)

let test_save () =
  let db = open_db x_init name in
  x_save db vx

let test_update () =
  let dbx = open_db x_init name in
  let dbt = open_db ~rm:false t_init name in
  x_save dbx vx;
  t_save dbt vt;
  x_save dbx vx;
  t_save dbt vt

let test_get () =
  let db = open_db ~rm:false x_init name in
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "values match" @? (i.x2 = vx.x2)

let test_save_get () =
  let db = open_db x_init name in
  x_save db vx;
  let i = x_get db in
  "1 in db" @? (List.length i = 1);
  let i = List.hd i in
  "physical values equal" @? ( vx == i)

let test_delete () = 
  let dbx = open_db x_init name in
  let dbt = open_db ~rm:false t_init name in
  x_save dbx vx;
  "1 x in db" @? (List.length (x_get dbx) = 1);
  "1 s in db" @? (List.length (t_get dbt) = 1);
  x_delete dbx vx;
  "0 x in db" @? (List.length (x_get dbx) = 0);
  "1 s in db" @? (List.length (t_get dbt) = 1)

let suite = [
  "recursive_init" >:: test_init;
  "recursive_save" >:: test_save;
  "recursive_update" >:: test_update;
  "recursive_get" >:: test_get;
  "recursive_save_get" >:: test_save_get;
  "recursive_delete" >:: test_delete;
]


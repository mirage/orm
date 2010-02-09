TYPE_CONV_PATH "Recursive"

type y = char with orm

type t = { 
  t1: string;
  t2: x option
} and x = {
  x1: t option;
  x2: char
} with orm

type z = t with orm

type a = { a : y } with orm

open Test_utils
open OUnit

let name = "recursive.db"

let vy = 'a'
let rec vt = { t1= "hello"; t2=(Some vx) }
and vx = { x1=(Some vt); x2=vy }
let vz = vt
let va = { a = vy }

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

(* We have:  vz => vt <=> vx => vy <= va                                      *)
(* Deletion should be possible if no reference exists to the recursive cycle  *)
(* the value belongs to.                                                      *)
(* For instance, deleting vx while vz is still in the database should not be  *)
(* possible. However, deleting vz should delete vt and vx as well, but let vy *)
(* is in the dabatase as va is referencing it.                                *)
let test_delete () =
  let dbz = open_db z_init name in
  let dbt = open_db ~rm:false t_init name in
  let dbx = open_db ~rm:false x_init name in
  let dby = open_db ~rm:false y_init name in
  let dba = open_db ~rm:false a_init name in

  let check (z, t, x, y, a) =
    (Printf.sprintf "%d z in db" z) @? (List.length (z_get dbz) = z);
    (Printf.sprintf "%d t in db" t) @? (List.length (t_get dbt) = t);
	(Printf.sprintf "%d x in db" x) @? (List.length (x_get dbx) = x);
	(Printf.sprintf "%d y in db" y) @? (List.length (y_get dby) = y);
	(Printf.sprintf "%d a in db" a) @? (List.length (a_get dba) = a) in

  z_save dbz vz;
  a_save dba va;
  
  (* 0. basic sanity checks before doing the delete test *)
  check (1, 1, 1, 1, 1);

  (* 1. deleting vx should not be possible *)
  x_delete dbx vx;
  check (1, 1, 1, 1, 1);

  (* 2. deleting vz should delete vt and vx as well and let vy in the database *)
  z_delete dbz vz;
  check (0, 0, 0, 1, 1)

let suite = [
  "recursive_init" >:: test_init;
  "recursive_save" >:: test_save;
  "recursive_update" >:: test_update;
  "recursive_get" >:: test_get;
  "recursive_save_get" >:: test_save_get;
  "recursive_delete" >:: test_delete;
]


TYPE_CONV_PATH "Recursive_mutate"

type t = { 
  t1: string;
  t2: x option;
  mutable t3: char;
}
and x = {
  x1: t option;
  mutable x2: char; 
  x3: int64
}
with orm 

open Test_utils
open OUnit


let rec x_equal ~depth u v =
  depth = 0 ||
  (match u.x1, v.x1 with
  | None, None       -> true
  | Some t1, Some t2 -> t_equal ~depth:(depth-1) t1 t2
  | _                -> false)
  && u.x2 = v.x2
  && u.x3 = v.x3

and t_equal ~depth u v =
  depth = 0 ||
  u.t1 = v.t1
  && (match u.t2, v.t2 with
  | None, None       -> true
  | Some x1, Some x2 -> x_equal ~depth:(depth-1) x1 x2
  | _                -> false)
  && u.t3 = v.t3

let name = "recursive_mutate.db"

let rec vt = { t1= "hello"; t2=(Some vx); t3='z' }
and vx = { x1=(Some vt); x2='z'; x3=1L }

let test_init () =
  ignore(open_db x_init name);
  ignore(open_db ~rm:false t_init name);
  ignore(open_db ~rm:false x_init name)

let test_save () =
  let db = open_db x_init name in
  x_save db vx

let test_update () =
  let dbt = open_db t_init name in
  let dbx = open_db ~rm:false x_init name in
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
  "values equal" @? ( x_equal ~depth:10 vx i)

let test_delete () = 
  let db = open_db x_init name in
  let dbt = open_db ~rm:false t_init_read_only name in
  x_save db vx;
  "1 x in db" @? (List.length (x_get db) = 1);
  "1 s in db" @? (List.length (t_get dbt) = 1);
  x_delete db vx;
  "0 x in db" @? (List.length (x_get db) = 0);
  "1 s in db" @? (List.length (t_get dbt) = 1)

let suite = [
  "recursive_mutate_init" >:: test_init;
  "recursive_mutate_save" >:: test_save;
  "recursive_mutate_update" >:: test_update;
  "recursive_mutate_get" >:: test_get;
  "recursive_mutate_save_get" >:: test_save_get;
  "recursive_mutate_delete" >:: test_delete;
]


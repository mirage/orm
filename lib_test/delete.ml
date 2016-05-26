(*pp camlp4orf *)

open Test_utils
open OUnit

type t = string with orm
type u = int * t with orm

let t = "foo"
let u = (3, t)

let name = "delete.db"

let test_delete () =
  let dbt = open_db t_init name in
  let dbu = open_db ~rm:false u_init name in

  let check n (t, u) =
    (Printf.sprintf "%d: %d t in db" n t) @? (List.length (t_get dbt) = t);
    (Printf.sprintf "%d: %d u in db" n u) @? (List.length (u_get dbu) = u) in

  u_save dbu u;
  check 0 (1, 1);

  (* recursive delete should delete all non-referenced sub-values *)
  u_delete dbu u;
  check 1 (0, 0);

  (* non-recursive delete should delete only the parent value *)
  u_save dbu u;
  u_delete ~recursive:false ~db:dbu u;
  check 2 (1, 0)

let suite = [
  "delete" >:: test_delete;
]

TYPE_CONV_PATH "Stress"

open OUnit
open Test_utils
type t = { a : int; b : int } with orm ()
open Orm

let time = Unix.gettimeofday
let name = "stress.db"

let test_bench () =
  let db = open_db init name in
  let t0 = time () in
  for i=0 to 1000 do
    let x = { a=Random.int 10; b=i } in
    t_save db x;
    if i mod 1000 = 0 then Printf.printf "Saved %i records in %.2fs\n%!" i ((time ()) -. t0);
  done;
  let all = t_get db in (* get all the elements in the database *)
  Printf.printf "timing (total: %i elements): %!" (List.length all);
  (*
  let t1 = time () in
  let l1 = Orm.t_get ~a:(`Eq 5) db in
  let t2 = time () in
  let l2 = Orm.t_get ~fn:(fun t -> t#a = 5) db in
  let t3 = time () in
  Printf.printf "get_where: %.4f (filtered: %i elements); get_custom: %.4f (filtered: %i elements)\n%!"
    (t2 -. t1) (List.length l1) (t3 -. t2) (List.length l2)
  *)
  ()

(* One result with big tables:

timing (total: 229733 elements):
get_where: 0.1541 (filtered: 23157 elements);
get_custom: 0.6346 (filtered: 23157 elements)

*)

let test_cache () =
  let db = open_db init name in
  for i = 0 to 1000 do
    let x = { a=Random.int 10; b=i } in
    t_save db x;
    if i mod 1000 = 0 then Printf.printf ".%!"
  done;
  for i = 0 to 10 do
    List.iter (fun x -> ignore(t_id db x)) (t_get db)
  done

let suite = [
  "stress_bench" >:: test_bench;
  "stress_cache" >:: test_cache;
]

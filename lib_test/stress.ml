TYPE_CONV_PATH "Stress"

type t = { a : int; b : int } with orm

open OUnit
open Printf
open Test_utils

let time = Unix.gettimeofday

let name = "stress.db"

let time_of fn =
  let t1 = time () in
  let r = fn () in
  let t2 = time () in
  t2 -. t1, r

let test_bench name () =
  let db = open_db t_init name in
  let t0 = time () in
  for i=0 to 4000 do
    let x = { a=Random.int 10; b=i } in
    t_save db x;
    if i mod 4000 = 0 then Printf.printf "Saved %i records in %.2fs\n%!" i ((time ()) -. t0);
  done;
  let t,all = time_of (fun () -> t_get db) in (* get all the elements in the database *)
  printf "timing %s (total: %i elements): %!" name (List.length all);
  printf "full_get: %f %!" t;
  (*
  let t1 = time () in
  let l1 = t_get ~a:(`Eq 5) db in
  let t2 = time () in
  let l2 = t_get ~fn:(fun t -> t#a = 5) db in
  let t3 = time () in
  Printf.printf "get_where: %.4f (filtered: %i elements); get_custom: %.4f (filtered: %i elements)\n%!"
    (t2 -. t1) (List.length l1) (t3 -. t2) (List.length l2)
  *)
  printf "\n%!"

(* One result with big tables:

timing (total: 229733 elements):
get_where: 0.1541 (filtered: 23157 elements);
get_custom: 0.6346 (filtered: 23157 elements)

*)

let test_cache () =
  let db = open_db t_init name in
  for i = 0 to 1000 do
    let x = { a=Random.int 10; b=i } in
    t_save db x;
    if i mod 1000 = 0 then Printf.printf ".%!"
  done;
  for i = 0 to 10 do
    List.iter (fun x -> ignore(t_id db x)) (t_get db)
  done

let suite = [
  "stress_bench" >:: test_bench "stress";
  "stress_memory_bench" >:: test_bench ":memory:";
  "stress_cache" >:: test_cache;
]

type t = { a : int; b : int } with persist ()

let time = Unix.gettimeofday
let _ =
  let db = Persist.orm_init_db "stress.db" in
  let t0 = time () in
  for i=0 to 1000 do
    let x = Persist.t { a=Random.int 10; b=i } db in
    ignore x#save;
    if i mod 1000 = 0 then Printf.printf "Saved %i records in %.2fs\n%!" i ((time ()) -. t0);
  done;
  let all = Persist.t_get db in (* get all the elements in the database *)
  Printf.printf "timing (total: %i elements): %!" (List.length all);
  let t1 = time () in
  let l1 = Persist.t_get ~a:(`Eq 5) db in
  let t2 = time () in
  let l2 = Persist.t_get ~fn:(fun t -> t#a = 5) db in
  let t3 = time () in
  Printf.printf "get_where: %.4f (filtered: %i elements); get_custom: %.4f (filtered: %i elements)\n%!"
    (t2 -. t1) (List.length l1) (t3 -. t2) (List.length l2)

(* One result with big tables:

timing (total: 229733 elements):
get_where: 0.1541 (filtered: 23157 elements);
get_custom: 0.6346 (filtered: 23157 elements)

*)

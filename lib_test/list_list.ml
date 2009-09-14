TYPE_CONV_PATH "List_list"

type s = {
  foo: int list list;
  bar: string
} with
persist()

open Orm
let _ = 
  let db = init "list_list.db" in
  let t1 = {foo=[[1]]; bar="t1" } in
  let t2 = {foo=[[2;3;4];[6;7]] ;bar="t2"} in
  let t3 = {foo=[]; bar="t3" } in
  List.iter (fun t -> Printf.eprintf "saved %Lu\n%!" (Orm.s_to_db db t1)) [t1;t2;t3]

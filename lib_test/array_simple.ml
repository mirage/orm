TYPE_CONV_PATH "Array_simple"

type s = {
  foo: int array;
  bar: string
} with
persist()

let _ = 
  let db = Orm.init "array_simple.db" in
  let t1 = { foo=[|1|]; bar="t1" } in
  let t2 = { foo=[|1;2;3|]; bar="t2" } in
  let t3 = { foo=[||]; bar="t3" } in
  List.iter (fun t -> Printf.eprintf "saved: %Lu\n" (Orm.s_to_db db t)) [t1;t2;t3]

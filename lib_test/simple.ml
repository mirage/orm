type t = {
  foo: int;
  bar: string
} with
persist()

let _ = 
  let db = orm_init_db "simple.db" in
  let t1 = t_new ~foo:1 ~bar:"hello world" db in
  let t2 = t_new ~foo:100 ~bar:"world hello" db in
  let id1 = t1#save in
  let id2 = t2#save in
  Printf.printf "saved: %Lu %Lu\n%!" id1 id2


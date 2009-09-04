TYPE_CONV_PATH "List_simple"

type t = {
  foo: int list;
  bar: string
} with
persist()

let _ = 
  let db = Persist.orm_init_db "list_simple.db" in
  let t1 = Persist.t_new ~foo:[1] ~bar:"hello world" db in
  let t2 = Persist.t_new ~foo:[2;3;4] ~bar:"world hello" db in
  let t3 = Persist.t_new ~foo:[] ~bar:"world hello" db in
  Printf.printf "saved: %Lu %Lu\n%!" t1#save t1#save;
  Printf.printf "saved: %Lu %Lu\n%!" t2#save t2#save;
  Printf.printf "saved: %Lu %Lu\n%!" t3#save t3#save

TYPE_CONV_PATH "List_tuple"

type t = {
  foo: (int * char list) list;
  bar: string
} with
persist()

let _ = 
  let db = orm_init_db "list_tuple.db" in
  let t1 = t_new ~foo:[(1,['x';'y'])] ~bar:"hello world" db in
  let t2 = t_new ~foo:[(2,[]); (3,['a';'b';'c']); (4,['a'])] ~bar:"world hello" db in
  let t3 = t_new ~foo:[] ~bar:"world hello" db in
  Printf.printf "saved: %Lu %Lu\n%!" t1#save t1#save;
  Printf.printf "saved: %Lu %Lu\n%!" t2#save t2#save;
  Printf.printf "saved: %Lu %Lu\n%!" t3#save t3#save

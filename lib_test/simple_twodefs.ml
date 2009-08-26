type t = {
  foo: int;
  bar: string
} 
and
x = {
  foo2: char;
  foo3: int32
}
with persist()

let _ = 
  let db = orm_init_db "simple_twodefs.db" in
  let t1 = t_new ~foo:1 ~bar:"hello world" db in
  let t2 = t_new ~foo:100 ~bar:"world hello" db in
  let x1 = x_new ~foo2:'x' ~foo3:100l db in
  let id1 = t1#save in
  let id2 = t2#save in
  let id3 = x1#save in
  Printf.printf "saved: %Lu %Lu %Lu\n%!" id1 id2 id3


TYPE_CONV_PATH "Array_simple"

type t = {
  foo: int array;
  bar: string
} with
persist()

let _ = 
  let db = Orm.init "array_simple.db" in
  let t1 = Orm.t_new ~foo:[|1|] ~bar:"hello world" db in
  let t2 = Orm.t_new ~foo:[|2;3;4|] ~bar:"world hello" db in
  let t3 = Orm.t_new ~foo:[||] ~bar:"world hello" db in
  Printf.printf "saved: %Lu %Lu\n%!" t1#save t1#save;
  Printf.printf "saved: %Lu %Lu\n%!" t2#save t2#save;
  Printf.printf "saved: %Lu %Lu\n%!" t3#save t3#save

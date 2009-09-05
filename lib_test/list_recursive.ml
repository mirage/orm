TYPE_CONV_PATH "List_simple"

type t = {
  foo: int list;
  bar: string
}
and x = {
  one: t list;
} with
persist()

let _ = 
  let db = init "list_simple.db" in
  let t1 = t_new ~foo:[1] ~bar:"hello world" db in
  let t2 = t_new ~foo:[2;3;4] ~bar:"world hello" db in
  let t3 = t_new ~foo:[] ~bar:"world hello" db in
  let x1 = x_new ~one:[ t1; t2 ] db in
  Printf.printf "saved: %Lu %Lu\n%!" t1#save t1#save;
  Printf.printf "saved: %Lu %Lu\n%!" t2#save t2#save;
  Printf.printf "saved: %Lu %Lu\n%!" t3#save t3#save;
  Printf.printf "saved: %Lu %Lu\n%1" x1#save x1#save

TYPE_CONV_PATH "Simple_option"

type t = {
  foo: int option;
  bar: string option
} with
persist()

let _ = 
  let db = Persist.orm_init_db "simple_option.db" in
  let t1 = Persist.t_new ~foo:(Some 1) ~bar:(Some "hello world") db in
  let t2 = Persist.t_new ~foo:(Some 100) ~bar:None db in
  let id1 = t1#save in
  let id2 = t2#save in
  Printf.printf "saved: %Lu %Lu\n%!" id1 id2


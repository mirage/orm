TYPE_CONV_PATH "Simple_option"

type t = {
  foo: int option;
  bar: string option
} with
persist()

let _ = 
  let db = Orm.init "simple_option.db" in
  let t1 = Orm.t_new ~foo:(Some 1) ~bar:(Some "hello world") db in
  let t2 = Orm.t_new ~foo:(Some 100) ~bar:None db in
  let id1 = t1#save in
  let id2 = t2#save in
  Printf.printf "saved: %Lu %Lu\n%!" id1 id2


TYPE_CONV_PATH "Nested_option"

type t = {
  foo: int option option;
  bar: string option option option
} with
persist()

let _ = 
  let db = orm_init_db "nested_option.db" in
  let t1 = t_new ~foo:(Some (Some 1)) ~bar:(Some (Some (Some "hello world"))) db in
  Printf.printf "saved: %Lu %Lu\n%!" t1#save t1#save


TYPE_CONV_PATH "Nested_option"

type x = {
  foo: int option option;
  bar: string option option option
} with
persist()

let _ = 
  let db = Orm.init "nested_option.db" in
  let t1 = { foo=(Some (Some 1)); bar=(Some (Some (Some "hello world"))) } in
  Printf.printf "saved: %Lu %Lu\n%!" (Orm.x_to_db db t1) (Orm.x_to_db db t1)


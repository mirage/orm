TYPE_CONV_PATH "Simple_option"

type x = {
  foo: int option;
  bar: string option
} with
persist()

let _ = 
  let db = Orm.init "simple_option.db" in
  let t1 = { foo=(Some 100); bar = (Some "bar") } in
  let t2 = { foo=None; bar = None } in
  Printf.printf "saved: %Lu %Lu\n%!" (Orm.x_to_db db t1) (Orm.x_to_db db t2);
  Printf.printf "saved: %Lu %Lu\n%!" (Orm.x_to_db db t1) (Orm.x_to_db db t2)


TYPE_CONV_PATH "Simple"
open Printf

type x = {
  foo: int;
  bar: string
} with
persist()

let _ = 
  let db = Orm.init "simple.db" in

  let t1 = { foo=100; bar="world hello"} in
  Orm.x_to_db db t1;
  Orm.x_to_db db t1;

  ()


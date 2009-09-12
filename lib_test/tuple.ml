TYPE_CONV_PATH "Tuple"

type x = {
  foo: int32;
  bar: string * char;
}
with persist()

let _ =
  let db = Orm.init "tuple.db" in
  let t1 = { foo=1000l; bar=("hello",'w') } in
  Printf.printf "saved: %Lu %Lu\n%!" (Orm.x_to_db db t1) (Orm.x_to_db db t1)

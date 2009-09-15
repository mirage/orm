TYPE_CONV_PATH "Nested_tuple"

type s = {
  foo: (int32 * int64 * string * (string * bool option));
  bar: string * char;
}
with persist()

let _ =
  let db = Orm.init "nested_tuple.db" in
  let t1 = {foo=(5l, 10L, "tt", ("xx", Some true)) ; bar=("hello",'w') } in
  Printf.printf "saved: %Lu %Lu\n%!" (Orm.s_to_db db t1) (Orm.s_to_db db t1)

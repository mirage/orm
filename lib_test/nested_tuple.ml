TYPE_CONV_PATH "Nested_tuple"

type t = {
  foo: (int32 * int64 * string * (string * bool option));
  bar: string * char;
}
with persist()

let _ =
  let db = Persist.orm_init_db "nested_tuple.db" in
  let t1 = Persist.t_new ~foo:(5l, 10L, "tt", ("xx", Some true))  ~bar:("hello",'w') db in
  Printf.printf "saved: %Lu %Lu\n%!" t1#save t1#save

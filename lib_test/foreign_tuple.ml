TYPE_CONV_PATH "Foreign_tuple"

type s = {
  foo: string;
  bar: int64;
}
and x = {
  first: (string * int64 * s);
  second: s;
  third: int;
}
with persist ()

let _ =
  let db = Orm.init "foreign_tuple.db" in
  let s1 = { foo="f1"; bar=59L } in
  let x1 = { first=("first",3434L,s1); second=s1; third=99 } in
  Printf.eprintf "saved: %Lu %Lu\n%!" (Orm.x_to_db db x1) (Orm.x_to_db db x1)

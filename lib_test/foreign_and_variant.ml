TYPE_CONV_PATH "Foreign_and_variant"

type v =
|Alice
|Bob of int
|Charlie of (int * int64)
and s = {
  foo: string;
  bar: int64;
  xyz: v;
}
and x = {
  first: s;
  second: s;
  third: int;
}
with persist ()

let _ =
  let db = Orm.init "foreign_and_variant.db" in
  let v = Charlie (1002,1003L) in
  let s1 = { foo="s1"; bar=99L; xyz=v } in
  Orm.s_to_db db s1

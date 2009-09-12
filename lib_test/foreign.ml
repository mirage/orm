TYPE_CONV_PATH "Foreign"

type s = {
  foo: string;
  bar: int64;
  xyz: char;
}
and x = {
  first: s;
  second: s;
  third: int;
}
with persist ()

let _ =
  let db = Orm.init "foreign.db" in
  let s1 = { foo="hello"; bar=100L; xyz='a' } in
  let s2 = { foo="world"; bar=200L; xyz='z' } in
  let x1 = { first=s1; second=s2; third=111 } in
  Orm.x_to_db db x1

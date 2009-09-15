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
  let t1 = {foo="t1" ; bar=100L ; xyz='a' } in
  let t2 = {foo="t2" ; bar=200L ; xyz='z' } in
  let x  = {first=t1 ; second=t2 ; third=6 } in
  prerr_endline (Printf.sprintf "saved: %Lu\n%!" (Orm.x_to_db db x))

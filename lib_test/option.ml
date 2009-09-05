TYPE_CONV_PATH "Option"

type t = {
  a: string;
  b: int list;
  c: string;
}
and s = {
  x: t;
  y: char * string;
  z: int;
  zzz: string option;
}
with persist (
  name: "random";
  unique : a,b;
  unique: b,c
)

let _ =
  let db = Orm.init "xxx.db" in
  let foo = Orm.t { a="foobar"; b=[1;2]; c="wobble" } db in
  Printf.printf "saved: %Lu %Lu\n%!" foo#save foo#save

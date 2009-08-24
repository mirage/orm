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
  let db = orm_init_db "xxx.db" in
  let foo = t_new ~a:"foobar" ~b:[1;2] ~c:"wobble" db in
  foo#save;
  ()

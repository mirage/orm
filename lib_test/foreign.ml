TYPE_CONV_PATH "Foreign"

type t = {
  foo: string;
  bar: int64;
  xyz: char;
}
and x = {
  first: t;
  second: t;
  third: int;
}
with persist ()

let _ =
  let db = Persist.orm_init_db "foreign.db" in
  let t1 = Persist.t_new ~foo:"hello" ~bar:100L ~xyz:'a' db in
  let t2 = Persist.t_new ~foo:"word"  ~bar:200L ~xyz:'z' db in
  let x  = Persist.x_new ~first:t1 ~second:t2 ~third:6 db in
  let id = x#save in
  prerr_endline (Printf.sprintf "saved: %Lu\n%!" id)

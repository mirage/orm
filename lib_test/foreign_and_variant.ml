TYPE_CONV_PATH "Foreign_and_variant"

type v =
|Alice
|Bob of int
|Charlie of (int * int64) option
and t = {
  foo: string;
  bar: int64;
  xyz: v option;
}
and x = {
  first: t;
  second: t;
  third: int;
}
with persist ()

let _ =
  let db = Persist.orm_init_db "foreign.db" in
  let t1 = Persist.t_new ~foo:"hello" ~bar:100L ~xyz:None db in
  let t2 = Persist.t_new ~foo:"word"  ~bar:200L ~xyz:(Some (Bob 1)) db in
  let t3 = Persist.t_new ~foo:"fgfg"  ~bar:300L ~xyz:(Some (Charlie (Some (89,90L)))) db in
  let x1  = Persist.x_new ~first:t1 ~second:t2 ~third:6 db in
  let x2  = Persist.x_new ~first:t1 ~second:t3 ~third:7 db in
  prerr_endline (Printf.sprintf "saved: %Lu %Lu %Lu %Lu\n%!" x1#save x2#save x1#save x2#save)

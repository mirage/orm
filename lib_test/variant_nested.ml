TYPE_CONV_PATH "Variant_nested"

type n =
  |Non of int
  |Ntwo
  |Nthree of char
  |Nfour of x
and x = 
  |Xone
  |Xtwo of n
  |Xthree of int
and
t = {
  foo: x;
  bar: n;
  xyz: int;
}
with persist()

let _ = 
  let db = orm_init_db "variant_nested.db" in
  let t1 = t_new ~foo:Xone ~bar:Ntwo  ~xyz:99 db in
  let t2 = t_new ~foo:(Xtwo (Nfour Xone)) ~bar:(Nthree 'x') ~xyz:66 db in
  let t3 = t_new ~foo:(Xtwo (Nthree 'z')) ~bar:(Non 111) ~xyz:88 db in
  List.iter (fun x -> Printf.printf "saved: %Lu\n%!" x#save) [t1;t2;t3]


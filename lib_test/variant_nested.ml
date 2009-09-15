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
s = {
  foo: x;
  bar: n;
  xyz: int;
}
with persist()

let _ = 
  let db = Orm.init "variant_nested.db" in
  let t1 = {foo=Xone ; bar=Ntwo ; xyz=99 } in
  let t2 = {foo=(Xtwo (Nfour Xone)) ; bar=(Nthree 'x') ;xyz=66 } in
  let t3 = {foo=(Xtwo (Nthree 'z')) ; bar=(Non 111) ; xyz=88 } in
  List.iter (fun x -> Printf.printf "saved: %Lu\n%!" (Orm.s_to_db db x)) [t1;t2;t3]


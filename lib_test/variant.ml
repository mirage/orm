TYPE_CONV_PATH "Variant"

type s = 
  |Foo
  |Bar of int
  |Xyz of string
and
x = {
  foo: s;
  o: int;
}
with persist()

let _ = 
  let db = Orm.init "variant.db" in
  let s1 = Foo in 
  let s2 = Bar (Random.int 100) in
  let s3 = Xyz "hello" in
  let x1 = { foo=s1; o=20 } in
  let x2 = { foo=s2; o=30 } in
  let x3 = { foo=s3; o=40 } in
  List.iter (fun x -> Printf.printf "saved: %Lu %Lu\n%!" 
    (Orm.x_to_db db x) (Orm.x_to_db db x)) [x1;x2;x3]


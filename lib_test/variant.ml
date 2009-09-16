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

open Printf

let string_of_x x =
  sprintf "foo=%s  o=%d" (match x.foo with |Foo->"Foo" |Bar i -> "Bar " ^ (string_of_int i) |Xyz x -> "Xyz " ^ x) x.o

let _ = 
  let db = Orm.init "variant.db" in
  let s1 = Foo in 
  let s2 = Bar (Random.int 100) in
  let s3 = Xyz "hello" in
  let x1 = { foo=s1; o=20 } in
  let x2 = { foo=s2; o=30 } in
  let x3 = { foo=s3; o=40 } in
  List.iter (fun x -> eprintf "saved: %Lu %Lu\n%!" 
    (Orm.x_to_db db x) (Orm.x_to_db db x)) [x1;x2;x3]; 
  List.iter (fun x -> eprintf "Found: %s\n" (string_of_x x)) (Orm.x_of_db ~o:(`Between (0,100)) db)
  


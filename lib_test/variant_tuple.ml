TYPE_CONV_PATH "Variant"

type s = 
  |Foo
  |Bar of (int * x) list
  |Xyz of string

and
x = {
  first : s;
  o: int list;
}
with orm()

let _ = 
  let db = Orm.init "variant_tuple.db" in
  let rec x1 = {first=Foo ; o=[100] }
  and x2 = {first=Bar [(1,x1); (2,x3)] ; o=[101;102]}
  and x3 = {first=(Xyz "hello") ; o=[] } in
  List.iter (fun x -> Printf.printf "saved: %Lu\n%!" (Orm.x_to_db db x)) [x1;x2;x3]


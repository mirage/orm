TYPE_CONV_PATH "Variant"

type t = 
  |Foo
  |Bar of (int * x) list
  |Xyz of string

and
x = {
  first : t;
  o: int list;
}
with persist()

let _ = 
  let db = Orm.init "variant.db" in
  let x1 = Orm.x_new ~first:Foo ~o:100 db in
  let x2 = Orm.x_new ~first:(Bar (1,2)) ~o:101 db in
  let x3 = Orm.x_new ~first:(Xyz "hello") ~o:102 db in
  List.iter (fun x -> Printf.printf "saved: %Lu\n%!" x#save) [x1;x2;x3]


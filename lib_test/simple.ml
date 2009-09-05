TYPE_CONV_PATH "Simple"
open Printf

type t = {
  foo: int;
  bar: string
} with
persist()

let _ = 
  let db = Orm.init "simple.db" in

  let t1 = Orm.t_new ~foo:1 ~bar:"hello world" db in
  let t2 = Orm.t { foo=100; bar="world hello"} db in

  printf "saved: %Lu %Lu\n%!" t1#save t2#save;

  let found t = printf "found <%i, %s>\n%!" t#foo t#bar in

  List.iter found (Orm.t_get ~fn:(fun t -> t#foo = 100) db);

  List.iter found (Orm.t_get ~foo:(`Between (0,1000)) db)


TYPE_CONV_PATH "List_foreign"

type s = {
  foo: int;
  bar: string
} 
and x = {
  one: s list;
}
with
persist()

let _ = 
  let db = Orm.init "list_foreign.db" in
  let t1 = {foo=1; bar="t1"} in
  let t2 = {foo=2; bar="t2"} in
  let x1 = {one=[t1;t2;t1] } in
  Orm.x_to_db db x1;
  Orm.x_to_db db x1

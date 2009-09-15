TYPE_CONV_PATH "Simple_twodefs"

type s = {
  foo: int;
  bar: string
} 
and
x = {
  foo2: char;
  foo3: int32
}
with persist()

let _ = 
  let db = Orm.init "simple_twodefs.db" in
  let t1 = {foo=1 ;bar="hello world" } in
  let t2 = {foo=100 ;bar="world hello" } in
  let x1 = {foo2='x' ;foo3=100l } in
  let ts t = ignore(Orm.s_to_db db t) in
  ts t1; ts t1;
  ts t2; ts t2;
  ignore(Orm.x_to_db db x1)


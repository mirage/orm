TYPE_CONV_PATH "Recursive"

type s = { 
  t1: string;
  t2: x
}
and x = {
  x1: s;
  x2: char
}
with persist ()

open Orm
let _ = 
  let db = init "recursive.db" in
  let rec vt = { t1= "hello"; t2=vx }
  and vx = { x1=vt; x2='z' } in
  s_to_db db vt 

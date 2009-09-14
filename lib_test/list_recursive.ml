TYPE_CONV_PATH "List_recursive"

type s = { 
  t1: string;
  t2: x list
}
and x = {
  x1: s list;
  x2: char
}
with persist ()

open Orm
let _ = 
  let db = init "list_recursive.db" in
  let rec vt1 = { t1="vt1"; t2=[ vx; vx; vx] }
  and vt2 = { t1= "vt2"; t2=[ vx ] }
  and vx = { x1=[ vt1; vt2] ; x2='z' } in
  s_to_db db vt1

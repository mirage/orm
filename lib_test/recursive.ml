TYPE_CONV_PATH "Recursive"

type s = { 
  t1: string;
  t2: x option
}
and x = {
  x1: s list;
  x2: char
}
with persist ()

open Orm
let _ = 
  let db = init "recursive.db" in
  let rec vt = { t1= "hello"; t2=(Some vx) }
  and vx = { x1=[vt;vt]; x2='z' } in
  ignore(s_to_db db vt );
  Orm.s_of_db db

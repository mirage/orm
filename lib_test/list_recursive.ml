TYPE_CONV_PATH "List_recursive"

type s = { 
  t1: string;
  t2: x list
}
and x = {
  x1: s list;
  x2: char
}
with orm ()

open Printf
let ps s = eprintf "[t1=%s t2=[%s]]\n" 
  s.t1 (String.concat "," (List.map (fun x -> sprintf "%c" x.x2) s.t2))

open Orm
let _ = 
  let db = init "list_recursive.db" in
  let rec vt1 = { t1="vt1"; t2=[ vx; vx; vx] }
  and vt2 = { t1= "vt2"; t2=[ vx ] }
  and vx = { x1=[ vt1; vt2] ; x2='z' } in
  let _ = s_to_db db vt1 in
  let _ = s_to_db db vt1 in
  List.iter ps (Orm.s_of_db db)

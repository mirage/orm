TYPE_CONV_PATH "Recursive"

type s = { 
  t1: string;
  t2: x option
}
and x = {
  x1: s option;
  x2: char
}
with persist ()

open Orm
open Printf

let ps s = eprintf "{ t1=%s t2=%c }\n" s.t1 (match s.t2 with |None -> '?' |Some x -> x.x2)

let _ = 
  let db = init "recursive.db" in
  let rec vt = { t1= "hello"; t2=(Some vx) }
  and vx = { x1=(Some vt); x2='z' } in
  ignore(s_to_db db vt );
  List.iter ps (Orm.s_of_db db)

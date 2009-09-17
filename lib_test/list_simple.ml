TYPE_CONV_PATH "List_simple"

type s = {
  foo: string list;
  bar: (char * string) list;
  pla: bool;
} with
persist()

open Printf
let strfn n s = eprintf "%s : foo=[%s] bar=[%s] pla=%b\n" n (String.concat "," s.foo)
   (String.concat "," (List.map (fun (x,y) -> sprintf "%c * %s" x y) s.bar)) s.pla

let _ = 
  let db = Orm.init "list_simple.db" in
  let t1 = { foo=["a1";"a2";"a3";"a4"]; bar=[ ('a',"AA"); ('b',"BB"); ('c',"CC") ] ; pla=true } in
  strfn "t1" t1;
  ignore(Orm.s_to_db db t1);
  ignore(Orm.s_to_db db t1);
  List.iter (strfn "find") (Orm.s_of_db db)
 
  

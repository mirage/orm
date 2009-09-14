TYPE_CONV_PATH "List_simple"

type s = {
  foo: string list;
  bar: (char * string) list;
  pla: bool;
} with
persist()

let _ = 
  let db = Orm.init "list_simple.db" in
  let t1 = { foo=["a1";"a2";"a3";"a4"]; bar=[ ('a',"AA"); ('b',"BB"); ('c',"CC") ] ; pla=true } in
  Orm.s_to_db db t1

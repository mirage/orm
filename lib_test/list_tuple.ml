TYPE_CONV_PATH "List_tuple"

type s = {
  foo: (int * char list) list;
  bar: string
} with
persist()

let _ = 
  let db = Orm.init "list_tuple.db" in
  let t1 = {foo=[(1,['x';'y'])] ; bar="hello world" } in
  let t2 = {foo=[(2,[]); (3,['a';'b';'c']); (4,['a'])] ; bar="world hello" } in
  let t3 = {foo=[] ; bar="world hello" } in
  List.iter (fun x -> Printf.eprintf "saved: %Lu\n" (Orm.s_to_db db x)) [t1;t2;t3]

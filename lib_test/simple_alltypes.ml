TYPE_CONV_PATH "Simple_alltypes"

type v =
 |One of string
 |Two of (char * v)
 |Three of v
 |Four of int32
 |Five of float
and x = {
  foo: v option;
  bar: x list;
} with persist()

let _ = 
  let db = Orm.init "simple_alltypes.db" in
  let v1 = One "hello" in
  let v2 = Two ('z',v1) in
  let v3 = Three v1 in
  let rec t1 = { foo=(Some v1); bar=[t2] }
  and t2 = { foo=(Some v3); bar=[t1] } in
  Printf.printf "saved: %Lu %Lu\n%!" (Orm.x_to_db db t1) (Orm.x_to_db db t1);
  Printf.printf "saved: %Lu %Lu\n%!" (Orm.x_to_db db t2) (Orm.x_to_db db t2)


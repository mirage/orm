TYPE_CONV_PATH "Get_set"
 
type t = {
  mutable x : string;
  mutable y : string;
} with orm()

let get db x =
  t_get db ~x:x

let set db x y =
  let t = List.hd (get db x) in
  t.y <- y;
  t_save db t

(*
 * The following should fail to compile 
let fail () =
  let t = { x="hello"; y="world" } in
  let db = t_init_read_only "foo" in
  t_save db t
*)

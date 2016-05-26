(*pp camlp4orf *)

type t = {
  mutable x : string;
  mutable y : string;
  z : bool;
} with orm()

let get db x =
  t_get db ~x:x

let get_true db =
  t_get ~z:(`T) db

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

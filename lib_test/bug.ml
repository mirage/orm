TYPE_CONV_PATH "bug"

type x = { foo : float } with orm ()
type t = { x: x } with orm ()

(*
make -f Makefile.debug p_bug
camlp4o -I/Users/user/thomasga/godi/lib/ocaml/pkg-lib/type-conv -I/Users/user/thomasga/godi/lib/ocaml/site-lib/sqlite3 -I ../lib pa_type_conv.cmo -parser ../lib/pa_orm.cma -printer o bug.ml
File "bug.ml", line 4, characters 23-26:
Failure: "Pa_type_conv: 'orm' is not a supported type generator."
make[1]: *** [p_bug] Error 2
make: *** [p_bug] Error 2
*)

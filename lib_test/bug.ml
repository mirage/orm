(*pp camlp4o -I ../lib -I `ocamlfind query type-conv` pa_type_conv.cmo pa_orm.cma *)

type t = { int: x } with orm ()

(*
make -f Makefile.debug p_bug
camlp4orf -I/Users/user/thomasga/godi/lib/ocaml/pkg-lib/type-conv -I/Users/user/thomasga/godi/lib/ocaml/site-lib/sqlite3 -I ../lib pa_type_conv.cmo -parser ../lib/pa_orm.cma -printer o bug.ml
File "bug.ml", line 3, characters 25-28:
Failure: "Pa_type_conv: 'orm' is not a supported type generator."
make[1]: *** [p_bug] Error 2
make: *** [p_bug] Error 2
*)

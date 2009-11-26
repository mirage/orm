(*pp camlp4o -I .. -I `ocamlfind query type-conv` pa_type_conv.cmo pa_type.cma *)

TYPE_CONV_PATH "All"
open Printf

type i1 = int32
and  i2 = int
and  i3 = int64
and  i4 = ( int32 * int * int64 )
and  p =
  | One of string * int array
  | Two of t
  | Three of x option list

and pp = [ `Poly1 | `Poly2 | `Poly3 of int ]

and t = {
  t1: int;
  mutable t2: string;
  t3: x
} and x = {
  x1: t array;
  x2: int64
} and f = {
  mutable f1: int;
  mutable f2: string list;
  f3: string;
  f4: int64;
  f5: char array;
} and tu = ( int  * f * pp )

with type_of

type o =
  < x: f; y: x; z: (int -> string) > 
  with type_of

open OUnit

let (<=>) n t =
  let ts = Type.to_string t in
  printf "%s: %s\n%!" n ts;
  printf "%s: %s\n%!" n (Type.to_string (Type.of_string ts));
  ("EQ " ^ n) @? ( t = Type.of_string ts)

let test_marshall () =
  "i1" <=> type_of_i1;
  "i2" <=> type_of_i2;
  "i3" <=> type_of_i3;
  "i4" <=> type_of_i4;
  "p"  <=> type_of_p;
  "pp" <=> type_of_pp;
  "t"  <=> type_of_t;
  "x"  <=> type_of_x;
  "f"  <=> type_of_f;
  "tu" <=> type_of_tu;
  "o"  <=> type_of_o

let suite = [
  "all_f_marshall" >::  test_marshall
]

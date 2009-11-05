(*pp camlp4o -I .. -I `ocamlfind query type-conv` pa_type_conv.cmo pa_type.cma *)

TYPE_CONV_PATH "All"

type p =
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
  < x: x; y: string; z: (int -> string) > 
  with type_of

open OUnit

let rs () =
	Random.self_init ();
	let len = Random.int 30 in
	let s = String.create len in
	for i = 0 to len - 1 do
		String.set s i (Char.chr (Random.int 25 + 97))
	done;
	s

let fg () =
	{ f1=(Random.int 100000); f2=[rs(); rs(); rs()]; f3=rs(); f4=(Random.int64 1000000L); f5=[|'a';'b'|] }

let test_marshall () =
	"EQ p" @? (type_of_p = Type.of_string (Type.to_string type_of_p));
	"EQ pp" @? (type_of_pp = Type.of_string (Type.to_string type_of_pp));
	"EQ t" @? (type_of_t = Type.of_string (Type.to_string type_of_t));
	"EQ x" @? (type_of_x = Type.of_string (Type.to_string type_of_x));
	"EQ f" @? (type_of_f = Type.of_string (Type.to_string type_of_f));
	"EQ tu" @? (type_of_tu = Type.of_string (Type.to_string type_of_tu));
	"EQ o" @? (type_of_o = Type.of_string (Type.to_string type_of_o))

let suite = [
	"all_f_marshall" >::  test_marshall
]

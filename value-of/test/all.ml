(*pp camlp4o -I .. -I `ocamlfind query type-conv` pa_type_conv.cmo pa_value.cma *)

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

and o = < x: int; y: string; z: (int -> string) > 
with value

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
	for i = 1 to 10000 do
		let f = fg () in
		"unmarshall a marshalled value is ident" @? (f_of_value (value_of_f f) = f)
	done

let suite = [
	"all_f_marshall" >::  test_replace
]

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

with weakid

type o =
  < x: f; y: x; z: (int -> string) > 
  with weakid

open OUnit

let test_syntax () = ()

let suite = [
	"all_f_syntax" >::  test_syntax
]

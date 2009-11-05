(*pp camlp4o -I .. -I ../../hash -I ../../weakid -I `ocamlfind query type-conv` pa_type_conv.cmo pa_hash.cma pa_weakid.cma pa_value.cma *)

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
with value

type o = < x: f; y: string; z: (int -> string) > 
with value

open OUnit

let _ = Random.self_init ()

let char () = Char.chr (Random.int 25 + 97)
let int () = Random.int max_int
let int64 () = Random.int64 Int64.max_int
let option v = if Random.int 4 = 0 then None else Some (v ())
			
let string () =
	let len = Random.int 30 in
	let s = String.create len in
	for i = 0 to len - 1 do
		String.set s i (char ())
	done;
	s

let array v =
	let len = Random.int 30 in
	let s = String.create len in
	for i = 0 to len - 1 do
		Array.set s i (v ())
	done;
	s

let list v = Array.to_list (array v)

let rec p () =
	match Random.int 3 with
	| 0 -> One (string (), array int)
	| 1 -> Two (t ())
	| 2 -> Three (list (fun () -> option (x ())))
	| _ -> assert false

and pp () =
	match Random.int 3 with
	| 0 -> `Poly
	| 1 -> `Poly2
	| 3 -> `Poly3 (int ())

and t () = { t1 = int (); t2 = string (); t3 = x () }

and f () = { f1 = int (); f2 = list string; f3 = string; f4 = int64 (); f5 = array char }

and tu () = ( int (), f (), pp () )

let o () = object method x = f () method y = string () method z = (fun i -> string () ^ string_of_int i) end

let test_marshall () =
	for i = 1 to 10000 do begin
		let p = p () and pp = pp () and t = t () and x = x () and f = f () and tu = tu () and o = o () in
		"EQ p" @? (p = p_of_value (value_of_p p));
		"EQ pp" @? (pp = pp_of_value (value_of_pp pp));
		"EQ t" @? (t = t_of_value (value_of_t t));
		"EQ x" @? (x = x_of_value (value_of_x x));
		"EQ f" @? (f = f_of_value (value_of_f f));
		"EQ tu" @? (tu = tu_of_value (value_of_tu tu));
		"EQ o" @? (o = o_of_value (value_of_o o));
	end done

let suite = [
	"all_f_marshall" >::  test_replace
]

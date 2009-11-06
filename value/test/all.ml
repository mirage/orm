(*pp camlp4o -I .. -I ../../hash -I ../../weakid -I `ocamlfind query type-conv` pa_type_conv.cmo pa_hash.cma pa_weakid.cma pa_value.cma *)

TYPE_CONV_PATH "All"

type p =
	| One of string * int array * char * bool * (float list)
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
let float () = Random.float 1000000.
let bool () = Random.int 1 = 0

let string () =
	let len = Random.int 30 in
	let s = String.create len in
	for i = 0 to len - 1 do
		String.set s i (char ())
	done;
	s

let array v =
	let len = Random.int 30 in
	let s = Array.create len (v()) in
	for i = 0 to len - 1 do
		s.(i) <- v ()
	done;
	s

let list v = Array.to_list (array v)

let p t x =
	match Random.int 3 with
	| 0 -> One (string (), array int, char (), bool (), list float)
	| 1 -> Two t
	| 2 -> Three (list (fun () -> option (fun () -> x)))
	| _ -> assert false

let pp () =
	match Random.int 3 with
	| 0 -> `Poly1
	| 1 -> `Poly2
	| 3 -> `Poly3 (int ())
	| _ -> assert false

let t x = { t1 = int (); t2 = string (); t3 = x }

let x t = { x1 = array (fun () -> t); x2 = int64 () }
		
let f () = { f1 = int (); f2 = list string; f3 = string (); f4 = int64 (); f5 = array char }
		
let tu f pp  = ( int (), f, pp)

let o () : o = object method x = f () method y = string () method z = (fun i -> string () ^ string_of_int i) end

let rec x1 = { x1 = [| t1; t2 |]; x2 = int64 () }
and t1 = { t1 = int (); t2 = string (); t3 = x1 }
and t2 = { t1 = int (); t2 = string (); t3 = x1 }

let test_marshall () =
	for i = 1 to 2 do begin
		let f = f () in
		let o = o () in

		Printf.printf "x1=%s\n%!" (Value.to_string (value_of_x x1));
		Printf.printf "t2=%s\n%!" (Value.to_string (value_of_t t1));
		Printf.printf "t2=%s\n%!" (Value.to_string (value_of_t t2));
		let _ = t_of_value (value_of_t t1) in
		"EQ f" @? (f = f_of_value (value_of_f f));
(*		"EQ t" @? (value_of_t t1 = value_of_t (t_of_value (value_of_t t1)));
		"EQ t" @? (value_of_t t2 = value_of_t (t_of_value (value_of_t t2)));
		"EQ t" @? (value_of_x x1 = value_of_x (x_of_value (value_of_x x1)));*)
(*
		"EQ p" @? (p = p_of_value (value_of_p p));
		"EQ pp" @? (pp = pp_of_value (value_of_pp pp));

		"EQ tu" @? (tu = tu_of_value (value_of_tu tu));
*)

	end done

let suite = [
	"all_f_marshall" >::  test_marshall
]

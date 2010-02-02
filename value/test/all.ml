(*pp camlp4o -I .. -I `ocamlfind query type-conv` pa_type_conv.cmo pa_value.cma *)

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

let rec x1 = { x1 = [| t1; t2 |]; x2 = int64 () }
and t1 = { t1 = int (); t2 = string (); t3 = x1 }
and t2 = { t1 = int (); t2 = string (); t3 = x1 }

let rec p () =
	match Random.int 3 with
	| 0 -> One (string (), array int, char (), bool (), list float)
	| 1 -> Two (t ())
	| 2 -> Three (list (fun () -> option x))
	| _ -> assert false

and pp () =
	match Random.int 3 with
	| 0 -> `Poly1
	| 1 -> `Poly2
	| 2 -> `Poly3 (int ())
	| _ -> assert false

and t () = if Random.int 10 > 1 then t1 else { t1 = int (); t2 = string (); t3 = x () }

and x () = if Random.int 10 > 1 then x1 else { x1 = array t; x2 = int64 () }
		
and f () = { f1 = int (); f2 = list string; f3 = string (); f4 = int64 (); f5 = array char }
		
and tu ()  = ( int (), f (), pp ())

let o () : o = object method x = f () method y = string () method z = (fun i -> string () ^ string_of_int i) end

let test_marshall () =
	for i = 1 to 200 do begin
		let p = p () in
		let pp = pp () in
		let t = t () in
		let x = x () in
		let f = f () in
		let tu = tu () in
		let o = o () in
(*                Printf.printf "p=%s\n%!" (Value.to_string (value_of_p p));
                Printf.printf "pp=%s\n%!" (Value.to_string (value_of_pp pp));
                Printf.printf "t=%s\n%!" (Value.to_string (value_of_t t));
                Printf.printf "x=%s\n%!" (Value.to_string (value_of_x x));
                Printf.printf "f=%s\n%!" (Value.to_string (value_of_f f));
                Printf.printf "tu=%s\n%!" (Value.to_string (value_of_tu tu));
                Printf.printf "o=%s\n%!" (Value.to_string (value_of_o o)); *)
		"EQ p" @? (value_of_p p = value_of_p (p_of_value (value_of_p p)));
		"EQ pp" @? (value_of_pp pp = value_of_pp (pp_of_value (value_of_pp pp)));
		"EQ t" @? (value_of_t t = value_of_t (t_of_value (value_of_t t)));
		"EQ x" @? (value_of_x x = value_of_x (x_of_value (value_of_x x))); 
		"EQ f" @? (value_of_f f = value_of_f (f_of_value (value_of_f f)));
		"EQ tu" @? (value_of_tu tu = value_of_tu (tu_of_value (value_of_tu tu)));
		"EQ o" @? (ignore( value_of_o (o_of_value (value_of_o o))); true); (* Marshaled functions cannot be compared *)
	end done

let suite = [
	"all_f_marshall" >::  test_marshall
]

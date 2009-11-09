(*pp camlp4o -I .. -I `ocamlfind query type-conv` pa_type_conv.cmo pa_hash.cma *)

TYPE_CONV_PATH "All"

type p =
 |One of int array * string * float * bool * (char list)
 |Two
 |Three of x option list

and pp = [ `Poly1 | `Poly2 | `Poly3 of int ]

and t = {
  t1: int;
  mutable t2: string;
  t3: x
}

and x = {
  x1: t array;
  x2: int64
}

and
f = {
  mutable f1: int;
  mutable f2: string list;
  f3: string;
  f4: int64;
  f5: char array;
}

and tu = ( int  * f * pp ) 

with hash

type o = < x: int; y: string; z: (int -> string) > with hash

module FH = Hashtbl.Make(struct
  let hash = hash_of_f
  let equal = (=)
  let compare = compare
  type t = f
end)

let rs () =
  Random.self_init ();
  let len = Random.int 30 in
  let s = String.create len in
  for i = 0 to len - 1 do
    String.set s i (Char.chr (Random.int 25 + 97))
  done;
  s

open OUnit

let test_replace () =
  let h = FH.create 1 in
  let fg () = { f1=(Random.int 100000); f2=[rs(); rs(); rs()]; f3=rs(); f4=(Random.int64 1000000L); f5=[|'a';'b'|] } in
  for i = 1 to 10000 do
    let f = fg () in
    FH.add h f ();
    "hash len ok" @? (FH.length h = i);
    FH.replace h f ();
    "hash len ok after replace" @? (FH.length h = i);
    f.f1 <- Random.int 100000;
    f.f2 <- [ rs (); rs (); rs ()];
    FH.replace h f ();
    "hash len ok after replace mutate" @? (FH.length h = i);
  done

let suite = [
  "all_f_replace" >::  test_replace
]

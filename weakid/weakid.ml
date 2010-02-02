(***********************************************************************)
(*                                                                     *)
(*                           HWeak                                     *)
(*                                                                     *)
(*                        Remi Vanicat                                 *)
(*                                                                     *)
(*  Copyright 2002 Rémi Vanicat                                        *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Library General Public License, with the special exception *)
(*  on linking described in the LICENCE file of the Objective Caml     *)
(*  distribution                                                       *)
(*                                                                     *)
(*  Most of this file is an adptation of the implentation of Weak      *)
(*  Hastable by Damien Doligez that can be found into Objective Caml   *)
(*  which is Copyright 1997 Institut National de Recherche en          *)
(*  Informatique et en Automatique and is distributed under the same   *)
(*  licence                                                            *)
(*                                                                     *)
(***********************************************************************)
(* 10/2009: modified by Thomas Gazagnaire <thomas@gazganaire.com>      *)

(** Weak array operations *)

(** Weak hash tables *)

module type S = sig
	type key
	type 'a t
	val create : int -> 'a t
	val clear : 'a t -> unit
	val add : 'a t -> key -> 'a -> unit
	val replace : 'a t -> key -> 'a -> unit
	val remove : 'a t -> key -> unit
	val merge : 'a t -> key -> 'a -> 'a
	val find : 'a t -> key -> 'a
	val find_all : 'a t -> key -> 'a list
	val mem : 'a t -> key -> bool
	val iter : (key -> 'a -> unit) -> 'a t -> unit
	val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	val count : 'a t -> int
	val stats : 'a t -> int * int * int * int * int * int
end

module type Arg = sig
	type 'a t
	val get : 'a t -> int -> 'a option
	val set : 'a t -> int -> 'a option -> unit
	val empty : unit -> 'a t
	val length : 'a t -> int
	val create : int -> 'a t
	val blit : 'a t -> int -> 'a t -> int -> int -> unit
end

module MakeOne (K : Arg) (V : Arg) (H : Hashtbl.HashedType) : (S with type key = H.t) = struct

	type key = H.t

	let check_key b i = K.get b i <> None
	let check_val b i = V.get b i <> None

	type 'a t = {
		mutable table : (key K.t * 'a V.t) array;
		mutable totsize : int;             (* sum of the bucket sizes *)
		mutable limit : int;               (* max ratio totsize/table length *)
	}

	let get_index t d = (H.hash d land max_int) mod (Array.length t.table);;

	let create sz =
		let sz = if sz < 7 then 7 else sz in
		let sz = if sz > Sys.max_array_length then Sys.max_array_length else sz in
		{
			table = Array.create sz (K.empty (), V.empty ());
			totsize = 0;
			limit = 3;
		}

	let clear t =
		for i = 0 to Array.length t.table - 1 do
			t.table.(i) <- (K.empty (), V.empty ());
		done;
		t.totsize <- 0;
		t.limit <- 3


	let fold f t init =
		let rec fold_bucket i ((b1, b2) as cpl) accu =
			if i >= K.length b1 then accu else
				match (K.get b1 i, V.get b2 i) with
				| (Some v1, Some v2) -> fold_bucket (i+1) cpl (f v1 v2 accu)
				| _ -> fold_bucket (i+1) cpl accu
		in
		Array.fold_right (fold_bucket 0) t.table init

	let iter f t = fold (fun d1 d2 () -> f d1 d2) t ();;

	let count t =
		let rec count_bucket i ((b1, b2) as cpl) accu =
			if i >= K.length b1 then accu else
				count_bucket (i+1) cpl (accu + (if check_key b1 i && check_val b2 i 
												then 1 else 0))
		in
		Array.fold_right (count_bucket 0) t.table 0

	let next_sz n = min (3*n/2 + 3) (Sys.max_array_length - 1);;

	let rec resize t =
		let oldlen = Array.length t.table in
		let newlen = next_sz oldlen in
		if newlen > oldlen then begin
			let newt = create newlen in
			newt.limit <- t.limit + 100;          (* prevent resizing of newt *)
			fold (fun f t () -> add newt f t) t ();
			(* assert Array.length newt.table = newlen; *)
			t.table <- newt.table;
			t.limit <- t.limit + 2;
		end

	and add_aux t k e index =
		let bucket1, bucket2 = t.table.(index) in
		let sz = K.length bucket1 in
		let rec loop i =
			if i >= sz then begin
				let newsz = min (sz + 3) (Sys.max_array_length - 1) in
				if newsz <= sz then failwith "Weak.Make : hash bucket cannot grow more";
				let newbucket1 = K.create newsz 
				and newbucket2 = V.create newsz in
				K.blit bucket1 0 newbucket1 0 sz;
				V.blit bucket2 0 newbucket2 0 sz;
				K.set newbucket1 i (Some k);
				V.set newbucket2 i (Some e);
				t.table.(index) <- (newbucket1, newbucket2);
				t.totsize <- t.totsize + (newsz - sz);
				if t.totsize > t.limit * Array.length t.table then resize t;
			end else begin
				if check_key bucket1 i && check_val bucket2 i
				then loop (i+1)
				else begin
					K.set bucket1 i (Some k);
					V.set bucket2 i (Some e);
				end
			end
		in
		loop 0

	and add t k e = add_aux t k e (get_index t k)
 

	let find_or t d ifnotfound =
		let index = get_index t d in
		let (bucket1, bucket2) = t.table.(index) in
		let sz = K.length bucket1 in
		let rec loop i =
			if i >= sz then ifnotfound index
			else begin
				match K.get bucket1 i with
					| Some v when H.equal v d ->
						begin match V.get bucket2 i with
							| Some v -> v
							| None -> loop (i+1)
						end
					| _ -> loop (i+1)
			end
		in
		loop 0

	let merge t k d = find_or t k (fun index -> add_aux t k d index; d);;

	let find t d = find_or t d (fun index -> raise Not_found);;

	let find_shadow t d iffound ifnotfound =
		let index = get_index t d in
		let (bucket1, bucket2) = t.table.(index) in
		let sz = K.length bucket1 in
		let rec loop i =
			if i >= sz then ifnotfound else begin
				match K.get bucket1 i with
				| Some v when H.equal v d && check_val bucket2 i ->
					iffound bucket1 bucket2 i
				| _ -> loop (i+1)
			end
		in
		loop 0

	let replace t k d = 
		if (find_shadow t k 
				(fun w1 w2 i -> 
					K.set w1 i (Some k); V.set w2 i (Some d); false ) true) 
		then
			add t k d

	let remove t d = find_shadow t d
		(fun w1 w2 i -> K.set w1 i None; V.set w2 i None) ()

	let mem t d = find_shadow t d (fun _ _ i -> true) false

	let find_all t d =
		let index = get_index t d in
		let (bucket1, bucket2) = t.table.(index) in
		let sz = K.length bucket1 in
		let rec loop i accu =
			if i >= sz then accu
			else begin
				match K.get bucket1 i with
				| Some v when H.equal v d ->
					begin match V.get bucket2 i with
						| Some v -> loop (i+1) (v::accu)
						| None -> loop (i+1) accu
					end
				| _ -> loop (i+1) accu
			end
		in
		loop 0 []

	let stats t =
		let len = Array.length t.table in
		let lens = Array.map (fun (b,_) -> K.length b) t.table in
		Array.sort compare lens;
		let totlen = Array.fold_left ( + ) 0 lens in
		(len, count t, totlen, lens.(0), lens.(len/2), lens.(len-1))

end

module W = struct
	include Weak
	let empty () = create 0
end

module A = struct
	include Array
	type 'a t = 'a option array
	let empty () = [| |]
	let create n = create n None
end

module MakeWeakKeys = MakeOne(W)(A)
module MakeWeakValues = MakeOne(A)(W)

module Make (H : Hashtbl.HashedType) = struct
	module K = MakeWeakKeys(H)
	module V = MakeWeakValues(struct type t = int64 let equal = (=) let hash = Hashtbl.hash end)

	type t = {
		id_elt : H.t V.t;
		elt_id : int64 K.t;
		mutable count : int64;
	}
	let create i = {
		id_elt = V.create i;
		elt_id = K.create i;
		count = 0L;
	}
		
	let to_weakid t (elt : H.t) : int64 =
		K.find t.elt_id elt

	let of_weakid t (id : int64) : H.t =
		V.find t.id_elt id

	let mem t elt =
		K.mem t.elt_id elt

	let mem_weakid t id =
		V.mem t.id_elt id

	let add t elt =
		let rec fresh () =
			t.count <- Int64.add t.count 1L;
			if V.mem t.id_elt t.count then
				fresh ()
			else
				t.count in
		let id = fresh () in
		V.replace t.id_elt id elt;
		K.replace t.elt_id elt id;
		id

	let remove t elt =
		if K.mem t.elt_id elt then begin
			let id = K.find t.elt_id elt in
			V.remove t.id_elt id;
			K.remove t.elt_id elt
		end

	let replace t elt id =
		V.replace t.id_elt id elt;
		K.replace t.elt_id elt id
end

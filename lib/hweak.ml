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


(** Weak array operations *)

open Weak

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
end;;

module Make (H : Hashtbl.HashedType) : (S with type key = H.t) = struct

  type 'a weak_t = 'a t;;
  let weak_create = create;;
  let emptybucket = weak_create 0;;

  type key = H.t;;

  type 'a t = {
    emptybucket : 'a weak_t;
    mutable table : (key weak_t * 'a weak_t) array;
    mutable totsize : int;             (* sum of the bucket sizes *)
    mutable limit : int;               (* max ratio totsize/table length *)
  };;

  let get_index t d = (H.hash d land max_int) mod (Array.length t.table);;

  let create sz =
    let sz = if sz < 7 then 7 else sz in
    let sz = if sz > Sys.max_array_length then Sys.max_array_length else sz in
    let em = weak_create 0 in
    {
      emptybucket = em; 
      table = Array.create sz (emptybucket, em);
      totsize = 0;
      limit = 3;
    };;

  let clear t =
    for i = 0 to Array.length t.table - 1 do
      t.table.(i) <- (emptybucket, t.emptybucket);
    done;
    t.totsize <- 0;
    t.limit <- 3;
  ;;

  let fold f t init =
    let rec fold_bucket i ((b1, b2) as cpl) accu =
      if i >= length b1 then accu else
      match (get b1 i, get b2 i) with
      | (Some v1, Some v2) -> fold_bucket (i+1) cpl (f v1 v2 accu)
      | _ -> fold_bucket (i+1) cpl accu
    in
    Array.fold_right (fold_bucket 0) t.table init
  ;;

  let iter f t = fold (fun d1 d2 () -> f d1 d2) t ();;

  let count t =
    let rec count_bucket i ((b1, b2) as cpl) accu =
      if i >= length b1 then accu else
      count_bucket (i+1) cpl (accu + (if check b1 i && check b2 i 
				      then 1 else 0))
    in
    Array.fold_right (count_bucket 0) t.table 0
  ;;

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
    let sz = length bucket1 in
    let rec loop i =
      if i >= sz then begin
        let newsz = min (sz + 3) (Sys.max_array_length - 1) in
        if newsz <= sz then failwith "Weak.Make : hash bucket cannot grow more";
        let newbucket1 = weak_create newsz 
	and newbucket2 = weak_create newsz in
        blit bucket1 0 newbucket1 0 sz;
        blit bucket2 0 newbucket2 0 sz;
        set newbucket1 i (Some k);
	set newbucket2 i (Some e);
        t.table.(index) <- (newbucket1, newbucket2);
        t.totsize <- t.totsize + (newsz - sz);
        if t.totsize > t.limit * Array.length t.table then resize t;
      end else begin
        if check bucket1 i && check bucket2 i
        then loop (i+1)
        else 
	  begin
	    set bucket1 i (Some k);
	    set bucket2 i (Some e);
	  end
      end
    in
    loop 0;

  and add t k e = add_aux t k e (get_index t k)
  ;;

  let find_or t d ifnotfound =
    let index = get_index t d in
    let (bucket1, bucket2) = t.table.(index) in
    let sz = length bucket1 in
    let rec loop i =
      if i >= sz then ifnotfound index
      else begin
        match get_copy bucket1 i with
        | Some v when H.equal v d
           -> begin match get bucket2 i with
              | Some v -> v
              | None -> loop (i+1)
              end
        | _ -> loop (i+1)
      end
    in
    loop 0
  ;;

  let merge t k d = find_or t k (fun index -> add_aux t k d index; d);;

  let find t d = find_or t d (fun index -> raise Not_found);;

  let find_shadow t d iffound ifnotfound =
    let index = get_index t d in
    let (bucket1, bucket2) = t.table.(index) in
    let sz = length bucket1 in
    let rec loop i =
      if i >= sz then ifnotfound else begin
        match get_copy bucket1 i with
        | Some v when H.equal v d && check bucket2 i 
	    -> iffound bucket1 bucket2 i
        | _ -> loop (i+1)
      end
    in
    loop 0
  ;;

  let replace t k d = 
    if (find_shadow t k 
	  (fun w1 w2 i -> 
	     set w1 i (Some k); set w2 i (Some d); false ) true) 
    then
      add t k d

  let remove t d = find_shadow t d
		     (fun w1 w2 i -> set w1 i None; set w2 i None) ()

  let mem t d = find_shadow t d (fun _ _ i -> true) false

  let find_all t d =
    let index = get_index t d in
    let (bucket1, bucket2) = t.table.(index) in
    let sz = length bucket1 in
    let rec loop i accu =
      if i >= sz then accu
      else begin
        match get_copy bucket1 i with
        | Some v when H.equal v d
           -> begin match get bucket2 i with
              | Some v -> loop (i+1) (v::accu)
              | None -> loop (i+1) accu
              end
        | _ -> loop (i+1) accu
      end
    in
    loop 0 []
  ;;

  let stats t =
    let len = Array.length t.table in
    let lens = Array.map (fun (b,_) -> length b) t.table in
    Array.sort compare lens;
    let totlen = Array.fold_left ( + ) 0 lens in
    (len, count t, totlen, lens.(0), lens.(len/2), lens.(len-1))
  ;;

end;;

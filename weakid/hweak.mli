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

module type S =
  sig
    type key
    (** The type of the elements stored in the table. *)
    type 'a t
    (** The type of weak hash tables from type [key] to type ['a]. 
        if etheir the key or the data of a binding is freed by the GC,
        the binding is silently droped *)
    val create : int -> 'a t
    (** [create n] creates a new empty weak hash table, of initial
        size [n].  The table will grow as needed. *)
    val clear : 'a t -> unit
    (** Remove all elements from the table. *)
    val add : 'a t -> key -> 'a -> unit
    (** [add tbl key x] adds a binding of [k] to [x] in table [t]. Previous
        binding for [x] are not removed, and which binding will be find 
        by next [find] (or [merge]) is unspecified *)
    val replace : 'a t -> key -> 'a -> unit
    (** [replace tbl key x] replace the current binding of [key] in table
        [t] by a binding from [key] to [x]. If there was no such binding
        a new one is still created. This new binding will be
        the one find by next find (and merge) *)
    val remove : 'a t -> key -> unit
    (** [remove tbl x] removes the current binding of [x] in [tbl].
        if there is another binding of [x] in [tbl] then it became the
        current one.
        It does nothing if x is not bound in [tbl] *)
    val merge : 'a t -> key -> 'a -> 'a
    (** [merge tbl key x] returns the current binding of [k] in [t] if any,
        or else adds a bindding of [k] to [x] in the table and return [x]. *)
    val find : 'a t -> key -> 'a
    (** [find tbl key] returns the current binding of [k] in [t] if any,
        otherwise raise Not_found *)
    val find_all : 'a t -> key -> 'a list
    (** [find tbl key] returns the current binding of [k] in [t] if any,
        otherwise raise Not_found *)
    val mem : 'a t -> key -> bool
    (** [mem tbl x] checks if [x] is bound in [tbl]. *)
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    (** [iter f tbl] applies [f] to all bindings in table [tbl].
        [f] receives the key as first argument, and the associated value
        as second argument. The order in which the bindings are passed to
        [f] is unspecified. Each binding is presented exactly once
        to [f]. *)
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** [fold f tbl init] computes
        [(f kN dN ... (f k1 d1 init)...)],
        where [k1 ... kN] are the keys of all bindings in [tbl],
        and [d1 ... dN] are the associated values.
        The order in which the bindings are passed to
        [f] is unspecified. Each binding is presented exactly once
        to [f]. *)

    val count : 'a t -> int
    val stats : 'a t -> int * int * int * int * int * int
    (** some statistic function *)
  end
module MakeWeakKeys :
  functor (H : Hashtbl.HashedType) -> S with type key = H.t
module MakeWeakValues :
  functor (H : Hashtbl.HashedType) -> S with type key = H.t

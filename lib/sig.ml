(*
 * Copyright (c) 2011 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module type ID = sig
  type t
  val of_int64: int64 -> t
  val to_int64: t -> int64
end

module Make_ID (A : sig end) : ID = struct
  type t = int64
  let of_int64 x = x
  let to_int64 x = x
end

module type T = sig
  type t
  type id
  type 'a get_params
  val init            : string -> (t, [`RW]) Db.t
  val init_read_only  : string -> (t, [`RO]) Db.t
  val save            : db:(t, [`RW]) Db.t -> t -> unit
  val get             : (?custom:(t -> bool) -> (t, [< `RO|`RW]) Db.t -> t list) get_params
  val get_by_id       : id:[`Eq of id] -> (t, [< `RO|`RW]) Db.t -> t
  val delete          : ?recursive:bool -> db:(t, [`RW]) Db.t -> t -> unit
  val id              : db:(t, [< `RO|`RW]) Db.t -> t -> id
end

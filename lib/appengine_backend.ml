(*
 * Copyright (c) 2009-2010
 *     Anil Madhavapeddy <anil@recoil.org>
 *     Thomas Gazagnaire <thomas@gazagnaire.com>
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

open Appengine_datastore
open Printf

type transaction_mode = [ `Deferred |`Immediate |`Exclusive ]

type state = {
    name : string;
    svc: datastore_service;
}

type env = [
      `Debug of string list
    | `Dot of string
    | `Index of (string * string list) list
    | `Unique of (string * string list) list ] list

let debug db (env:env) ty n e = 
	let in_env s = List.exists (function | `Debug sl -> List.mem s sl | _ -> false)  env in
	let d () = Printf.eprintf "%s(%s): %s\n%!" n db e in
	let b () = () in
	if match ty with
		|`Sql -> in_env "sql" || in_env "all"
		|`Cache -> in_env "cache" || in_env "all"
		|`Bind -> in_env "bind" || in_env "all"
	then d() else b()

let new_state name = 
    let svc = (new datastore_service_factory `Null)#getDatastoreService in
    { name = name; svc = svc; }

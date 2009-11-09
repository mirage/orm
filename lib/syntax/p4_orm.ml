(*pp camlp4orf *)
(*
 * Copyright (c) 2009 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2009 Thomas Gazagnaire <thomas@gazagnaire.com>
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

open Camlp4
open PreCast
open Ast

let init n = n ^ "init"

(* Utils *)
let list_of_ctyp_decl tds =
  let rec aux accu = function
    | Ast.TyAnd (loc, tyl, tyr)      -> aux (aux accu tyl) tyr
    | Ast.TyDcl (loc, id, _, ty, []) -> (id, ty) :: accu
    | _                               ->  failwith "list_of_ctyp_decl: unexpected type"
  in aux [] tds

module Env = struct
  type t = {
    indices: (bool * string * string list) list;
    debug_sql: bool;
    debug_binds: bool;
    debug_cache: bool;
    debug_dot: string option;
  }

  let empty = { indices = []; debug_sql = false; debug_binds = false; debug_cache = false; debug_dot = None }

  let create_sig tds =
    let _loc = loc_of_ctyp tds in
    let bindings = List.flatten (List.map (fun (n,_) -> [
      <:ctyp< $lid:P4_weakid.weakid_of n$ : $lid:n$ -> int64 >> ;
      <:ctyp< $lid:P4_weakid.of_weakid n$ : int64 -> $lid:n$ >> ;
      <:ctyp< $lid:P4_weakid.has_weakid n$ : $lid:n$ -> bool >> ;
      <:ctyp< $lid:P4_weakid.create_weakid n$ : $lid:n$ -> int64 >> ;
      <:ctyp< $lid:P4_weakid.set_weakid n$ : $lid:n$ -> int64 -> unit >> ]
      ) (list_of_ctyp_decl tds)) in
    <:ctyp< { $tySem_of_list bindings$ } >>

  let create tds =
    let _loc = loc_of_ctyp tds in
    let bindings = List.flatten (List.map (fun (n,_) -> [
      <:rec_binding< Deps.$lid:P4_weakid.weakid_of n$ = W.$lid:P4_weakid.weakid_of n$ >> ;
      <:rec_binding< Deps.$lid:P4_weakid.of_weakid n$ = W.$lid:P4_weakid.of_weakid n$ >> ;
      <:rec_binding< Deps.$lid:P4_weakid.has_weakid n$ = W.$lid:P4_weakid.has_weakid n$ >> ;
      <:rec_binding< Deps.$lid:P4_weakid.create_weakid n$ = W.$lid:P4_weakid.create_weakid n$ >> ;
      <:rec_binding< Deps.$lid:P4_weakid.set_weakid n$ = W.$lid:P4_weakid.set_weakid n$ >> ]
      ) (list_of_ctyp_decl tds)) in
    <:expr< let module W = struct $P4_weakid.gen tds$ end in { $rbSem_of_list bindings$ } >>
end

let gen env tds =
  let _loc = loc_of_ctyp tds in
  let ts = list_of_ctyp_decl tds in
  let bindings = List.map (fun (n,t) ->
    <:binding< $lid:init n$ =
      let module Deps = struct
	type env = $Env.create_sig tds$;
	$P4_type.gen tds$;
      end in
      fun db_name ->
        let db = Sql_backend.new_state $Env.create tds$ db_name in
        Sql_init.create_tables db $lid:P4_type.type_of n$ >>) ts in
  <:str_item<
    value $biAnd_of_list bindings$
  >>

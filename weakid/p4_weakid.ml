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

let weakid_of n     = "weakid_of_" ^ n
let of_weakid n     = n ^ "_of_weakid"
let has_weakid n    = n ^ "_has_weakid"
let create_weakid n = n ^ "_create_weakid"
let rm_weakid n     = n ^ "_rm_weakid"
let set_weakid n    = n ^ "_set_weakid"

let create_weakid_fns n    = n ^ "_create_weakid_fns"

let list_of_ctyp_decl tds =
  let rec aux accu = function
  | Ast.TyAnd (loc, tyl, tyr)      -> aux (aux accu tyl) tyr
  | Ast.TyDcl (loc, id, _, ty, []) -> (id, ty) :: accu
  | _                               ->  failwith "list_of_ctyp_decl: unexpected type"
  in aux [] tds

let gen_body envfn name ctyp =
  let _loc = loc_of_ctyp ctyp in
  <:expr<
    let module Wkeys = H.MakeWeakKeys (
      struct
        type __t__ = $lid:name$;
        type t = __t__;
        value equal = ( == );
        value hash x = $P4_hash.gen1 ~envfn ctyp$;
      end ) in
    let module Wvalues = H.MakeWeakValues (
      struct
        type t = int64;
        value equal = ( = );
        value hash = Hashtbl.hash;
      end ) in
    let wkeys = Wkeys.create 128 in
    let wvalues = Wvalues.create 128 in
    let count = ref 0L in
    ( Wkeys.find wkeys,
      Wvalues.find wvalues,
      Wkeys.mem wkeys,
      (fun t -> let id = count.val in
         do { Wkeys.replace wkeys t id; Wvalues.replace wvalues id t; count.val := Int64.add count.val 1L; id } ),
      (fun t -> try let id = Wkeys.find wkeys t in do { Wkeys.remove wkeys t; Wvalues.remove wvalues id } with [ _ -> () ] ),
      (fun t id -> do { Wkeys.replace wkeys t id; Wvalues.replace wvalues id t } ) )
  >>

let gen_fn envfn (name, ctyp) =
  let _loc = loc_of_ctyp ctyp in
  <:str_item<
    value $lid:create_weakid_fns name$ = fun () -> let module H = Hweak in $gen_body envfn name ctyp$;
    value ( $lid:weakid_of name$,
            $lid:of_weakid name$,
            $lid:has_weakid name$,
            $lid:create_weakid name$,
            $lid:rm_weakid name$,
            $lid:set_weakid name$ ) =
      $lid:create_weakid_fns name$ ()
  >>

let gen tds =
  let _loc = loc_of_ctyp tds in
  let env = list_of_ctyp_decl tds in
  let envfn id = try List.assoc id env with Not_found -> <:ctyp< t >> in
  let bindings = List.map (gen_fn envfn) env in
  <:str_item< $stSem_of_list bindings$ >>

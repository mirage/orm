(*pp camlp4orf *)

(*
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
open P4_utils

let make_name t =
  "type_of_"^t

let create_fun name ctyp =
  let _loc = loc_of_ctyp ctyp in
  let rec aux = function
    | <:ctyp< unit >> -> <:expr< `Unit >>
    | <:ctyp< int >> -> <:expr< `Int >>
    | <:ctyp< int32 >> -> <:expr< `Int32 >>
    | <:ctyp< int64 >> -> <:expr< `Int64 >>
    | <:ctyp< float >> -> <:expr< `Float >>
    | <:ctyp< bool >> -> <:expr< `Bool >>
    | <:ctyp< char >> -> <:expr< `Char >>
    | <:ctyp< string >> -> <:expr< `String >>
    | <:ctyp< option $ty$ >> -> <:expr< `Option $aux ty$ >>
    | <:ctyp< ( $tup:tp$ ) >> -> <:expr< `Product $List.fold_left (fun accu x -> <:expr< [ $aux x$ :: $accu$ ] >>) <:expr< [] >> (list_of_ctyp tp [])$ >>
    | <:ctyp< $lid:id$ >> -> <:expr< if type_name = $str:name$ then `Rec else $lid:make_name id$ type_name >>  
    | <:ctyp< list $ctyp$ >>
    | <:ctyp< array $ctyp$ >> -> <:expr< `Collection $aux ctyp$ >>
    | <:ctyp< [< $row_fields$ ] >> 
    | <:ctyp< [> $row_fields$ ] >>
    | <:ctyp< [= $row_fields$ ] >> 
    | <:ctyp< [ $row_fields$ ] >> ->
        let rec fn accu = function
          | <:ctyp< $t1$ | $t2$ >> -> fn (fn accu t1) t2
          | <:ctyp< $uid:id$ of $t$ >> -> <:expr< [ ($str:id$, [ $exSem_of_list (List.map aux (list_of_ctyp t []))$ ]) :: $accu$ ] >>
          | <:ctyp< $uid:id$ >> -> <:expr< [ ($str:id$, []) :: $accu$ ] >>
          | _ -> failwith "unexpected AST" in
        <:expr< `Named_sum $fn <:expr< [] >> row_fields$ >>
    | <:ctyp< { $fs$ } >> ->
	let rec fn accu = function
          | <:ctyp< $t1$; $t2$ >> -> fn (fn accu t1) t2
          | <:ctyp< $lid:id$ : mutable $t$ >>
          | <:ctyp< $lid:id$ : $t$ >> -> <:expr< [ ($str:id$, $aux t$) :: $accu$ ] >>
          | _ -> failwith "unexpected AST" in
        <:expr< `Named_product $fn <:expr< [] >> fs$ >>
    | x -> 
        debug_ctyp x;
        failwith "unknown type" in
 <:binding< $lid:make_name name$ type_name : Orm.Types.t = $aux ctyp$ >>

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

let list_of_ctyp_decl tds =
  let rec aux accu = function
  | Ast.TyAnd (loc, tyl, tyr)      -> aux (aux accu tyl) tyr
  | Ast.TyDcl (loc, id, _, ty, []) -> (loc, id, ty) :: accu
  | _                               ->  failwith "list_of_ctyp_decl: unexpected type"
  in aux [] tds

let list_of_fields ctyp =
  let rec fn accu = function
    | <:ctyp< $t1$; $t2$ >>             -> fn (fn accu t1) t2
    | <:ctyp< $lid:id$ : mutable $t$ >>
    | <:ctyp< $lid:id$ : $t$ >>         -> id :: accu
    | _                                 -> failwith "unexpected AST" in
  fn [] ctyp

let mapi fn =
  let pos = ref 0 in
  List.map (fun x ->
    incr pos;
    fn !pos x
  ) 

let gen_normal tds =
  let aux (_loc, n, ctyp) =
    let create n = <:patt< $lid:n ^ "_create"$ >> in
    let create_expr n = <:expr< $lid:n ^ "_create"$ >> in
    match ctyp with
    | <:ctyp< option $ty$ >> -> <:binding< $create n$ = fun ?x -> fun () -> x >>
    | <:ctyp< unit >>
    | <:ctyp< int >>
    | <:ctyp< int32 >>
    | <:ctyp< int64 >>
    | <:ctyp< float >>
    | <:ctyp< bool >>
    | <:ctyp< list $_$ >>
    | <:ctyp< array $_$ >>
    | <:ctyp< char >>
    | <:ctyp< [< $_$ ] >> 
    | <:ctyp< [> $_$ ] >>
    | <:ctyp< [= $_$ ] >> 
    | <:ctyp< [ $_$ ] >>
    | <:ctyp< string >>      -> <:binding< $create n$ = fun x -> x >>
    | <:ctyp< ( $tup:tp$ ) >> ->
      let args = mapi (fun i _ -> "x" ^ string_of_int i) (list_of_ctyp tp []) in
      let body = <:expr< ( $exCom_of_list (List.map (fun a -> <:expr< $lid:a$ >>) args)$ ) >> in
      <:binding< $create n$ = $List.fold_right (fun a accu -> <:expr< fun $lid:a$ -> $accu$ >>) args body$ >>
    | <:ctyp< < $t$ > >>     ->
      let fields = list_of_fields t in
      let args = List.map (fun id -> <:patt< ~ $lid:id$ >>) fields in
      let body = <:expr< object $crSem_of_list (List.map (fun id -> <:class_str_item< method $lid:id$ = $lid:id$ >>) fields)$ end >> in
      <:binding< $create n$ = $List.fold_right (fun a accu -> <:expr< fun $a$ -> $accu$ >>) args body$ >>
    | <:ctyp< { $t$ } >>     ->
      let fields = list_of_fields t in
      let args = List.map (fun id -> <:patt< ~ $lid:id$ >>) fields in
      let body = <:expr< { $rbSem_of_list (List.map (fun id -> <:rec_binding< $lid:id$ = $lid:id$ >>) fields)$ } >> in
      <:binding< $create n$ = $List.fold_right (fun a accu -> <:expr< fun $a$ -> $accu$ >>) args body$ >>
    | <:ctyp< $lid:id$ >>    -> <:binding< $create n$ = $create_expr id$ >>
    | x                      -> failwith "unknown type" in
  let _loc = loc_of_ctyp tds in
  let bindings = List.map aux (list_of_ctyp_decl tds) in
  <:str_item< value rec $biAnd_of_list bindings$ >>

let gen_lazy tds =
  let aux (_loc, n, ctyp) =
    let create n = <:patt< $lid:n ^ "_create_lazy"$ >> in
    let create_expr n = <:expr< $lid:n ^ "_create_lazy"$ >> in
    match ctyp with
    | <:ctyp< option $ty$ >> -> <:binding< $create n$ = fun ?x -> fun () -> x >>
    | <:ctyp< unit >>
    | <:ctyp< int >>
    | <:ctyp< int32 >>
    | <:ctyp< int64 >>
    | <:ctyp< float >>
    | <:ctyp< bool >>
    | <:ctyp< list $_$ >>
    | <:ctyp< array $_$ >>
    | <:ctyp< char >>
    | <:ctyp< [< $_$ ] >> 
    | <:ctyp< [> $_$ ] >>
    | <:ctyp< [= $_$ ] >> 
    | <:ctyp< [ $_$ ] >>
    | <:ctyp< string >>      -> <:binding< $create n$ = fun x -> x >>
    | <:ctyp< ( $tup:tp$ ) >> ->
      let args = mapi (fun i _ -> "x" ^ string_of_int i) (list_of_ctyp tp []) in
      let body = <:expr< ( $exCom_of_list (List.map (fun a -> <:expr< $lid:a$ >>) args)$ ) >> in
      <:binding< $create n$ = $List.fold_right (fun a accu -> <:expr< fun $lid:a$ -> $accu$ >>) args body$ >>
    | <:ctyp< < $t$ > >>     ->
      let fields = list_of_fields t in
      let args = List.map (fun id -> <:patt< ~ $lid:id$ >>) fields in
      let body = <:expr< object $crSem_of_list (List.map (fun id -> <:class_str_item< method $lid:id$ = $lid:id$ >>) fields)$ end >> in
      <:binding< $create n$ = $List.fold_right (fun a accu -> <:expr< fun $a$ -> $accu$ >>) args body$ >>
    | <:ctyp< { $t$ } >>     ->
      let fields = list_of_fields t in
      let args = List.map (fun id -> <:patt< ~ $lid:id$ >>) fields in
      let body = <:expr< object $crSem_of_list (List.map (fun id -> <:class_str_item< method $lid:id$ = $lid:id$ () >>) fields)$ end >> in
      <:binding< $create n$ = $List.fold_right (fun a accu -> <:expr< fun $a$ -> $accu$ >>) args body$ >>
    | <:ctyp< $lid:id$ >>    -> <:binding< $create n$ = $create_expr id$ >>
    | x                      -> failwith "unknown type" in
  let _loc = loc_of_ctyp tds in
  let bindings = List.map aux (list_of_ctyp_decl tds) in
  <:str_item< value rec $biAnd_of_list bindings$ >>

let gen tds =
  let _loc = loc_of_ctyp tds in
  <:str_item< $gen_normal tds$; $gen_lazy tds$ >>

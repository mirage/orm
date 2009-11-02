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

(* Type generator *)

open Camlp4
open PreCast
open Ast

open Type

let list_of_ctyp_decl tds =
  let rec aux accu = function
  | Ast.TyAnd (loc, tyl, tyr)      -> aux (aux accu tyl) tyr
  | Ast.TyDcl (loc, id, _, ty, []) -> (loc, id, ty) :: accu
  | _                               ->  failwith "list_of_ctyp_decl: unexpected type"
  in aux [] tds

(* For each type declaration in tds, returns the corresponding unrolled Type.t.        *)
(* The remaining free variables in the type corresponds to external type declarations. *)
let create tds : (loc * string * t) list =
  let ctyps = list_of_ctyp_decl tds in
  let all_vars = List.map (fun (_,v,_) -> v) ctyps in
  let table = Hashtbl.create 16 in
  let rec aux bind_vars ctyp =
    match ctyp with
    | <:ctyp< unit >> -> Unit
    | <:ctyp< int >> -> Int
    | <:ctyp< int32 >> -> Int32
    | <:ctyp< int64 >> -> Int64
    | <:ctyp< float >> -> Float
    | <:ctyp< bool >> -> Bool
    | <:ctyp< char >> -> Char
    | <:ctyp< string >> -> String
    | <:ctyp< option $ty$ >> -> Option (aux bind_vars ty)
    | <:ctyp< ( $tup:tp$ ) >> -> Product (List.map (aux bind_vars) (list_of_ctyp tp []))
    | <:ctyp< list $ctyp$ >>
    | <:ctyp< array $ctyp$ >> -> Collection (aux bind_vars ctyp)
    | <:ctyp< [< $variants$ ] >> 
    | <:ctyp< [> $variants$ ] >>
    | <:ctyp< [= $variants$ ] >> 
    | <:ctyp< [ $variants$ ] >> ->
        let rec fn accu = function
          | <:ctyp< $t1$ | $t2$ >>     -> fn (fn accu t1) t2
          | <:ctyp< $uid:id$ of $t$ >> -> (id, List.map (aux bind_vars) (list_of_ctyp t [])) :: accu
          | <:ctyp< $uid:id$ >>        -> (id, []) :: accu
          | _ -> failwith "unexpected AST" in
        Named_sum (fn [] variants)
    | <:ctyp< { $fields$ } >> ->
	let rec fn accu = function
          | <:ctyp< $t1$; $t2$ >>             -> fn (fn accu t1) t2
          | <:ctyp< $lid:id$ : mutable $t$ >> -> (id, M, aux bind_vars t) :: accu
          | <:ctyp< $lid:id$ : $t$ >>         -> (id, I, aux bind_vars t) :: accu
          | _                                 -> failwith "unexpected AST" in
        Named_product (fn []  fields)
    | <:ctyp< $lid:id$ >> when not (List.mem id all_vars) || List.mem id bind_vars -> Var id
    | <:ctyp< $lid:id$ >> -> (Hashtbl.find table id) (id :: bind_vars)
    | x -> failwith "unknown type" in
  List.iter (fun (loc, name, ctyp) -> Hashtbl.replace table name (fun bind_vars -> aux bind_vars ctyp)) ctyps;
  let types = List.map (fun (loc, name, ctyp) -> loc, name, (Hashtbl.find table name) [name]) ctyps in
  List.map (fun (loc, name, t) -> loc, name, if List.mem name (free_vars t) then Rec (name, t) else t) types

let make_name t =
  "type_of_"^t

let gen tds =
  let _loc = loc_of_ctyp tds in
  let types = create tds in
  let subst_external_var (_loc, name, t) =
    let freev = free_vars t in
    let rec aux = function
    | Var v when not (List.mem v freev) -> <:expr< $lid:make_name v$ () >>
    | Var v                             -> <:expr< Var t >>
    | Rec (v, t)                        -> <:expr< Rec (v, $aux t$) >>
    | Unit                              -> <:expr< Unit >>
    | Int                               -> <:expr< Int >>
    | Int32                             -> <:expr< Int32 >>
    | Int64                             -> <:expr< Int64 >>
    | Float                             -> <:expr< Float >>
    | Bool                              -> <:expr< Bool >>
    | Char                              -> <:expr< Char >>
    | String                            -> <:expr< String >>
    | Option t                          -> <:expr< Option $aux t$ >>
    | Product tl                        -> <:expr< Product $List.fold_left (fun accu x -> <:expr< [ $aux x$ :: $accu$ ] >>) <:expr< [] >> tl$ >>
    | Collection t                      -> <:expr< Collection $aux t$ >>
    | Named_sum ts                      -> 
      let rec fn accu = function
      | []          -> accu
      | (n, t) :: l -> <:expr< [ ( $str:n$, $List.fold_left (fun accu x -> <:expr< [ $aux x$ :: $accu$ ] >>) <:expr< [] >> t$ ) :: $fn accu l$ ] >> in
      <:expr< Named_sum $fn <:expr< [] >> ts$ >>
    | Named_product ts                  ->
      let rec fn accu = function
      | []              -> accu
      | (n, M, t) :: l -> <:expr< [ ($str:n$, M, $aux t$) :: $fn accu l$ ] >>
      | (n, I, t) :: l -> <:expr< [ ($str:n$, I, $aux t$) :: $fn accu l$ ] >> in
      <:expr< Named_product $fn <:expr< [] >> ts$ >>
    in
    <:binding< $lid:make_name name$ = $aux t$ >>
  in
  let bindings = List.map subst_external_var types in
  <:str_item< value $biAnd_of_list bindings$ >>

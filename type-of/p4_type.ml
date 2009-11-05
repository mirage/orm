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

exception Type_not_supported of ctyp

(* For each type declaration in tds, returns the corresponding unrolled Type.t.        *)
(* The remaining free variables in the type corresponds to external type declarations. *)
let create tds : (loc * string * t) list =
  let bind v t = if List.mem v (free_vars t) then Rec (v, t) else t in
  let tablefn = Hashtbl.create 16 in
  let register name fn = Hashtbl.replace tablefn name fn in
  let apply name arg = bind name ((Hashtbl.find tablefn name) arg) in
  let exists name = Hashtbl.mem tablefn name in

  let rec aux bind_vars ctyp =
    match ctyp with
    | <:ctyp< unit >>         -> Unit
    | <:ctyp< int >>          -> Int
    | <:ctyp< int32 >>        -> Int32
    | <:ctyp< int64 >>        -> Int64
    | <:ctyp< float >>        -> Float
    | <:ctyp< bool >>         -> Bool
    | <:ctyp< char >>         -> Char
    | <:ctyp< string >>       -> String
    | <:ctyp< option $ty$ >>  -> Option (aux bind_vars ty)
    | <:ctyp< ( $tup:tp$ ) >> -> Product (List.map (aux bind_vars) (list_of_ctyp tp []))
    | <:ctyp< list $ctyp$ >>
    | <:ctyp< array $ctyp$ >> -> Enum (aux bind_vars ctyp)
    | <:ctyp< [< $variants$ ] >> 
    | <:ctyp< [> $variants$ ] >>
    | <:ctyp< [= $variants$ ] >> 
    | <:ctyp< [ $variants$ ] >> ->
        let rec fn accu = function
          | <:ctyp< $t1$ | $t2$ >>     -> fn (fn accu t1) t2
          | <:ctyp< `$uid:id$ of $t$ >>
          | <:ctyp< $uid:id$ of $t$ >> -> (id, List.map (aux bind_vars) (list_of_ctyp t [])) :: accu
          | <:ctyp< `$uid:id$ >>
          | <:ctyp< $uid:id$ >>        -> (id, []) :: accu
          | _ -> failwith "unexpected AST" in
        Sum (fn [] variants)
    | <:ctyp< { $fields$ } >> | <:ctyp< < $fields$ > >> ->
	let rec fn accu = function
          | <:ctyp< $t1$; $t2$ >>             -> fn (fn accu t1) t2
          | <:ctyp< $lid:id$ : mutable $t$ >> -> (id, `M, aux bind_vars t) :: accu
          | <:ctyp< $lid:id$ : $t$ >>         -> (id, `I, aux bind_vars t) :: accu
          | _                                 -> failwith "unexpected AST" in
        Dict (fn []  fields)
	| <:ctyp< $t$ -> $u$ >>   -> Arrow ( (aux bind_vars t), (aux bind_vars u) )
    | <:ctyp< $lid:id$ >> when not (exists id) || List.mem id bind_vars -> Var id
    | <:ctyp< $lid:id$ >>     -> apply id (id :: bind_vars)
    | x                       -> raise (Type_not_supported x) in

  let ctyps = list_of_ctyp_decl tds in
  List.iter (fun (loc, name, ctyp) -> register name (fun bind_vars -> aux bind_vars ctyp)) ctyps;
  List.map (fun (loc, name, ctyp) -> loc, name, apply name [name]) ctyps

let make_name t =
  "type_of_"^t

let gen module_name tds =
  let _loc = loc_of_ctyp tds in
  let types = create tds in
  let subst_external_var (_loc, name, t) =
    let freev = free_vars t in
    let rec aux = function
    | Var v when List.mem v freev
                 -> <:expr< $lid:make_name v$ >>
    | Var v      -> <:expr< T.Var $str:v$ >>
    | Rec (v, t) -> <:expr< T.Rec ($str:v$, $aux t$) >>
    | Unit       -> <:expr< T.Unit >>
    | Int        -> <:expr< T.Int >>
    | Int32      -> <:expr< T.Int32 >>
    | Int64      -> <:expr< T.Int64 >>
    | Float      -> <:expr< T.Float >>
    | Bool       -> <:expr< T.Bool >>
    | Char       -> <:expr< T.Char >>
    | String     -> <:expr< T.String >>
    | Option t   -> <:expr< T.Option $aux t$ >>
    | Product tl -> <:expr< T.Product $List.fold_left (fun accu x -> <:expr< [ $aux x$ :: $accu$ ] >>) <:expr< [] >> tl$ >>
    | Enum t     -> <:expr< T.Enum $aux t$ >>
    | Sum ts     -> 
      let rec fn accu = function
      | []          -> accu
      | (n, t) :: l -> <:expr< [ ( $str:n$, $List.fold_left (fun accu x -> <:expr< [ $aux x$ :: $accu$ ] >>) <:expr< [] >> t$ ) :: $fn accu l$ ] >> in
      <:expr< T.Sum $fn <:expr< [] >> ts$ >>
    | Dict ts    ->
      let rec fn accu = function
      | []              -> accu
      | (n, `M, t) :: l -> <:expr< [ ($str:n$, `M, $aux t$) :: $fn accu l$ ] >>
      | (n, `I, t) :: l -> <:expr< [ ($str:n$, `I, $aux t$) :: $fn accu l$ ] >> in
      <:expr< T.Dict $fn <:expr< [] >> ts$ >>
    | Arrow(t, s) -> <:expr< T.Arrow( $aux t$, $aux s$ ) >>
    in
    <:binding< $lid:make_name name$ = let module T = $uid:module_name$ in $aux t$ >>
  in
  let bindings = List.map subst_external_var types in
  <:str_item< value $biAnd_of_list bindings$ >>

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
open Syntax

(* module PP = Camlp4.Printers.OCaml.Make(Syntax)
let pp = new PP.printer ()
let debug_ctyp ty = Format.eprintf "DEBUG CTYP: %a@." pp#ctyp ty *)

(* convenience function to wrap the TyDcl constructor since I cant
   find an appropriate quotation to use for this *)
let declare_type name ty =
  let _loc = loc_of_ctyp ty in
  Ast.TyDcl (_loc, name, [], ty, [])

(* defines the Ast.binding for a function of form:
let fun_name ?(opt_arg1) ?(opt_arg2) ident1 ident2 = function_body ...
*)
let function_with_label_args ~fun_name ~idents ~function_body ~return_type opt_args =
  let _loc = loc_of_expr function_body in
  let opt_args = opt_args @ (List.map (fun x -> <:patt< $lid:x$ >>) idents) in
    <:binding< $lid:fun_name$ = 
      $List.fold_right (fun b a ->
        <:expr<fun $b$ -> $a$ >>
       ) opt_args <:expr< ( $function_body$ : $return_type$ ) >>
      $ >>

(* convert a list of bindings into an expr fragment:
   let x = 1 in y = 2 in z = 3 in ()
*)
let biList_to_expr bindings final =
  let _loc = loc_of_expr final in
  List.fold_right (fun b a -> 
    <:expr< let $b$ in $a$ >>
  ) bindings final

(* build something like 'f ?x1 ?x2 ?x3 ... xn' *)
let apply _loc f label_args =
  let make x = Ast.ExId (_loc, Ast.IdLid (_loc, x)) in
  let make_label x = Ast.ExOlb (_loc, x, Ast.ExNil _loc) in
  let rec aux = function
  | []   -> make f
  | h::t -> Ast.ExApp (_loc, aux t , make_label h) in
  aux (List.rev label_args)

let access_array _loc a i =
  let make x = Ast.ExId (_loc, Ast.IdLid (_loc, x)) in
  Ast.ExAre (_loc, make a, Ast.ExInt (_loc, string_of_int i))

let ctyp_is_list = function
  | <:ctyp< list $c$ >> 
  | <:ctyp< array $c$ >> -> true
  | _ -> false

(* List.map with the integer position passed to the function *)
let mapi fn =
  let pos = ref 0 in
  List.map (fun x ->
    incr pos;
    fn !pos x
  ) 

let make_function _loc ?opt_args ?label_args ?return_type ~name ~args ~body () =
	let opt_args = match opt_args with
	| None      -> []
	| Some opts -> List.map (fun o -> <:patt< ? $lid:o$ >>) opts in
	let label_args = match label_args with
	| None      -> []
	| Some labs -> List.map (fun l -> <:patt< ~ $lid:l$ >>) labs in
    let args = List.map (fun a -> <:patt< $lid:a$ >>) args in
	let body = match return_type with
	| None      -> body
	| Some rtyp -> <:expr< ( $body$ : $rtyp$ ) >> in
	<:binding< $lid:name$ = $List.fold_right (fun b a -> <:expr< fun $b$ -> $a$ >>) (opt_args @ label_args @ args) body$ >>

let list_of_ctyp_decl tds =
	let rec aux accu = function
	| Ast.TyAnd (loc, tyl, tyr)      -> aux (aux accu tyl) tyr
	| Ast.TyDcl (loc, id, _, ty, []) -> (loc, id, ty) :: accu
	| _                               ->  failwith "list_of_ctyp_decl: unexpected type"
	in aux [] tds

let expr_list_of_list _loc exprs =
	match List.rev exprs with
	| []   -> <:expr< [] >>
	| h::t -> List.fold_left (fun accu x -> <:expr< [ $x$ :: $accu$ ] >>) <:expr< [ $h$ ] >> t 

let patt_list_of_list _loc patts =
	match List.rev patts with
	| []   -> <:patt< [] >>
	| h::t -> List.fold_left (fun accu x -> <:patt< [ $x$ :: $accu$ ] >>) <:patt< [ $h$ ] >> t

let expr_tuple_of_list _loc = function
	| []   -> <:expr< >>
	| [x]  -> x
	| h::t -> ExTup (_loc, List.fold_left (fun accu n -> <:expr< $accu$, $n$ >>) h t)

let patt_tuple_of_list _loc = function
	| []   -> <:patt< >>
	| [x]  -> x
	| h::t -> PaTup (_loc, List.fold_left (fun accu n -> <:patt< $accu$, $n$ >>) h t)

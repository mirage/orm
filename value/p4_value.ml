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

(* Fork of http://xenbits.xen.org/xapi/xen-api-libs.hg?file/7a17b2ab5cfc/rpc-light/pa_rpc.ml *)


open Camlp4
open PreCast
open Ast

open Type

let value_of _loc n = <:patt< $lid:"value_of_" ^ n$ >>
let of_value _loc n = <:patt< $lid:n ^ "_value_of"$ >>

(* Utils *)

let debug_ctyp ctyp =
	let module PP = Camlp4.Printers.OCaml.Make(Syntax) in
	let pp = new PP.printer () in
	Format.eprintf "unknown type %a@.\n" pp#ctyp ctyp

let list_of_ctyp_decl tds =
	let rec aux accu = function
	| Ast.TyAnd (loc, tyl, tyr)      -> aux (aux accu tyl) tyr
	| Ast.TyDcl (loc, id, _, ty, []) -> (id, ty) :: accu
	| _                               ->  failwith "list_of_ctyp_decl: unexpected type"
	in aux [] tds

let rec decompose_fields _loc fields =
	match fields with
	| <:ctyp< $t1$; $t2$ >> ->
		decompose_fields _loc t1 @ decompose_fields _loc t2
	| <:ctyp< $lid:field_name$: mutable $t$ >> | <:ctyp< $lid:field_name$: $t$ >> ->
		[ field_name, t ]
	| _ -> failwith "unexpected type while processing fields"

let expr_list_of_list _loc exprs =
	match List.rev exprs with
	| []   -> <:expr< [] >>
	| h::t -> List.fold_left (fun accu x -> <:expr< [ $x$ :: $accu$ ] >>) <:expr< [ $h$ ] >> t 

let patt_list_of_list _loc patts =
	match List.rev patts with
	| []   -> <:patt< [] >>
	| h::t -> List.fold_left (fun accu x -> <:patt< [ $x$ :: $accu$ ] >>) <:patt< [ $h$ ] >> t

let expr_tuple_of_list _loc exprs =
  <:expr< ( $exCom_of_list exprs$ ) >>

let patt_tuple_of_list _loc exprs =
  <:patt< ( $paCom_of_list exprs$ ) >>

let decompose_variants _loc variant =
	let rec fn accu = function
	| <:ctyp< $t$ | $u$ >>        -> fn (fn accu t) u
	| <:ctyp< $uid:id$ of $t$ >>  -> ((id, `V) , list_of_ctyp t []) :: accu
	| <:ctyp< `$uid:id$ of $t$ >> -> ((id, `PV), list_of_ctyp t []) :: accu
	| <:ctyp< $uid:id$ >>         -> ((id, `V) , []) :: accu
	| <:ctyp< `$uid:id$ >>        -> ((id, `PV), []) :: accu
	| _ -> failwith "decompose_variant"
	in
	List.split (fn [] variant)

let recompose_variant _loc (n, t) patts =
	match t, patts with
	| `V , [] -> <:patt< $uid:n$ >>
	| `PV, [] -> <:patt< `$uid:n$ >>
	| `V , _  -> <:patt< $uid:n$ $patt_tuple_of_list _loc patts$ >>
	| `PV, _  -> <:patt< `$uid:n$ $patt_tuple_of_list _loc patts$ >>

let count = ref 0
let new_id _loc =
	incr count;
	let new_id = Printf.sprintf "__x%i__" !count in
	<:expr< $lid:new_id$ >>, <:patt< $lid:new_id$ >>

let new_id_list _loc l =
	List.split (List.map (fun _ -> new_id _loc) l)

exception Type_not_supported of ctyp

(* Conversion ML type -> Value.t *)
module Value_of = struct
	
	let rec create id ctyp =
		let _loc = loc_of_ctyp ctyp in
		match ctyp with
		| <:ctyp< unit >>    -> <:expr< V.Null >>
		| <:ctyp< int >>     -> <:expr< V.Int (Int64.of_int $id$) >>
		| <:ctyp< int32 >>   -> <:expr< V.Int (Int64.of_int32 $id$) >>
		| <:ctyp< int64 >>   -> <:expr< V.Int $id$ >>
		| <:ctyp< float >>   -> <:expr< V.Float $id$ >>
		| <:ctyp< char >>    -> <:expr< V.Int (Int64.of_int (Char.code $id$)) >>
		| <:ctyp< string >>  -> <:expr< V.String $id$ >>
		| <:ctyp< bool >>    -> <:expr< V.Bool $id$ >>

		| <:ctyp< [< $t$ ] >> | <:ctyp< [> $t$ ] >> | <:ctyp< [= $t$ ] >> | <:ctyp< [ $t$ ] >> ->
			let ids, ctyps = decompose_variants _loc t in
			let pattern (n, t) ctyps =
				let ids, pids = new_id_list _loc ctyps in
				let body = <:expr< V.List [ V.String $str:n$ :: $expr_list_of_list _loc (List.map2 create ids ctyps)$ ] >> in
				<:match_case< $recompose_variant _loc (n,t) pids$ -> $body$ >> in
			let patterns = mcOr_of_list (List.map2 pattern ids ctyps) in
			<:expr< match $id$ with [ $patterns$ ] >>

		| <:ctyp< option $t$ >> ->
			let new_id, new_pid = new_id _loc in
			<:expr< match $id$ with [ Some $new_pid$ -> $create new_id t$ | None -> V.Null ] >> 

		| <:ctyp< $tup:tp$ >> ->
			let ctyps = list_of_ctyp tp [] in
			let ids, pids = new_id_list _loc ctyps in
			let exprs = List.map2 create ids ctyps in
			<:expr<
				let $patt_tuple_of_list _loc pids$ = $id$ in
				V.List $expr_list_of_list _loc exprs$
			>>

		| <:ctyp< list $t$ >> ->
			let new_id, new_pid = new_id _loc in
			<:expr< V.List (List.map (fun $new_pid$ -> $create new_id t$) $id$) >>

		| <:ctyp< array $t$ >> ->
			let new_id, new_pid = new_id _loc in
			<:expr< V.List (Array.to_list (Array.map (fun $new_pid$ -> $create new_id t$) $id$)) >>

		| <:ctyp< { $t$ } >> ->
			let fields = decompose_fields _loc t in
            let ids, pids = new_id_list _loc fields in
			let bindings = List.map2 (fun pid (f, _) -> <:binding< $pid$ = $id$ . $lid:f$ >>) pids fields in
			let one_expr nid (n, ctyp) = <:expr< ($str:n$, $create nid ctyp$) >> in
			let expr = <:expr< V.Dict $expr_list_of_list _loc (List.map2 one_expr ids fields)$ >> in
			<:expr< let $biAnd_of_list bindings$ in $expr$ >>

		| <:ctyp< < $t$ > >> ->
			let fields = decompose_fields _loc t in
            let ids, pids = new_id_list _loc fields in
			let bindings = List.map2 (fun pid (f, _) -> <:binding< $pid$ = $id$ # $lid:f$ >>) pids fields in
			let one_expr nid (n, ctyp) = <:expr< ($str:n$, $create nid ctyp$) >> in
			let expr = <:expr< V.Dict $expr_list_of_list _loc (List.map2 one_expr ids fields)$ >> in
			<:expr< let $biAnd_of_list bindings$ in $expr$ >>

		| <:ctyp< $t$ -> $u$ >> -> <:expr< V.Marshal (let module M = Marshal in M.to_string [ M.No_sharing; M.Closures ] $id$) >>

		| <:ctyp< $lid:t$ >> -> <:expr< V.Var $str:t$ >>

		| _ -> raise (Type_not_supported ctyp)

	let gen_one name ctyp =
		let _loc = loc_of_ctyp ctyp in
		let id, pid = new_id _loc in
		<:binding< $value_of _loc name$ = fun $pid$ -> let module V = Value in $create id ctyp$ >>

	let gen tds =
		let _loc = loc_of_ctyp tds in
		let ids, ctyps = List.split (list_of_ctyp_decl tds) in
		let bindings = List.map2 gen_one ids ctyps in
		let str =
			try <:str_item< value $biAnd_of_list bindings$ >>
			with Type_not_supported ctyp ->
				debug_ctyp ctyp;
				exit (-1) in
		str
end


(* Conversion Value.t -> ML type *)
module Of_value = struct

	let parse_error expected =
		let _loc = Loc.ghost in
		<:match_case< __x__ -> do {
			Printf.eprintf "Parse error: got '%s' while '%s' was expected\\n" (Value.to_string __x__) $str:expected$;
			raise (Parse_error($str:expected$, __x__)) }
		>>

	let parse_exn_error doing =
		let _loc = Loc.ghost in
		<:match_case< __x__ -> do {
			Printf.eprintf "Parse error: got exception '%s' doing '%s'\\n" (Printexc.to_string __x__) $str:doing$;
			raise (Parse_exn_error($str:doing$, __x__)) }
		>>

	let rec create id ctyp =
		let _loc = loc_of_ctyp ctyp in
		match ctyp with
		| <:ctyp< unit >>   -> <:expr< match $id$ with [ V.Null -> () | $parse_error "None"$ ] >>
		| <:ctyp< int >>    -> <:expr< match $id$ with [ V.Int x -> Int64.to_int x | $parse_error "Int(int)"$ ] >>
		| <:ctyp< int32 >>  -> <:expr< match $id$ with [ V.Int x -> Int64.to_int32 x | $parse_error "Int(int32)"$ ] >>
		| <:ctyp< int64 >>  -> <:expr< match $id$ with [ V.Int x ->  x | $parse_error "Int(int64)"$ ] >>
		| <:ctyp< float >>  -> <:expr< match $id$ with [ V.Float x -> x | $parse_error "Float"$ ] >>
		| <:ctyp< char >>   -> <:expr< match $id$ with [ V.Int x -> Char.chr (Int64.to_int x) | $parse_error "Int(char)"$ ] >>
		| <:ctyp< string >> -> <:expr< match $id$ with [ V.String x -> x | $parse_error "String(string)"$ ] >>
		| <:ctyp< bool >>   -> <:expr< match $id$ with [ V.Bool x -> bool_of_string x | $parse_error "Bool"$ ] >>

		| <:ctyp< [< $t$ ] >> | <:ctyp< [> $t$ ] >> | <:ctyp< [= $t$ ] >> | <:ctyp< [ $t$ ] >> ->
			let ids, ctyps = decompose_variants _loc t in
			let pattern (n, t) ctyps =
				let ids, pids = new_id_list _loc ctyps in
				let patt = <:patt< V.List [ V.String $str:n$ :: $patt_list_of_list _loc pids$ ] >> in
				let exprs = List.map2 create ids ctyps in
				let body = List.fold_right
					(fun a b -> <:expr< $a$ $b$ >>)
					exprs
					(if t = `V then <:expr< $uid:n$ >> else <:expr< `$uid:n$ >>) in
				<:match_case< $patt$ -> $body$ >> in
			let fail_match = <:match_case< $parse_error "List[String;_]"$ >> in
			let patterns = mcOr_of_list (List.map2 pattern ids ctyps @ [ fail_match ]) in
			<:expr< match $id$ with [ $patterns$ ] >>

		| <:ctyp< option $t$ >> ->
			let nid, npid = new_id _loc in
			<:expr< match $id$ with [ Null -> None | $npid$ -> Some $create nid t$ ] >>

		| <:ctyp< $tup:tp$ >> ->
			let ctyps = list_of_ctyp tp [] in
			let ids, pids = new_id_list _loc ctyps in
			let exprs = List.map2 create ids ctyps in
			<:expr< match $id$ with [ V.List $patt_list_of_list _loc pids$ -> $expr_tuple_of_list _loc exprs$ | $parse_error "List"$ ] >>

		| <:ctyp< list $t$ >> ->
			let nid, npid = new_id _loc in
			let nid2, npid2 = new_id _loc in
			<:expr< match $id$ with [ V.List $npid$ -> List.map (fun $npid2$ -> $create nid2 t$) $nid$ | $parse_error "List"$ ] >>

		| <:ctyp< array $t$ >> ->
			let nid, npid = new_id _loc in
			let nid2, npid2 = new_id _loc in
			<:expr< match $id$ with [ V.List $npid$ -> Array.of_list (List.map (fun $npid2$ -> $create nid2 t$) $nid$) | $parse_error "List"$ ] >>

		| <:ctyp< { $t$ } >> ->
			let nid, npid = new_id _loc in
			let fields = decompose_fields _loc t in
			let ids, pids = new_id_list _loc fields in
			let exprs = List.map2 (fun id (n, ctyp) -> <:rec_binding< $lid:n$ = $create id ctyp$ >>) ids fields in
			let bindings =
				List.map2 (fun pid (n, ctyp) ->
					<:binding< $pid$ = try List.assoc $str:n$ $nid$ with [ $parse_exn_error ("Looking for key "^n)$ ] >>
					) pids fields in
			<:expr< match $id$ with [ V.Dict $npid$ -> let $biAnd_of_list bindings$ in { $rbSem_of_list exprs$ } | $parse_error "Dict(_)"$ ] >>

		| <:ctyp< < $t$ > >> ->
			let nid, npid = new_id _loc in
			let fields = decompose_fields _loc t in
			let ids, pids = new_id_list _loc fields in
			let exprs = List.map2 (fun id (n, ctyp) -> <:class_str_item< method $lid:n$ = $create id ctyp$ >>) ids fields in
			let bindings =
				List.map2 (fun pid (n, ctyp) ->
					<:binding< $pid$ = try List.assoc $str:n$ $nid$ with [ $parse_exn_error ("Looking for key "^n)$ ] >>
					) pids fields in
			<:expr< match $id$ with [ V.Dict $npid$ -> let $biAnd_of_list bindings$ in object $crSem_of_list exprs$ end | $parse_error "Dict(_)"$ ] >>

		| <:ctyp< $t$ -> $u$ >> ->
			<:expr< match $id$ with [ V.Marshal f -> (let module M = Marshal in M.from_string f : $t$ -> $u$) | $parse_error "Marshal"$ ] >>

		| <:ctyp< $lid:t$ >> -> <:expr< match $id$ with [ V.Var t -> $id$ | $parse_error "Var"$ ] >>

		| _ -> failwith "ML_of_rpc.scalar_of_ctyp: unsuported type"

	let gen_one name ctyp =
		let _loc = loc_of_ctyp ctyp in
		let id, pid = new_id _loc in
		<:binding< $of_value _loc name$ = fun $pid$ -> let module V = Value in $create id ctyp$ >>

	let gen tds =
		let _loc = loc_of_ctyp tds in
		let ids, ctyps = List.split (list_of_ctyp_decl tds) in
		let bindings = List.map2 gen_one ids ctyps in
		let str =
			try <:str_item< 
				exception Parse_error of (string * Value.t);
				exception Parse_exn_error of (string * exn);
				value $biAnd_of_list bindings$
			>>
			with Type_not_supported ctyp ->
				debug_ctyp ctyp;
				exit (-1) in
		str

end

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

let patt_value_of _loc n = <:patt< $lid:"value_of_" ^ n$ >>
let patt_value_of_aux _loc n = <:patt< $lid:"value_of_" ^ n ^ "_aux"$ >>
let patt_of_value _loc n = <:patt< $lid:n ^ "_of_value"$ >>
let patt_of_value_aux _loc n = <:patt< $lid:n ^ "_of_value_aux"$ >>

let expr_value_of _loc n = <:expr< $lid:"value_of_" ^ n$ >>
let expr_value_of_aux _loc n = <:expr< $lid:"value_of_" ^ n ^ "_aux"$ >>
let expr_of_value _loc n = <:expr< $lid:n ^ "_of_value"$ >>
let expr_of_value_aux _loc n = <:expr< $lid:n ^ "_of_value_aux"$ >>

let has_weakid _loc n = <:expr< Deps . $lid:n ^ "_has_weakid"$ >>
let create_weakid _loc n = <:expr< Deps . $lid:n ^ "_create_weakid"$ >>
let set_weakid _loc n = <:expr< Deps . $lid:n ^ "_set_weakid"$ >>
let weakid_of _loc n = <:expr< Deps . $lid:"weakid_of_" ^ n$ >>
let of_weakid _loc n = <:expr< Deps . $lid:n ^ "_of_weakid"$ >>


(* Utils *)

let debug_ctyp ctyp =
	let module PP = Camlp4.Printers.OCaml.Make(Syntax) in
	let pp = new PP.printer () in
	Format.printf "unknown type %a@.\n" pp#ctyp ctyp

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

let expr_tuple_of_list _loc = function
	| []   -> <:expr< >>
	| [x]  -> x
	| h::t -> ExTup (_loc, List.fold_left (fun accu n -> <:expr< $accu$, $n$ >>) h t)

let patt_tuple_of_list _loc = function
	| []   -> <:patt< >>
	| [x]  -> x
	| h::t -> PaTup (_loc, List.fold_left (fun accu n -> <:patt< $accu$, $n$ >>) h t)

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
	
	let env_type _loc names =
		<:ctyp< { $List.fold_left (fun accu n -> <:ctyp< $lid:n$ : list ( $lid:n$ * int64 ); $accu$ >>) <:ctyp< >> names$ } >>

	let empty_env _loc names =
		<:expr< { $rbSem_of_list (List.map (fun n -> <:rec_binding< Deps.$lid:n$ = [] >>) names)$ } >>

	let rec create names id ctyp =
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
				let body = <:expr< V.Sum ( $str:n$, $expr_list_of_list _loc (List.map2 (create names) ids ctyps)$ ) >> in
				<:match_case< $recompose_variant _loc (n,t) pids$ -> $body$ >> in
			let patterns = mcOr_of_list (List.map2 pattern ids ctyps) in
			<:expr< match $id$ with [ $patterns$ ] >>

		| <:ctyp< option $t$ >> ->
			let new_id, new_pid = new_id _loc in
			<:expr< match $id$ with [ Some $new_pid$ -> $create names new_id t$ | None -> V.Null ] >> 

		| <:ctyp< $tup:tp$ >> ->
			let ctyps = list_of_ctyp tp [] in
			let ids, pids = new_id_list _loc ctyps in
			let exprs = List.map2 (create names) ids ctyps in
			<:expr<
				let $patt_tuple_of_list _loc pids$ = $id$ in
				V.Tuple $expr_list_of_list _loc exprs$
			>>

		| <:ctyp< list $t$ >> ->
			let new_id, new_pid = new_id _loc in
			<:expr< V.Enum (List.map (fun $new_pid$ -> $create names new_id t$) $id$) >>

		| <:ctyp< array $t$ >> ->
			let new_id, new_pid = new_id _loc in
			<:expr< V.Enum (Array.to_list (Array.map (fun $new_pid$ -> $create names new_id t$) $id$)) >>

		| <:ctyp< { $t$ } >> ->
			let fields = decompose_fields _loc t in
            let ids, pids = new_id_list _loc fields in
			let bindings = List.map2 (fun pid (f, _) -> <:binding< $pid$ = $id$ . $lid:f$ >>) pids fields in
			let one_expr nid (n, ctyp) = <:expr< ($str:n$, $create names nid ctyp$) >> in
			let expr = <:expr< V.Dict $expr_list_of_list _loc (List.map2 one_expr ids fields)$ >> in
			<:expr< let $biAnd_of_list bindings$ in $expr$ >>

		| <:ctyp< < $t$ > >> ->
			let fields = decompose_fields _loc t in
            let ids, pids = new_id_list _loc fields in
			let bindings = List.map2 (fun pid (f, _) -> <:binding< $pid$ = $id$ # $lid:f$ >>) pids fields in
			let one_expr nid (n, ctyp) = <:expr< ($str:n$, $create names nid ctyp$) >> in
			let expr = <:expr< V.Dict $expr_list_of_list _loc (List.map2 one_expr ids fields)$ >> in
			<:expr< let $biAnd_of_list bindings$ in $expr$ >>

		| <:ctyp< $t$ -> $u$ >> -> <:expr< V.Arrow (let module M = Marshal in M.to_string $id$  [ M.No_sharing; M.Closures ]) >>

		| <:ctyp< $lid:t$ >> ->
			if not (List.mem t names) then
				<:expr< $expr_value_of _loc t$ $id$ >>
			else
				<:expr<
					if List.mem_assq $id$ __env__.Deps.$lid:t$
					then V.Var ($str:t$, List.assq $id$ __env__.Deps.$lid:t$)
					else begin
						let __id__ = if $has_weakid _loc t$ $id$
						then $weakid_of _loc t$ $id$
						else $create_weakid _loc t$ $id$ in
						let __value__ = $expr_value_of_aux _loc t$
							{ (__env__) with Deps.$lid:t$ = [ ($id$, __id__) :: __env__.Deps.$lid:t$ ] }
							$id$ in
						if List.mem ($str:t$, __id__) (V.free_vars __value__) then
							V.Rec (($str:t$, __id__), __value__ )
						else
							__value__
					end >>

		| _ -> raise (Type_not_supported ctyp)

	let gen_one names name ctyp =
		let _loc = loc_of_ctyp ctyp in
		let id, pid = new_id _loc in
		<:binding< $patt_value_of_aux _loc name$ = fun __env__ -> fun $pid$ ->
			let module V = Value in
			$create names id ctyp$
		>>

	let gen tds =
		let _loc = loc_of_ctyp tds in
		let ids, ctyps = List.split (list_of_ctyp_decl tds) in
		let bindings = List.map2 (gen_one ids) ids ctyps in
		biAnd_of_list bindings

	let inputs _loc ids =
		patt_tuple_of_list _loc (List.map (fun x -> <:patt< ($patt_value_of _loc x$ : $lid:x$ -> Value.t) >>) ids)

	let outputs _loc ids =
		expr_tuple_of_list _loc (List.map (fun x -> <:expr< $expr_value_of_aux _loc x$ $empty_env _loc ids$>>) ids)

end


(* Conversion Value.t -> ML type *)
module Of_value = struct

	let env_type _loc names =
		<:ctyp< { $List.fold_left (fun accu n -> <:ctyp< $lid:n$ : list (int64 * Lazy.t $lid:n$) ; $accu$ >>) <:ctyp< >> names$ } >>

	let empty_env _loc names =
		<:expr< { $rbSem_of_list (List.map (fun n -> <:rec_binding< Deps.$lid:n$ = [] >>) names)$ } >>

	let runtime_error expected =
		let _loc = Loc.ghost in
		<:match_case<  __x__ -> do {
			Printf.printf "Runtime error: got '%s' while '%s' was expected\\n" (Value.to_string __x__) $str:expected$;
			raise (Deps.Runtime_error($str:expected$, __x__)) }
		>>

	let error expected =
		let _loc = Loc.ghost in
		<:expr< do {
			Printf.printf "Runtime error: '%s'\\n" $str:expected$;
			raise (Deps.Runtime_error($str:expected$, V.Null)) }
		>>

	let runtime_exn_error doing =
		let _loc = Loc.ghost in
		<:match_case< __x__ -> do {
			Printf.printf "Runtime error: got exception '%s' doing '%s'\\n" (Printexc.to_string __x__) $str:doing$;
			raise (Deps.Runtime_exn_error($str:doing$, __x__)) }
		>>

	let rec create names id ctyp =
		let _loc = loc_of_ctyp ctyp in
		match ctyp with
		| <:ctyp< unit >>   -> <:expr< match $id$ with [ V.Null -> () | $runtime_error "None"$ ] >>
		| <:ctyp< int >>    -> <:expr< match $id$ with [ V.Int x -> Int64.to_int x | $runtime_error "Int(int)"$ ] >>
		| <:ctyp< int32 >>  -> <:expr< match $id$ with [ V.Int x -> Int64.to_int32 x | $runtime_error "Int(int32)"$ ] >>
		| <:ctyp< int64 >>  -> <:expr< match $id$ with [ V.Int x ->  x | $runtime_error "Int(int64)"$ ] >>
		| <:ctyp< float >>  -> <:expr< match $id$ with [ V.Float x -> x | $runtime_error "Float"$ ] >>
		| <:ctyp< char >>   -> <:expr< match $id$ with [ V.Int x -> Char.chr (Int64.to_int x) | $runtime_error "Int(char)"$ ] >>
		| <:ctyp< string >> -> <:expr< match $id$ with [ V.String x -> x | $runtime_error "String(string)"$ ] >>
		| <:ctyp< bool >>   -> <:expr< match $id$ with [ V.Bool x -> x | $runtime_error "Bool"$ ] >>

		| <:ctyp< [< $t$ ] >> | <:ctyp< [> $t$ ] >> | <:ctyp< [= $t$ ] >> | <:ctyp< [ $t$ ] >> ->
			let ids, ctyps = decompose_variants _loc t in
			let pattern (n, t) ctyps =
				let ids, pids = new_id_list _loc ctyps in
				let patt = <:patt< V.Sum ( $str:n$, $patt_list_of_list _loc pids$ ) >> in
				let exprs = List.map2 (create names) ids ctyps in
				let body = List.fold_right
					(fun a b -> <:expr< $b$ $a$ >>)
					(List.rev exprs)
					(if t = `V then <:expr< $uid:n$ >> else <:expr< `$uid:n$ >>) in
				<:match_case< $patt$ -> $body$ >> in
			let fail_match = <:match_case< $runtime_error "List[String;_]"$ >> in
			let patterns = mcOr_of_list (List.map2 pattern ids ctyps @ [ fail_match ]) in
			<:expr< match $id$ with [ $patterns$ ] >>

		| <:ctyp< option $t$ >> ->
			let nid, npid = new_id _loc in
			<:expr< match $id$ with [ V.Null -> None | $npid$ -> Some $create names nid t$ ] >>

		| <:ctyp< $tup:tp$ >> ->
			let ctyps = list_of_ctyp tp [] in
			let ids, pids = new_id_list _loc ctyps in
			let exprs = List.map2 (create names) ids ctyps in
			<:expr< match $id$ with
				[ V.Tuple $patt_list_of_list _loc pids$ -> $expr_tuple_of_list _loc exprs$ | $runtime_error "List"$ ]
			>>

		| <:ctyp< list $t$ >> ->
			let nid, npid = new_id _loc in
			let nid2, npid2 = new_id _loc in
			<:expr< match $id$ with
				[ V.Enum $npid$ -> List.map (fun $npid2$ -> $create names nid2 t$) $nid$ | $runtime_error "List"$ ]
			>>

		| <:ctyp< array $t$ >> ->
			let nid, npid = new_id _loc in
			let nid2, npid2 = new_id _loc in
			<:expr< match $id$ with
				[ V.Enum $npid$ -> Array.of_list (List.map (fun $npid2$ -> $create names nid2 t$) $nid$) | $runtime_error "List"$ ]
			>>

		| <:ctyp< { $t$ } >> ->
			let nid, npid = new_id _loc in
			let fields = decompose_fields _loc t in
			let ids, pids = new_id_list _loc fields in
			let exprs = List.map2 (fun id (n, ctyp) -> <:rec_binding< $lid:n$ = $create names id ctyp$ >>) ids fields in
			let bindings =
				List.map2 (fun pid (n, ctyp) ->
					<:binding< $pid$ = try List.assoc $str:n$ $nid$ with [ $runtime_exn_error ("Looking for key "^n)$ ] >>
					) pids fields in
			<:expr< match $id$ with
				[ V.Dict $npid$ -> let $biAnd_of_list bindings$ in { $rbSem_of_list exprs$ } | $runtime_error "Dict(_)"$ ]
			>>

		| <:ctyp< < $t$ > >> ->
			let nid, npid = new_id _loc in
			let fields = decompose_fields _loc t in
			let ids, pids = new_id_list _loc fields in
			let exprs = List.map2 (fun id (n, ctyp) -> <:class_str_item< method $lid:n$ = $create names id ctyp$ >>) ids fields in
			let bindings =
				List.map2 (fun pid (n, ctyp) ->
					<:binding< $pid$ = try List.assoc $str:n$ $nid$ with [ $runtime_exn_error ("Looking for key "^n)$ ] >>
					) pids fields in
			<:expr< match $id$ with 
				[ V.Dict $npid$ -> let $biAnd_of_list bindings$ in object $crSem_of_list exprs$ end | $runtime_error "Dict(_)"$ ]
			>>

		| <:ctyp< $t$ -> $u$ >> ->
			<:expr< match $id$ with
				[ V.Arrow f -> (let module M = Marshal in M.from_string f : $t$ -> $u$) | $runtime_error "Marshal"$ ]
			>>

		| <:ctyp< $lid:t$ >> ->
			if not (List.mem t names) then
				<:expr< $expr_of_value _loc t$ $id$ >>
			else
				let nid, npid = new_id _loc in
				<:expr< match $id$ with [
				  V.Var (n, __id__) -> Lazy.force (List.assoc __id__ __env__.Deps.$lid:t$)
				| V.Rec ((n, __id__), $npid$ ) ->
					let __env__  = { (__env__) with Deps.$lid:t$ = [ (__id__, lazy ($expr_of_value_aux _loc t$ __env__ $id$)) :: __env__.Deps.$lid:t$ ] } in
					Lazy.force (List.assoc __id__ __env__.Deps.$lid:t$)
				| _ -> $error "Var/Rec"$ ] >>

		| _ -> raise (Type_not_supported ctyp)

	let gen_one names name ctyp =
		let _loc = loc_of_ctyp ctyp in
		let id, pid = new_id _loc in
		<:binding< $patt_of_value_aux _loc name$ = fun __env__ -> fun $pid$ ->
			let module V = Value in
			$create names id ctyp$
		>>

	let gen tds =
		let _loc = loc_of_ctyp tds in
		let ids, ctyps = List.split (list_of_ctyp_decl tds) in
		let bindings = List.map2 (gen_one ids) ids ctyps in
		biAnd_of_list bindings


	let inputs _loc ids =
		patt_tuple_of_list _loc (List.map (fun x -> <:patt< ($patt_of_value _loc x$ : Value.t -> $lid:x$) >>) ids)

	let outputs _loc ids =
		expr_tuple_of_list _loc (List.map (fun x -> <:expr< $expr_of_value_aux _loc x$ $empty_env _loc ids$ >>) ids)
end


let gen tds =
	let _loc = loc_of_ctyp tds in
	let ids, _ = List.split (list_of_ctyp_decl tds) in
	<:str_item<
		value $Value_of.inputs _loc ids$ =
			let module Deps = struct
				$P4_weakid.gen tds$;
				type env = $Value_of.env_type _loc ids$;
			end in
			let rec $Value_of.gen tds$ in
			$Value_of.outputs _loc ids$;
		value $Of_value.inputs _loc ids$ =
			let module Deps = struct
				$P4_weakid.gen tds$;
				type env = $Of_value.env_type _loc ids$;
				exception Runtime_error of (string * Value.t);
				exception Runtime_exn_error of (string * exn);
			end in
			let rec $Of_value.gen tds$ in
			$Of_value.outputs _loc ids$;
	>>


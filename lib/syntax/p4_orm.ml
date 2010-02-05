(*pp camlp4orf *)
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

open Camlp4
open PreCast
open Ast

open P4_utils

let init n   = n ^ "_init"
let initRO n = n ^ "_init_read_only"
let save n   = n ^ "_save"
let get n    = n ^ "_get"
let delete n = n ^ "_delete"
let id n     = n ^ "_id"
let cache n  = n ^ "_cache"

let env_to_env _loc env =
	let sl_of_sl sl = 
		expr_list_of_list _loc (List.map (fun s -> <:expr< $str:s$ >>) sl) in
	let aux = function
		| `Unique l -> <:expr< `Unique $expr_list_of_list _loc (List.map (fun (x,y) -> <:expr< ($str:x$, $sl_of_sl y$) >>) l)$ >>
		| `Index l  -> <:expr< `Index $expr_list_of_list _loc (List.map (fun (x,y) -> <:expr< ($str:x$, $sl_of_sl y$) >>) l)$ >>
		| `Debug l  -> <:expr< `Debug $sl_of_sl l$ >>
		| `Dot f    -> <:expr< `Dot $str:f$ >> in
	expr_list_of_list _loc (List.map aux env)

let init_binding env tds (_loc, n, t) =
	<:binding< $lid:init n$ =
	fun __file__ ->
		let __name__ = Db.name_of_file __file__ in
		let __db__ = OB.new_state __name__ in
		let () = OI.init_tables ~mode:`RW ~env:__env__ ~db:__db__ $lid:P4_type.type_of n$ in
		__db__
	>>

let initRO_binding env tds (_loc, n, t) =
	<:binding< $lid:initRO n$ =
	fun __file__ ->
		let db = OB.new_state (Db.name_of_file __file__) in
		let () = OI.init_tables ~mode:`RO ~env:__env__ ~db $lid:P4_type.type_of n$ in
		db
	>>

let save_binding env tds (_loc, n, t) =
	<:binding< $lid:save n$ =
	let __get_id__ __name__ =
		let __db__ = OB.new_state __name__ in
		fun __v__ ->
			let _ = Printf.printf "==> get_id(%s)\n%!" __name__ in
			if OC.mem __env__ $lid:cache n$ __name__ __v__ then
				OC.to_weakid __env__ $lid:cache n$ __name__ __v__
			else (
				let __id__ = OS.empty_row ~env:__env__ ~db:__db__ $str:n$ in
				do { OC.add __env__ $lid:cache n$ __name__ __v__ __id__; __id__ }
			) in
	let () = $lid:P4_value.set_new_id n$ __get_id__ in
	if Type.is_mutable $lid:P4_type.type_of n$ then (
		fun ~db: __db__ ->
			fun $lid:n$ ->
				let _ = Printf.printf "I1\n%!" in
				let v = $lid:P4_value.value_of n$ ~key:__db__.OB.name $lid:n$ in
				let _ = Printf.printf "I2(%s)\n%!" (Value.to_string v) in
				OS.update_value ~env:__env__ ~db:__db__ v
    ) else (
		fun ~db:__db__ ->
			fun $lid:n$ ->
				if not (OC.mem __env__ $lid:cache n$ __db__.OB.name $lid:n$) then (
					let v = $lid:P4_value.value_of n$ ~key:__db__.OB.name $lid:n$ in
					OS.update_value ~env:__env__ ~db:__db__ v
				) else ()
    )
	>> 

module Get = struct
	(* This type is computed at preprocessing time, so no information is available on the external type variabales *)
	let pp_type_of _loc tds name =
		let tys = P4_type.create tds in
		let _, _, t = List.find (fun (_, n, _) -> n = name) tys in
		t

	let map_type fn t =
		let module T = Type in
		let rec aux name accu t =
			(match fn name t with None -> [] | Some r -> [r])
			@ match t with
				| T.Unit | T.Sum _ | T.Var _ | T.Arrow _
				| T.Bool | T.Float | T.Char | T.String | T.Int _ | T.Option _ | T.Enum _
					-> accu
				| T.Dict d when name = []
					-> List.fold_left (fun accu (n,_,t) -> aux [n] accu t) accu d
				| T.Dict _  -> accu
				| T.Tuple t when name = []
					-> fst (List.fold_left (fun (accu, i) t -> aux ["value"; string_of_int i] accu t, i+1) (accu, 1) t)
				| T.Tuple t -> fst (List.fold_left (fun (accu, i) t -> aux (name @ [string_of_int i]) accu t, i+1) (accu, 1) t)
				| T.Rec (n,t) | T.Ext (n,t) when name = []
					-> aux [] accu t
				| T.Rec (n,t) | T.Ext (n,t)
					-> accu in
		aux [] [] t

	let arg_names_of_type t =
		let module T = Type in
		let fn name = function
			| T.Bool | T.Float | T.Char | T.String | T.Int _  -> Some (if name = [] then "value" else String.concat "_" name)
			| _ -> None in
		map_type fn t

	let fun_of_type _loc t body =
		List.fold_left (fun accu n -> <:expr< fun ? $lid:n$ -> $accu$ >>) body (arg_names_of_type t)

	let ctyp_of_arg _loc t =
		let module T = Type in
		let int_like t =
			<:ctyp< [= `Eq of $lid:t$ | `Neq of $lid:t$ | `Le of $lid:t$ | `Ge of $lid:t$ | `Leq of $lid:t$ | `Geq of $lid:t$ ] >> in
		let fn _ = function
			| T.Bool     -> Some (<:ctyp< [= `True | `False ] >>)
			| T.Float    -> Some (int_like "float")
			| T.Char     -> Some (int_like "char")
			| T.String   -> Some (<:ctyp< [=`Eq of string | `Contains of string ] >>)
			| T.Int (Some i) when i + 1 = Sys.word_size -> Some (int_like "int")
			| T.Int (Some i) when i <= 32               -> Some (int_like "int32")
			| T.Int (Some i) when i <= 64               -> Some (int_like "int64")
			| T.Int _    -> Some (int_like "Big_int.big_int")
			| _          -> None in
		map_type fn t

	let sig_of_type _loc t body =
		List.fold_left2
			(fun accu n ctyp -> <:ctyp< ? $lid:n$ : $ctyp$ -> $accu$ >> )
			body
			(arg_names_of_type t)
			(ctyp_of_arg _loc t)

	let fun_of_name _loc tds n body =
		let t = pp_type_of _loc tds n in
		fun_of_type _loc t body

	let sig_of_name _loc tds n body =
		let t = pp_type_of _loc tds n in
		sig_of_type _loc t body

	let constraints_of_args _loc tds n =
		let t = pp_type_of _loc tds n in
		let make name str =
			let name_str = match name with [] -> "value" | l -> String.concat "_" l in
			let name_lst = expr_list_of_list _loc (List.map (fun s -> <:expr< $str:s$ >>) name) in
			<:expr< match $lid:name_str$ with [ None -> [] | Some x -> [ ($name_lst$, ` $uid:str$ x) ] ] >> in
		let module T = Type in
		let fn name = function
			| T.Bool     -> Some (make name "Bool")
			| T.Float    -> Some (make name "Float")
			| T.Char     -> Some (make name "Char")
			| T.Int (Some i) when i + 1 = Sys.word_size -> Some (make name "Int")
			| T.Int (Some i) when i <= 32               -> Some (make name "Int32")
			| T.Int (Some i) when i <= 64               -> Some (make name "Int64")
			| T.Int _    -> Some (make name "Big_int")
			| T.String   -> Some (make name "String")
			| _          -> None in
		List.fold_left (fun accu expr -> <:expr< $expr$ @ $accu$ >>) <:expr< [] >> (map_type fn t)
end

let get_binding env tds (_loc, n, t) =
	<:binding< $lid:get n$ = $Get.fun_of_name _loc tds n <:expr<
    let __constraints__ = $Get.constraints_of_args _loc tds n$ in
    if Type.is_mutable $lid:P4_type.type_of n$ then (
		fun __db__ ->
			List.map
				(fun (__id__, __v__) ->
					let __n__ = $lid:P4_value.of_value n$ __v__ in
					do { OC.add __env__ $lid:cache n$ __db__.OB.name __n__ __id__; __n__ }
				) (OG.get_values ~env:__env__ ~db:__db__ ~constraints:__constraints__ $lid:P4_type.type_of n$)
    ) else (
		fun __db__ ->
			List.map
				(fun (__id__, __v__) ->
					 if OC.mem_weakid __env__ $lid:cache n$ __db__.OB.name __id__ then (
						 let __n__ = List.hd (OC.of_weakid __env__ $lid:cache n$ __db__.OB.name __id__) in
						 __n__
					 ) else (
						 let __n__ = $lid:P4_value.of_value n$ __v__ in
						 do { OC.replace __env__ $lid:cache n$ __db__.OB.name __n__ __id__; __n__ } )
				) (OG.get_values ~env:__env__ ~db:__db__ ~constraints:__constraints__ $lid:P4_type.type_of n$)
	) >>$
	>>

let delete_binding env tds (_loc, n, t) =
	<:binding< $lid:delete n$ =
    fun ~db:__db__ ->
		fun __n__ ->
			let __id__ = OC.to_weakid __env__ $lid:cache n$ __db__.OB.name __n__ in
			let () = OD.delete_value ~env:__env__ ~db:__db__ ~id:__id__ ($lid:P4_value.value_of n$ ~key:__db__.OB.name __n__) in
			let () = OC.remove __env__ $lid:cache n$ __db__.OB.name __n__ in
			()
	>>

let id_binding env tds (_loc, n, t) =
	<:binding< $lid:id n$ =
	fun ~db:__db__ ->
		fun __n__ ->
			OC.to_weakid __env__ $lid:cache n$ __db__.OB.name __n__
	>>

let cache_binding env tds (_loc, n, t) =
	<:binding< $lid:cache n$ = Cache.$uid:String.capitalize n$.create $str:n$ >>

let cache_module env tds (_loc, n, t) =
	<:str_item<
		module $uid:String.capitalize n$ = OC.Make(
			struct
				type __t__ = $lid:n$;
				type t = __t__;
				value equal = (==);
				value hash = $lid:P4_hash.hash_of n$;
			end)
	>>

let gen env tds =
	let _loc = loc_of_ctyp tds in

	let ts = list_of_ctyp_decl tds in
	let init_bindings = List.map (init_binding env tds) ts in
	let initRO_bindings = List.map (initRO_binding env tds) ts in
	let save_bindings = List.map (save_binding env tds) ts in
	let get_bindings = List.map (get_binding env tds) ts in
	let delete_bindings = List.map (delete_binding env tds) ts in
	let id_bindings = List.map (id_binding env tds) ts in
	let cache_bindings = List.map (cache_binding env tds) ts in
	let cache_modules = List.map (cache_module env tds) ts in

	let patts = 
		  List.map (fun (_,n,_) -> <:patt< $lid:init n$ >>) ts
		@ List.map (fun (_,n,_) -> <:patt< $lid:initRO n$ >>) ts
		@ List.map (fun (_,n,_) -> <:patt< $lid:save n$ >>) ts
		@ List.map (fun (_,n,_) -> <:patt< $lid:get n$ >>) ts
		@ List.map (fun (_,n,_) -> <:patt< $lid:delete n$ >>) ts
		@ List.map (fun (_,n,_) -> <:patt< $lid:id n$ >>) ts in

	let exprs =
		  List.map (fun (_,n,_) -> <:expr< ( $lid:init n$ : Db.file -> Db.t $lid:n$ [=`RW] ) >>) ts
		@ List.map (fun (_,n,_) -> <:expr< ( $lid:initRO n$ : Db.file -> Db.t $lid:n$ [=`RO] ) >>) ts
		@ List.map (fun (_,n,_) -> <:expr< ( $lid:save n$ : ~db:(Db.t $lid:n$ [=`RW]) -> $lid:n$ -> unit ) >>) ts
		@ List.map (fun (_,n,_) -> <:expr< ( $lid:get n$ : $Get.sig_of_name _loc tds n <:ctyp< (Db.t $lid:n$ [<`RW|`RO]) -> list $lid:n$ >>$ ) >>) ts
		@ List.map (fun (_,n,_) -> <:expr< ( $lid:delete n$ : ~db:(Db.t $lid:n$ [=`RW]) -> $lid:n$ -> unit ) >>) ts
		@ List.map (fun (_,n,_) -> <:expr< ( $lid:id n$ : ~db:(Db.t $lid:n$ [<`RW|`RO]) -> $lid:n$ -> int64 ) >>) ts in

	<:str_item<
		$P4_hash.gen tds$;
		$P4_type.gen tds$;
		$P4_value.gen_with_key tds$;
		value $patt_tuple_of_list _loc patts$ =
			let module OB = Orm.Sql_backend in
			let module OC = Orm.Sql_cache in
			let module OI = Orm.Sql_init in
			let module OG = Orm.Sql_get in
			let module OS = Orm.Sql_save in
			let module OD = Orm.Sql_delete in
			let module Db = Orm.Db in
			let module Cache = struct
				$stSem_of_list cache_modules$
			end in
			let __env__ = $env_to_env _loc env$ in
			let $biAnd_of_list cache_bindings$ in
			let $biAnd_of_list init_bindings$ in
			let $biAnd_of_list initRO_bindings$ in
			let rec $biAnd_of_list save_bindings$ in
			let rec $biAnd_of_list get_bindings$ in
			let $biAnd_of_list delete_bindings$ in
			let $biAnd_of_list id_bindings$ in
			$expr_tuple_of_list _loc exprs$
	>>

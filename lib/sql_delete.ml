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

open Printf
open Sqlite3
open Sql_backend
open Value

let exec_sql ~env ~db = exec_sql ~tag:"delete" ~db ~env

let foreign_ids ~env ~db (table, id) =

	let aux accu (parent, field) =
		let sql = sprintf "SELECT * FROM %s WHERE %s=?;" parent field in
		let ids = exec_sql ~env ~db sql [ Data.INT id ] (fun stmt -> step_map db stmt (fun stmt -> column stmt 0)) in
		List.fold_left (fun accu -> function Data.INT id -> (parent, id) :: accu | _ -> accu) accu  ids in

	let all_parents =
		let sql = "SELECT parent, field FROM __links__ WHERE child=?;" in
		let fn stmt = let r = row_data stmt in match r.(0), r.(1) with
			| Data.TEXT p, Data.TEXT f -> (p, f)
			| _                        -> failwith "is_referenced" in
		exec_sql ~env ~db sql [Data.TEXT table] (fun stmt -> step_map db stmt fn) in

	List.fold_left aux [] all_parents

let list_union l1 l2 =
	List.fold_left (fun accu elt -> if List.mem elt l1 then accu else elt :: accu) l1 l2

let get_ids ~env ~db var v =
	let rec mem = function
		| Unit | Int _ | String _ | Bool _ | Float _ | Arrow _ | Null -> false
		| Var m      -> m=var
		| Value w    -> mem w
		| Tuple vs
		| Enum vs
		| Sum (_,vs) -> List.exists mem vs
		| Dict vs    -> List.exists (fun (_,w) -> mem w) vs
		| Ext (_,w)
		| Rec (_,w)  -> mem w in
	let rec aux name ((foreigns, internals) as accu) v =
		if not (mem v) then
			accu
		else match v with
		| Unit | Int _ | String _ | Bool _ | Float _ | Arrow _ | Null | Var _ -> accu
		| Value v         -> aux (Name.option name) accu v
		| Tuple vs        -> list_foldi (fun accu i v -> aux (Name.tuple name i) accu v) accu vs
		| Dict vs         -> List.fold_left (fun accu (n,v) -> aux (Name.dict name n) accu v) accu vs
		| Sum (r,vs)      -> list_foldi (fun accu i v -> aux (Name.sum name r i) accu v) accu vs
		| Enum vs         -> List.fold_left (aux name) accu vs
		| Ext ((n,i), w)
		| Rec ((n,i), w)  ->
			let new_foreigns = list_union (foreign_ids ~env ~db (n,i)) foreigns in
			let new_internals = list_union [ n,i ] internals in
			aux n (new_foreigns, new_internals) w in
	aux "" ([], []) (Rec (var, v))

let string_of_ids ids =
	let aux (p,i) = sprintf "(%s:%Ld)" p i in
	String.concat "; " (List.map aux ids)

let delete_value ~env ~db ~recursive v =

	let process (table, id) =
		let sql = sprintf "DELETE FROM %s WHERE __id__=?" table in
		exec_sql ~env ~db sql [ Data.INT id ] (db_must_step db) in

	let rec aux ~recurse ~deleted = function
		| Null | Unit | Int _ | Bool _ | Float _ | String _ | Arrow _ | Var _ -> ()
		| Value t      -> aux ~recurse ~deleted t
		| Ext (var, w) when recurse ->
			if not (List.mem var deleted) then begin
				let delete = List.length (foreign_ids ~env ~db var) = 0 in
				if delete then begin
					process var;
					aux ~recurse:recursive ~deleted:(var::deleted) w;
				end
			end else
				aux ~recurse:recursive ~deleted w
		| Rec (var, w) when recurse ->
			if not (List.mem var deleted) then begin
				let externals, internals = get_ids ~env ~db var w in
				let foreign_ids = List.filter (fun x -> not (List.mem x internals)) externals in
				let delete = List.length foreign_ids = 0 in
				(* Printf.printf "externals: %s\ninternals: %s\nforeigns:  %s\n%!" (string_of_ids externals) (string_of_ids internals) (string_of_ids foreign_ids); *)
				if delete then begin
					List.iter process internals;
					aux ~recurse:recursive ~deleted:(internals @ deleted) w
				end
			end else
				aux ~recurse:recursive ~deleted w
		| Sum (_,tl)
		| Tuple tl
		| Enum tl      -> List.iter (aux ~recurse ~deleted) tl
		| Dict tl      -> List.iter (fun (_,t) -> aux ~recurse ~deleted t) tl
		| Ext _ | Rec _ -> () in

	aux ~recurse:true ~deleted:[] v

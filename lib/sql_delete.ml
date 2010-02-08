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

let is_referenced ~env ~db table id =

	let exists (parent, field) =
		let sql = sprintf "SELECT * FROM %s WHERE %s=?;" parent field in
		exec_sql ~env ~db sql [ Data.INT id ]  (fun stmt -> List.length (step_map db stmt (fun stmt -> column stmt 0)) = 1) in

	let all_parents =
		let sql = "SELECT parent, field FROM __links__ WHERE child=?;" in
		let fn stmt = let r = row_data stmt in match r.(0), r.(1) with
			| Data.TEXT p, Data.TEXT f -> (p, f)
			| _                        -> failwith "is_referenced" in
		exec_sql ~env ~db sql [Data.TEXT table] (fun stmt -> step_map db stmt fn) in

	List.exists exists all_parents


let delete_value ~env ~db v =

  let process table id =
    let sql = sprintf "DELETE FROM %s WHERE __id__=?" table in
	exec_sql ~env ~db sql [ Data.INT id ] (db_must_step db) in

  let rec aux ?name v = match v, name with
    | Null, _ | Int _, _ | Bool _, _ | Float _, _ | String _, _ | Arrow _, _ | Enum _, None | Var _, _ -> ()
    | Enum t    , Some n    -> ()
    | Ext ((n,i),v), _
    | Rec ((n,i),v), _      ->
		let delete = not (is_referenced ~env ~db n i) in
		if delete then begin
			process n i;
			aux ~name:n v;
		end
    | Sum (r,tl), Some name -> list_iteri (fun i t -> aux ~name:(Name.sum name r i) t) tl
    | Dict tl   , Some name -> List.iter (fun (n,t) -> aux ~name:(Name.dict name n) t) tl
    | Tuple tl  , Some name -> list_iteri (fun i t -> aux ~name:(Name.tuple name i) t) tl
    | _                     -> failwith (Printf.sprintf "TODO:%s,%s" (to_string v) (match name with None -> "<none>" | Some n -> n)) in

  aux v

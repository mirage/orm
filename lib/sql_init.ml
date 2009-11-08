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
open Type
open Sql_backend
open Sqlite3

exception Sql_process_error of string

let create_types_table db t =
  let create = "CREATE TABLE IF NOT EXISTS __types__ (n TEXT, t TEXT)" in
  let select = "SELECT t FROM __types__ WHERE n=?" in
  let insert = "INSERT INTO __types__ (n,t) VALUES (?,?)" in

  (* Create the __types__ table *)
  db_must_ok db (fun () -> exec db.db create);
  
  (* Populate the table with the decomposition of t *)
  List.iter (fun (n, t) ->
    let stmt = prepare db.db select in
    db_must_bind db stmt 1 (Data.TEXT (Type.to_string t));
    match step_map db stmt (fun stmt -> column stmt 0) with
    | [] ->
      let stmt = prepare db.db insert in
      db_must_bind db stmt 1 (Data.TEXT n);
      db_must_bind db stmt 2 (Data.TEXT (Type.to_string t));
      db_must_step db stmt
    | [Data.TEXT x] ->
      if not (Type.is_subtype_of t (Type.of_string x)) then
	raise (Type.Subtype_error (Type.to_string t, x))
    | _ -> raise (Sql_process_error "create_types_sig")
    ) (Type.types_of t)

let create_depends_table db t =
  let create = "CREATE TABLE IF NOT EXISTS __links__ (parent TEXT, child TEXT)" in
  let select = "SELECT child FROM __links__ WHERE parent=? AND child=?" in
  let insert = "INSERT INTO __links__ (parent, child) VALUES (?,?)" in

  (* Create the __links__ table *)
  db_must_ok db (fun () -> exec db.db create);

  (* Populate the table with the links of the decomposition of t *)
  List.iter (fun (p, n) ->
    let stmt = prepare db.db select in
    db_must_bind db stmt 1 (Data.TEXT p);
    match step_map db stmt (fun stmt -> column stmt 0) with
    | [] ->
      let stmt = prepare db.db insert in
      db_must_bind db stmt 1 (Data.TEXT p);
      db_must_bind db stmt 1 (Data.TEXT n);
      db_must_step db stmt
    | [Data.TEXT x] -> ()
    | _ -> raise (Sql_process_error "create_links_table")
    ) (Type.links_of t)

(* let process_field = function
  | Unit -> "NULL"
  | Int | Int32 | Int64 | Char | Bool -> "INT"
  | Float -> "REAL"
  | String -> "TEXT"
  |  -> "INT"

let process_table = function
  | Unit -> "CREATE TABLE IF NOT EXISTS '%s' (id )"

     (* Simpler case: just add a new field to the current table *)
     Field.add ~env ~t_name ~f_name:name ~f_info:(Data (Field.data_of_t env s))

  | Product sl ->
    (* Create new tables containing the tuple values, and link them to the current table *)
    let env = Table.add ~env ~t_name:(t_name >> name) ~t_info:Normal ~t_exposed ~parent:(t_name, name) in
    foldi (fun env pos t ->
      process_sig ~env ~t_name:(t_name >> name) ~t_exposed:false ~name:(name >>> pos) t
      ) env sl

  | Collection s ->
    (* Create a new table containing an ordered collection of values, and link it to the current table *)
    let env = Table.add ~env ~t_name:(t_name >> name) ~t_info:List ~t_exposed ~parent:(t_name, name) in 
    let env = Field.add_list_counter ~env ~t_name:(t_name >> name) in
    process_sig ~env ~t_name:(t_name >> name) ~t_exposed:false ~name s

  | Named_product sl ->
    (* Create a new table containing the fields' values, and link them to the current table *)
    let env = Table.add ~env ~t_name:(t_name >> name) ~t_info:Normal ~t_exposed ~parent:(t_name, name) in
    List.fold_left (fun env (field_name, is_mutable, s) ->
      process_sig ~env ~t_name:(t_name >> name) ~t_exposed:false ~name:field_name s
      ) env sl

  | Named_sum sl ->
    (* Create a new table for each element of the sum type, and link them to the current table *)
    let env = Table.add ~env ~t_name:(t_name >> name) ~t_info:Variant ~t_exposed ~parent:(t_name, name) in
    let env = Field.add_row_name ~env ~t_name:(t_name >> name) sl in
    List.fold_left (fun env (row_name, sl) ->
      foldi (fun env pos s ->
        process_sig ~env ~t_name:(t_name >> name) ~t_exposed:false ~name:(row_name >>> pos) s
        ) env sl
      ) env sl

  | Option s ->
    let env = process_sig ~env ~t_name ~t_exposed ~name s in
    Field.set_optional ~env ~t_name ~f_name:name

  | Rec (v, s) ->
    Env.with_table_alias ~env ~t_name:v ~alias:(t_name >> name)
      (fun env -> process_sig ~env ~t_name ~t_exposed ~name s)

  | Var v      ->
    if Env.mem_alias env v then
	  Field.add_foreign_internal ~env ~t_name ~f_name:name ~foreign:(Env.find_alias env v)
    else
      Field.add_foreign_external ~env ~t_name ~f_name:name ~foreign:v

let process env =
  List.fold_left
    (fun env (_, name, s) -> process_sig ~env ~t_name:Env.toplevel_table_name ~t_exposed:true ~name s)
    env env.e_type
*)

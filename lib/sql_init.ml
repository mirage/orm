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

exception Sql_process_error of Type.t * string

let process_error t s =
  Printf.printf "ERROR(%s): %s\n%!" s (to_string t);
  raise (Sql_process_error (t,s))

let init_and_check_types_table ~mode ~env ~db tables =
  let create = "CREATE TABLE IF NOT EXISTS __types__ (n TEXT, t TEXT)" in
  let select = "SELECT t FROM __types__ WHERE n=?" in
  let insert = "INSERT INTO __types__ (n,t) VALUES (?,?)" in

  (* Create the __types__ table *)
  if mode = `RW then begin
    debug env `Sql "init" create;
    db_must_ok db (fun () -> exec db.db create);
  end;
  
  (* Insert the type t of table n into __types__ *)
  let process (n, t) =
    debug env `Sql "init" select;
    let stmt = prepare db.db select in
    debug env `Bind "init" n;
    db_must_bind db stmt 1 (Data.TEXT n);
    match step_map db stmt (fun stmt -> column stmt 0) with
    | [] when mode = `RW ->
      debug env `Sql "init" insert;
      let stmt = prepare db.db insert in
      debug env `Bind "init" n;
      db_must_bind db stmt 1 (Data.TEXT n);
      debug env `Bind "init" (Type.to_string t);
      db_must_bind db stmt 2 (Data.TEXT (Type.to_string t));
      db_must_step db stmt
    | [] -> ()
    | [Data.TEXT x] ->
      if not (t <: (Type.of_string x)) then begin
        Printf.printf "%s\n%!" (string_of_last_type_error ());
        raise (Type.Subtype_error (Type.to_string t, x));
      end else if mode = `RW && not (Type.of_string x <: t) then begin
        Printf.printf "%s\n%!" (string_of_last_type_error ());
        raise (Type.Subtype_error (x, Type.to_string t));
      end
    | _ -> process_error t "create_types_table:1" in
  
  List.iter process tables

let init_links_table ~mode ~env ~db t table_links =
  let create = "CREATE TABLE IF NOT EXISTS __links__ (parent TEXT, child TEXT)" in
  let select = "SELECT child FROM __links__ WHERE parent=? AND child=?" in
  let insert = "INSERT INTO __links__ (parent, child) VALUES (?,?)" in

  (* Create the __links__ table *)
  if mode = `RW then begin
    debug env `Sql "init" create;
    db_must_ok db (fun () -> exec db.db create);
  end;

  (* Insert the link 'p is a parent of n' into __links__ *)
  let process (p, n) =
    debug env `Sql "init" select;
    let stmt = prepare db.db select in
    db_must_bind db stmt 1 (Data.TEXT p);
    begin match step_map db stmt (fun stmt -> column stmt 0) with
    | [] when mode = `RW ->
      debug env `Sql "init" insert;
      let stmt = prepare db.db insert in
      debug env `Bind "init" p;
      db_must_bind db stmt 1 (Data.TEXT p);
      debug env `Bind "init" n;
      db_must_bind db stmt 1 (Data.TEXT n);
      db_must_step db stmt
    | [] -> ()
    | [Data.TEXT x] -> ()
    | _ -> process_error t "create_links_table:1"
    end in

  List.iter process table_links

let create_tables ~mode ~env ~db tables =

  let process (name, t) =
    let t_internal = if is_enum t then get_enum_type t else get_internal_type t in
    let field_names = field_names_of_type ~id:false t_internal in
    let field_types = field_types_of_type ~id:false t_internal in
    let fields = List.map2 (sprintf "%s %s") field_names field_types in
    let extra = if is_enum t then "__next__ INTEGER,__size__ INTEGER," else "" in
    let sql =
      sprintf "CREATE TABLE IF NOT EXISTS %s (__id__ INTEGER PRIMARY KEY AUTOINCREMENT, %s%s);" name extra (String.concat "," fields) in
    debug env `Sql "init" sql;
	db_must_ok db (fun () -> exec db.db sql);
    if is_enum t then begin
      let sql = sprintf "CREATE UNIQUE INDEX IF NOT EXISTS idx_%s_enum ON %s (__id__,__next__);" name name in
      debug env `Sql "init" sql;
      db_must_ok db (fun () -> exec db.db sql)
    end in

  if mode = `RW then List.iter process tables

let init_custom_indexes ~mode ~env ~db tables =
  let process kind (t, fs) =
    if List.mem_assoc t tables then begin
      let sql = match kind with
        | `U -> sprintf "CREATE UNIQUE INDEX IF NOT EXISTS idx_%s_%s ON %s (%s);" t (String.concat "_" fs) t (String.concat "," fs)
        | `I -> sprintf "CREATE INDEX IF NOT EXISTS idx_%s_%s ON %s (%s);" t (String.concat "_" fs) t (String.concat "," fs) in
       debug env `Sql "init" sql;
       db_must_ok db (fun () -> exec db.db sql)
    end in

  (* Process indices *)
  if mode = `RW then begin
    List.iter (function 
      | `Unique l -> List.iter (process `U) l
      | `Index l  -> List.iter (process `I) l
	  | _         -> ()
      ) env;
  end

let init_tables ~mode ~env ~db t =
  let tables, table_links = subtables_of_type t in
  init_and_check_types_table ~mode ~env ~db tables;
  init_links_table ~mode ~env ~db t table_links;
  create_tables ~mode ~env ~db tables;
  init_custom_indexes ~mode ~env ~db tables

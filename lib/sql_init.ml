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
  Printf.printf "ERROR(%s): %s\n%!" s (Type.to_string t);
  raise (Sql_process_error (t,s))

let list n = sprintf "%s__list" n
let tuple n i = sprintf "%s__%i" n i
let row n r i = sprintf "%s__%s__%i" n r i



let create_types_table env db t =
  let create = "CREATE TABLE IF NOT EXISTS __types__ (n TEXT, t TEXT)" in
  let select = "SELECT t FROM __types__ WHERE n=?" in
  let insert = "INSERT INTO __types__ (n,t) VALUES (?,?)" in

  (* Create the __types__ table *)
  debug env `Sql "init" create;
  db_must_ok db (fun () -> exec db.db create);
  
  (* Insert the type t of table n into __types__ *)
  let rec process (n, t) =
    debug env `Sql "init" select;
    let stmt = prepare db.db select in
    debug env `Bind "init" n;
    db_must_bind db stmt 1 (Data.TEXT n);
    begin match step_map db stmt (fun stmt -> column stmt 0) with
    | [] ->
      debug env `Sql "init" insert;
      let stmt = prepare db.db insert in
      debug env `Bind "init" n;
      db_must_bind db stmt 1 (Data.TEXT n);
      debug env `Bind "init" (Type.to_string t);
      db_must_bind db stmt 2 (Data.TEXT (Type.to_string t));
      db_must_step db stmt
    | [Data.TEXT x] ->
      if not (Type.is_subtype_of t (Type.of_string x)) then
        raise (Type.Subtype_error (Type.to_string t, x))
    | _ -> process_error t "create_types_table:1"
    end;
    aux ~n t
  
  (* Call process for any sub-table of t *)
  and aux ?n t = match t with
    | Type.Ext (n, t) | Type.Rec (n, t) -> process (n, t)
    | Type.Enum t ->
      begin match n with
      | None -> process_error t "create_types_table:2"
      | Some n -> process (list n, t)
      end
    | _ -> () in

  aux t

let create_links_table env db t =
  let create = "CREATE TABLE IF NOT EXISTS __links__ (parent TEXT, child TEXT)" in
  let select = "SELECT child FROM __links__ WHERE parent=? AND child=?" in
  let insert = "INSERT INTO __links__ (parent, child) VALUES (?,?)" in

  (* Create the __links__ table *)
  debug env `Sql "init" create;
  db_must_ok db (fun () -> exec db.db create);

  (* Insert the link 'p is a parent of n' into __links__ *)
  let rec process p (n, t) =
    debug env `Sql "init" select;
    let stmt = prepare db.db select in
    db_must_bind db stmt 1 (Data.TEXT p);
    begin match step_map db stmt (fun stmt -> column stmt 0) with
    | [] ->
      debug env `Sql "init" insert;
      let stmt = prepare db.db insert in
      debug env `Bind "init" p;
      db_must_bind db stmt 1 (Data.TEXT p);
      debug env `Bind "init" n;
      db_must_bind db stmt 1 (Data.TEXT n);
      db_must_step db stmt
    | [Data.TEXT x] -> ()
    | _ -> process_error t "create_links_table:1"
    end;
    aux ~p:n t

  (* Call process for any pair of sub-tables of t *)
  and aux ?p t = match t with 
    | Type.Ext (n, t) | Type.Rec (n, t) ->
       begin match p with
       | None -> aux ~p:n t
       | Some p -> process p (n, t)
       end
    | Type.Enum t ->
      begin match p with
      | None -> process_error t "create_links_table:2"
      | Some p -> process p (list p, t)
      end;
    | _ -> () in

  aux t

let foldi fn accu l =
  let accu, _ = List.fold_left (fun (accu, i) x -> fn accu i x, i + 1) (accu, 0) l in accu

let mapi fn l = foldi (fun accu i x -> fn i x :: accu) [] l
let map_strings sep fn sl = String.concat sep (List.map fn sl)
let map_stringsi sep fn sl = String.concat sep (mapi fn sl)

let init_tables ~mode ~env ~db t =
  let sub_tables = ref [] in
  let process t s =
    let sql = sprintf "CREATE TABLE IF NOT EXISTS %s (__id__ INTEGER PRIMARY KEY AUTOINCREMENT, %s)" t s in
	sub_tables := t :: !sub_tables;
    debug env `Sql "init" sql;
	db_must_ok db (fun () -> exec db.db sql) in

  let rec field name = function 
    | Unit | Int | Int32 | Int64 | Char | Bool
    | Var _ | Rec _ | Ext _
               -> sprintf "%s INTEGER" name
    | Enum t   -> table ~name (Enum t); sprintf "%s INT" name
    | Float    -> sprintf "%s REAL" name
    | String   -> sprintf "%s TEXT" name
    | Arrow _  -> sprintf "%s BLOB" name
    | Tuple tl -> map_stringsi ", " (fun i t -> table ~name t; field (tuple name i) t) tl
    | Dict tl  -> map_strings ", " (fun (m,_,t) -> table ~name t; field m t) tl
    | Sum tl   -> map_strings "__row__ TEXT, %s" (fun (r, tl) -> map_stringsi "," (fun i t -> table ~name t; field (row name r i) t) tl) tl
    | Option t -> field name t

  and table ?name t = match t with
    | Type.Ext (n, t) | Type.Rec (n, t) -> process n (field n t)
    | Type.Enum t ->
      begin match name with
      | None   -> process_error t "init_tables:1"
      | Some n -> process n (sprintf "__idx__ INT, PRIMARY KEY (__id__, __idx___), %s" (field n t))
      end;
    | _ -> () in

  (* Create all the sub-table of t*)
  table t;
  create_types_table env db t;
  create_links_table env db t;

  let process_custom_index kind (t, fs) =
    if List.mem t !sub_tables then begin
        let sql = match kind with
        | `U -> sprintf "CREATE UNIQUE INDEX IF NOT EXISTS idx_%s_%s ON %s (%s);" t (String.concat "_" fs) t (String.concat "," fs)
        | `I -> sprintf "CREATE INDEX IF NOT EXISTS idx_%s_%s ON %s (%s);" t (String.concat "_" fs) t (String.concat "," fs) in
        debug env `Sql "init" sql;
        db_must_ok db (fun () -> exec db.db sql)
	end in

  (* Process indices *)
  List.iter (function 
    | `Unique l -> List.iter (process_custom_index `U) l
    | `Index l  -> List.iter (process_custom_index `I) l
	| _         -> ()
    ) env

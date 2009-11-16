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

exception Sql_process_error of Value.t * string

let process_error t s =
  Printf.printf "ERROR(%s): %s\n%!" s (to_string t);
  raise (Sql_process_error (t,s))

(* Build up the list of fields actually needed to save the row *)
let rec field_names name = function
  | Null | Int _ | String _ | Bool _ | Float _ | Var _ | Rec _ | Ext _ | Enum _ | Arrow _
             -> [ name ]
  | Tuple tl   -> list_foldi (fun accu i t -> accu @ field_names (Sql_init.tuple name i) t) [] tl
  | Dict tl    -> List.fold_left (fun accu (n,t) -> accu @ field_names (Sql_init.dict name n) t) [] tl
  | Sum (r,tl) -> "__row__" :: list_foldi (fun accu i t -> accu @ field_names (Sql_init.sum name r i) t) [] tl

let save_value ~env ~db ?id (t : Value.t) =

  (* Insert/update a specific row in a specific table *)
  let process_row ?id table_name field_names field_values =
    let op_name, id_str, field_values = match id with
      | None    -> "INSERT", "", field_values
      | Some id -> "UPDATE", " WHERE __id__=?", field_values @ [ Data.INT id ] in
    let qmarks = List.map (fun _ -> "?") field_names in
    let sql = sprintf "%s INTO %s (%s) VALUES (%s)%s"
      op_name table_name
      (String.concat "," field_names)
      (String.concat "," qmarks)
      id_str in
    debug env `Sql "save" sql;
    let stmt = prepare db.db sql in
    list_iteri (fun i v ->
      debug env `Bind "save" (string_of_data v);
      db_must_bind db stmt (i+1) v
      ) field_values;
    db_must_step db stmt;
    match id with
    | None    -> last_insert_rowid db.db 
    | Some id -> id
    in

  (* Build up the list of values which are composing the row *)
  let ids = ref [] in
  let rec field_values ?(nullforeign=false) name = function
  | Null          -> [ Data.INT 0L ]
  | Int i         -> [ Data.INT i ]
  | Bool b        -> if b then [ Data.INT 1L ] else [ Data.INT 0L ]
  | Float f       -> [ Data.FLOAT f ]
  | String s      -> [ Data.TEXT s ]
  | Arrow a       -> [ Data.BLOB a ]
  | Enum tl       -> let id = save (Sql_init.enum name) (Enum tl) in [ Data.INT id ]
  | Tuple tl      -> list_foldi (fun accu i t -> accu @ field_values (Sql_init.tuple name i) t) [] tl
  | Dict tl       -> List.fold_left (fun accu (n,t) -> accu @ field_values (Sql_init.dict name n) t) [] tl
  | Sum (r,tl)    -> Data.TEXT r :: list_foldi (fun accu i t -> accu @ field_values (Sql_init.sum name r i) t) [] tl
  | Var (n,i)     when nullforeign -> [ Data.INT 0L ]
  | Rec ((n,i),t) when nullforeign -> [ Data.INT 0L ]
  | Ext ((n,i),t) when nullforeign -> [ Data.INT 0L ]
  | Var (n,i)     -> [ Data.INT (List.assoc i !ids) ]
  | Rec ((n,i),t) -> let id = save n t in ids := (i, id) :: !ids; [ Data.INT id ]
  | Ext ((n,i),t) -> let id = save n t in [ Data.INT id ]

  (* Recursively save all the sub-rows in the dabatabse *)
  and save ?id n = function
  | Null | Int _ | String _ | Bool _ | Float _ | Var _ | Arrow _ as f
                  -> process_row n (field_names n f) (field_values n f)
  | Enum t        -> process_error (Enum t) "TODO" (* begin match id with None -> assert false | Some id -> process n ("__idx__" :: field_names n t) (id :: field_values n t) end *)
  | Rec ((n,i),t) ->
    let id = process_row n (field_names n t) (field_values ~nullforeign:true n t) in
    process_row n ~id (field_names n t) (field_values n t)
  | Ext ((n,i),t) -> process_row n (field_names n t) (field_values n t)
  | Sum _ | Dict _ | Tuple _ as f
                  -> process_error f "save_value:1"
 in

  save "" t

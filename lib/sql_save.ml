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

let save_value ~env ~db ?id (t : Value.t) =

  (* Insert/update a specific row in a specific table *)
  let process_row ?id table_name field_names field_values =
    let sql, binds = match id with
    | None    ->
      let qmarks = List.map (fun _ -> "?") field_names in
      sprintf "INSERT INTO %s (%s) VALUES (%s)" table_name (String.concat "," field_names) (String.concat "," qmarks),
      field_values
    | Some id ->
      let fields = List.map (fun n -> sprintf "%s=?" n) field_names in
      sprintf "UPDATE %s SET %s WHERE __id__=?" table_name (String.concat "," fields),
      ( field_values @ [ Data.INT id ] ) in
    debug env `Sql "save" sql;
    let stmt = prepare db.db sql in
    list_iteri (fun i v ->
      debug env `Bind "save" (string_of_data v);
      db_must_bind db stmt (i+1) v
      ) binds;
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
  | Enum tl       -> let id = save (Name.enum_table name) (Enum tl) in [ Data.INT id ]
  | Tuple tl      -> list_foldi (fun accu i t -> accu @ field_values (Name.tuple_field name i) t) [] tl
  | Dict tl       -> List.fold_left (fun accu (n,t) -> accu @ field_values (Name.dict_field name n) t) [] tl
  | Sum (r,tl)    -> Data.TEXT r :: list_foldi (fun accu i t -> accu @ field_values (Name.sum_field name r i) t) [] tl
  | Var (n,i)     when nullforeign -> [ Data.INT 0L ]
  | Rec ((n,i),t) when nullforeign -> [ Data.INT 0L ]
  | Ext ((n,i),t) when nullforeign -> [ Data.INT 0L ]
  | Var (n,i)     -> [ Data.INT (List.assoc i !ids) ]
  | Rec ((n,i),t) -> let id = save n t in ids := (i, id) :: !ids; [ Data.INT id ]
  | Ext ((n,i),t) -> let id = save n t in [ Data.INT id ]

  (* Recursively save all the sub-rows in the dabatabse *)
  and save ?id n = function
  | Null | Int _ | String _ | Bool _ | Float _ | Var _ | Arrow _ as f
                  -> process_row ?id n (field_names_of_value false f) (field_values n f)
  | Enum t        -> process_error (Enum t) "TODO" (* begin match id with None -> assert false | Some id -> process n ("__idx__" :: field_names n t) (id :: field_values n t) end *)
  | Rec ((n,i),t) ->
    let field_names = field_names_of_value ~id:false t in
    let id = match id with
      | None    -> process_row ?id n field_names (field_values ~nullforeign:true n t)
      | Some id -> id in
    process_row ~id n field_names (field_values n t)
  | Ext ((n,i),t) -> process_row ?id n (field_names_of_value ~id:false t) (field_values n t)
  | Sum _ | Dict _ | Tuple _ as f
                  -> process_error f "save_value:1"
  in
  save ?id "" t


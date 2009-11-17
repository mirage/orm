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

  (* Insert a collection of rows in a specific table *)
  let process_enum_rows ?id table_name field_names field_values_enum =
	let aux field_values others =
      let id = process_row table_name field_names field_values in
      let sql = sprintf "UPDATE %s SET __idx__=%Ld WHERE __id__=%Ld" table_name id id in
      debug env `Sql "save" sql;
      db_must_ok db (fun () -> exec db.db sql);
      List.iter (fun field_values ->
        let (_:int64) = process_row table_name ("__idx__" :: field_names) (Data.INT id :: field_values) in ()
        ) others;
      id in

    match id, field_values_enum with
    | _      , []     -> failwith "TODO"
    | None   , h :: t -> aux h t
    | Some id, h :: t -> aux h t in (* TODO: need to be optimized later to reuse the elements which are equal *)

  (* Build up the list of values which are composing the row *)
  let ids = ref [] in
  let rec field_values ?(nullforeign=false) name = function
  | Null          -> [ Data.INT 0L ]
  | Value v       -> Data.INT 1L :: field_values ~nullforeign name v
  | Int i         -> [ Data.INT i ]
  | Bool b        -> if b then [ Data.INT 1L ] else [ Data.INT 0L ]
  | Float f       -> [ Data.FLOAT f ]
  | String s      -> [ Data.TEXT s ]
  | Arrow a       -> [ Data.BLOB a ]
  | Enum []       -> [ Data.NULL ]
  | Enum tl       -> let id = save name (Enum tl) in [ Data.INT id ]
  | Tuple tl      -> list_foldi (fun accu i t -> accu @ field_values (Name.tuple_field name i) t) [] tl
  | Dict tl       -> List.fold_left (fun accu (n,t) -> accu @ field_values (Name.dict_field name n) t) [] tl
  | Sum (r,tl)    -> Data.TEXT r :: list_foldi (fun accu i t -> accu @ field_values (Name.sum_field name r i) t) [] tl
  | Var (n,i)     when nullforeign -> [ Data.INT 0L ]
  | Rec ((n,i),_) when nullforeign -> [ Data.INT 0L ]
  | Ext ((n,i),_) when nullforeign -> [ Data.INT 0L ]
  | Var (n,i)     -> [ Data.INT (List.assoc i !ids) ]
  | Rec ((n,i),_) as t -> let id = save n t in ids := (i, id) :: !ids; [ Data.INT id ]
  | Ext ((n,i),_) as t -> let id = save n t in [ Data.INT id ]

  (* Recursively save all the sub-rows in the dabatabse *)
  and save ?id name = function
  | Null | Int _ | String _ | Bool _ | Float _ | Var _ | Arrow _  | Value _ as f
                  -> process_row ?id name (field_names_of_value ~id:false ~name f) (field_values name f)
  | Enum tl       -> process_enum_rows ?id name (field_names_of_value ~id:false ~name t) (List.map (field_values name) tl)
  | Rec ((n,i),t) ->
    let field_names = field_names_of_value ~id:false ~name t in
    let id = match id with
      | None    -> process_row ?id n field_names (field_values ~nullforeign:true n t)
      | Some id -> id in
    process_row ~id n field_names (field_values n t)
  | Ext ((n,i),t) -> process_row ?id n (field_names_of_value ~id:false t) (field_values n t)
  | Sum _ | Dict _ | Tuple _ as f
                  -> process_error f "save_value:1"
  in
  save ?id "" t


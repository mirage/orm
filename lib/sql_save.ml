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

let save_value ~env ~db (v : Value.t) =

  let exec_sql sql binds fn = 
    debug env `Sql "save" sql;
    let stmt = prepare db.db sql in
    list_iteri (fun i v ->
      debug env `Bind "save" (string_of_data v);
      db_must_bind db stmt (i+1) v
    ) binds;
    fn stmt in

  (* Insert/update a specific row in a specific table *)
  let process_row table_name field_names field_values =
    let qmarks = List.map (fun _ -> "?") field_names in
    let constraints =
      List.map2 (fun f v -> if v = Data.NULL then sprintf "%s ISNULL" f else sprintf "%s=?" f) field_names field_values in
    let insert = sprintf "INSERT INTO %s (%s) VALUES (%s);" table_name (String.concat "," field_names) (String.concat "," qmarks) in
    let select = sprintf "SELECT __id__ FROM %s WHERE %s;" table_name (String.concat " AND " constraints) in
    let fn stmt =  step_map db stmt (fun stmt -> column stmt 0) in
    match exec_sql select (List.filter (fun v -> v <> Data.NULL) field_values) fn with
    | [Data.INT i ] -> i
    | []            -> exec_sql insert field_values (db_must_step db); last_insert_rowid db.db
    | ds            -> process_error v (sprintf "Found {%s}" (String.concat "," (List.map string_of_data ds))) in

  let replace_row table_name id field_names field_values =
    let field_names = List.map (fun f -> sprintf "%s=?" f) field_names in
    let replace = sprintf "UPDATE %s SET %s WHERE __id__=%Ld;" table_name (String.concat "," field_names) id in
    exec_sql replace field_values (db_must_step db) in

  (* Insert a collection of rows in a specific table *)
  let process_enum_rows table_name field_names field_values_enum =
    let joints =
      sprintf "%s as __t0__" table_name ::
      list_mapi (fun i _ -> sprintf "%s AS __t%i__ ON __t%i__.__next__=__t%i__.__id__" table_name (i+1) i (i+1)) field_values_enum in
    let constraints =
      List.flatten (list_mapi (fun i _ -> List.map (fun f -> sprintf "__t%i__.%s=?" i f) field_names) field_values_enum) in
    let select = sprintf "SELECT __t0__.__id__ FROM %s WHERE __t%i__.__next__ ISNULL AND %s;"
      (String.concat " JOIN " joints)
      (List.length field_values_enum)
      (String.concat " AND " constraints) in
    match exec_sql select [] (fun stmt -> step_map db stmt (fun stmt -> column stmt 0)) with
    | [Data.INT i ] -> i
    | []            ->
      let rec aux ?last i = function
        | []                -> (match last with None -> process_error v "Empy enum" | Some id -> id)
        | field_values :: t ->
          let last = match last with None -> Data.NULL | Some id -> Data.INT id in
          let id = process_row table_name
            ("__next__" :: "__size__" :: field_names)
            (last :: Data.INT (Int64.of_int i) :: field_values) in
          aux ~last:id (i+1) t in
      aux 0 (List.rev field_values_enum)
    | ds           -> process_error v (sprintf "Found {%s}" (String.concat "," (List.map string_of_data ds))) in

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
  | Tuple tl      -> list_foldi (fun accu i t -> accu @ field_values ~nullforeign (Name.tuple_field name i) t) [] tl
  | Dict tl       -> List.fold_left (fun accu (n,t) -> accu @ field_values ~nullforeign (Name.dict_field name n) t) [] tl
  | Sum (r,tl)    -> Data.TEXT r :: list_foldi (fun accu i t -> accu @ field_values ~nullforeign (Name.sum_field name r i) t) [] tl
  | Var (n,i)     when nullforeign -> [ Data.NULL ]
  | Rec ((n,i),_) when nullforeign -> [ Data.NULL ]
  | Ext ((n,i),_) when nullforeign -> [ Data.NULL ]
  | Var (n,i)          -> [ Data.INT (List.assoc i !ids) ]
  | Rec ((n,i),_) as t -> let id = save n t in [ Data.INT id ]
  | Ext ((n,i),_) as t -> let id = save n t in [ Data.INT id ]

  (* Recursively save all the sub-rows in the dabatabse *)
  and save name = function
  | Null | Int _ | String _ | Bool _ | Float _ | Var _ | Arrow _  | Value _ as f
                  -> process_row name (field_names_of_value ~id:false f) (field_values name f)
  | Enum tl       -> process_enum_rows name (field_names_of_value ~id:false v) (List.map (field_values name) tl)
  | Rec ((n,i),s) ->
    let field_names = field_names_of_value ~id:false s in
    let id = process_row n field_names (field_values ~nullforeign:true n s) in
    ids := (i, id) :: !ids;
    replace_row n id field_names (field_values n s);
    id
  | Ext ((n,i),s) -> process_row n (field_names_of_value ~id:false s) (field_values n s)
  | Sum _ | Dict _ | Tuple _ as f
                  -> process_error f "save_value:1"
  in
  save "" v


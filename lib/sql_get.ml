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

module T = Type
module V = Value

exception Sql_process_error of T.t * Data.t * string

let process_error v d s =
  Printf.printf "ERROR(%s): %s - %s\n%!" s (T.to_string v) (string_of_data d);
  raise (Sql_process_error (v, d, s))

(* Build up the list of fields actually needed to save the row *)
let rec parse_row ~env ~db ?(skip=false) ~id ~name t row n =
  match t, row.(n) with
  | T.Unit    , Data.INT 0L -> V.Null, n + 1
  | T.Int     , Data.INT i
  | T.Int32   , Data.INT i
  | T.Int64   , Data.INT i
  | T.Char    , Data.INT i   -> V.Int i, n + 1
  | T.Bool    , Data.INT 0L  -> V.Bool false, n + 1
  | T.Bool    , Data.INT 1L  -> V.Bool true, n + 1
  | T.Float   , Data.FLOAT f -> V.Float f, n + 1
  | T.String  , Data.TEXT t  -> V.String t, n + 1
  | T.Enum t  , Data.INT idx ->
    let values = get_values ~env ~db ~idx (T.Ext (name, T.Enum t)) in
    let values = snd (List.split values) in
    V.Enum values, n + 1
  | T.Arrow _ , Data.BLOB b  -> V.Arrow b, n + 1
  | T.Option t, Data.INT r   -> let res, j = parse_row ~env ~db ~skip:(r=0L) ~id ~name t row (n + 1) in (if r=0L then V.Null else res), j
  | T.Tuple tl, _            ->
    let tuple, n = list_foldi (fun (accu, n1) i t ->
      let res, n2 = parse_row ~env ~db ~id ~name:(Name.tuple_field name i) t row n1 in res :: accu, n2
      ) ([], n) tl in
    V.Tuple (List.rev tuple), n
  | T.Dict tl , _            ->
    let dict, n = List.fold_left (fun (accu, n1) (f,_,t) ->
      let res, n2 = parse_row ~env ~db ~id ~name:(Name.dict_field name f) t row n1 in (f, res) :: accu, n2
      ) ([], n) tl in
    V.Dict (List.rev dict), n
  | T.Sum tl  , Data.TEXT r  ->
    let row, n = List.fold_left (fun (accu, n1) (rn, tl) ->
      list_foldi (fun (accu, n2) i t ->
        let res, n3 = parse_row ~skip:(rn<>r) ~env ~db ~id ~name:(Name.sum_field name rn i) t row n2 in (if rn<>r then accu else res :: accu), n3
        ) (accu, n1) tl)
      ([], n + 1) tl in
    V.Sum (r, List.rev row), n
  | T.Rec(r, s), Data.INT i  ->
    begin match get_values ~env ~db ~id:i t with
    | [_,v] -> v, n + 1
    | []    -> process_error t row.(n) "No value found"
    | _     -> process_error t row.(n) "Too many values found"
    end
  | T.Ext(e, s), Data.INT i  ->
    begin match get_values ~env ~db ~id:i t with
    | [_,v] -> v, n + 1
    | []    -> process_error t row.(n) "No value found"
    | _     -> process_error t row.(n) "Too many values found"
    end
  | T.Var v   , Data.INT i   -> V.Var (v, i), n + 1
  | _ when skip              -> V.Null, n + 1
  | _                        -> process_error t row.(n) "unknown"

and get_values ~env ~db ?id ?idx t =
  let process (name, t) =
    let t_internal = if is_enum t then get_enum_type t else t in
    let t_name = if is_enum t then name else "" in
    let field_names = field_names_of_type ~id:true ~name:t_name t_internal in
    let where =
      (match id with None -> "" | Some id -> " WHERE __id__=?") ^
      (match idx with None -> "" | Some id -> " WHERE __idx__=?") in
    let sql = sprintf "SELECT %s FROM %s%s" (String.concat "," field_names) name where in
    debug env `Sql "get" sql;
    let stmt = prepare db.db sql in
    let bind = ref 0 in
    (match id with 
      | None    -> () 
      | Some id ->
        debug env `Bind "get" (Int64.to_string id);
        db_must_bind db stmt (incr bind; !bind) (Data.INT id));
    (match idx with
      | None     -> ()
      | Some id  ->
        debug env `Bind "get" (Int64.to_string id);
        db_must_bind db stmt (incr bind; !bind) (Data.INT id)); 
    step_map db stmt (fun stmt ->
      let row = row_data stmt in
      let id = match row.(0) with Data.INT i -> i | _ -> failwith "TODO:4" in
      let r, _ = parse_row ~env ~db ~id ~name t row 1 in
      if is_enum t then
        id, r
      else if List.mem (name, id) (V.free_vars r) then
        id, V.Rec ((name,id),r)
      else
        id, V.Ext ((name,id),r))

  in
  match t with
  | T.Rec (n, t)
  | T.Ext (n, t) -> process (n, t)
  | _            -> failwith "TODO"  

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

exception Sql_process_error of Data.t * string

let process_error t s =
  Printf.printf "ERROR(%s): %s\n%!" s (string_of_data t);
  raise (Sql_process_error (t,s))

(* Build up the list of fields actually needed to save the row *)
let rec parse_row ~env ~db ?(skip=false) ~id t row n =
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
  | T.Enum t  , Data.INT i   -> failwith "TODO"
  | T.Arrow _ , Data.BLOB b  -> V.Arrow b, n + 1
  | T.Option t, Data.INT r   -> let res, j = parse_row ~env ~db ~skip:(r=0L) ~id t row (n + 1) in (if r=0L then V.Null else res), j
  | T.Tuple tl, _            ->
    let tuple, n = List.fold_left (fun (accu, i) t ->
      let res, j = parse_row ~env ~db ~id t row i in res :: accu, j
      ) ([], n) tl in
    V.Tuple (List.rev tuple), n
  | T.Dict tl , _            ->
    let dict, n = List.fold_left (fun (accu, i) (f,_,t) ->
      let res, j = parse_row ~env ~db ~id t row i in (f, res) :: accu, j
      ) ([], n) tl in
    V.Dict (List.rev dict), n
  | T.Sum tl  , Data.TEXT r  ->
    let row, n = List.fold_left (fun (accu, i) (rn, tl) ->
      List.fold_left (fun (accu, j) t ->
        let res, k = parse_row ~skip:(rn<>r) ~env ~db ~id t row i in (if rn<>r then accu else res :: accu), k
        ) (accu, i) tl)
      ([], n) tl in
    V.Sum (r, List.rev row), n
  | T.Rec(r, t), Data.INT i  ->
    begin match get_values ~env ~db ~id:i t with
    | [_,v] when List.mem (r,id) (V.free_vars v) -> V.Rec ((r, i), v), n + 1
    | [_,v]                                      -> V.Ext ((r, i), v), n + 1
    | _                                          -> failwith "TODO:1"
    end
  | T.Ext(e,t), Data.INT i   ->
    begin match get_values ~env ~db ~id:i t with
    | [_,v] -> V.Ext ((e, i), v), n + 1
    | _     -> failwith "TODO:2"
    end
  | T.Var v   , Data.INT i   -> V.Var (v, i), n + 1
  | _ when skip              -> V.Null, n + 1
  | _                        -> failwith "TODO:3"

and get_values ~env ~db ?id t =

  let process n t =
    let fields = field_names_of_type ~id:true t in
    let sql = sprintf "SELECT %s FROM %s" (String.concat "," fields) n in
    debug env `Sql "get" sql;
    let stmt = prepare db.db sql in
    (* TODO: binds *)
    step_map db stmt (fun stmt ->
      let row = row_data stmt in
      let id = match row.(0) with Data.INT i -> i | _ -> failwith "TODO:4" in
      let r, _ = parse_row ~env ~db ~id t row 1 in
      if List.mem (n, id) (V.free_vars r) then
        id, V.Rec ((n,id),r)
      else
        id, V.Ext ((n,id),r))

  in
  match t with
  | T.Rec (n,t) | T.Ext (n,t) -> process n t
  | _ -> failwith "TODO"  
  

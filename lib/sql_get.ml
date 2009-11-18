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
  | T.Enum t  , Data.INT idx -> V.Enum (get_enum_values ~env ~db ~idx name t), n + 1
  | T.Arrow _ , Data.BLOB b  -> V.Arrow b, n + 1
  | T.Option t, Data.INT r   -> let res, j = parse_row ~env ~db ~skip:(r=0L) ~id ~name t row (n + 1) in (if r=0L then V.Null else V.Value res), j
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

and process ?extra ~env ~db ~fn ~constraints name t =
  let field_names = field_names_of_type ~id:true t in
  let where_str = String.concat " AND " (List.map (function | (n,c,None) -> sprintf "%s %s" n c | (n,c,Some _) -> sprintf "%s %s ?" n c) constraints) in
  let where = if where_str = "" then "" else sprintf " WHERE %s" where_str in
  let extra = match extra with None -> "" | Some e -> sprintf " %s" e in
  let sql = sprintf "SELECT %s FROM %s%s%s" (String.concat "," field_names) name where extra in
  debug env `Sql "get" sql;
  let stmt = prepare db.db sql in
  let bind = ref 0 in
  List.iter (function 
    | (_,_,None  ) -> ()
    | (_,_,Some v) ->
        debug env `Bind "get" (string_of_data v);
        db_must_bind db stmt (incr bind; !bind) v;
    ) constraints;
  step_map db stmt fn 

and get_values ~env ~db ?id t =
  let aux name s stmt =
    let row = row_data stmt in
    let id = match row.(0) with Data.INT i -> i | _ -> failwith "TODO:4" in
    let r, _ = parse_row ~env ~db ~id ~name s row 1 in
    if List.mem (name, id) (V.free_vars r) then
      id, V.Rec ((name,id),r)
    else
      id, V.Ext ((name,id),r) in
  let constraints = match id with None -> [] | Some id -> [ "__id__", "=", Some (Data.INT id) ] in
  match t with
  | T.Rec (n, s)
  | T.Ext (n, s) -> process ~fn:(aux n s) ~env ~db ~constraints n s
  | _            -> failwith "TODO"  

and get_enum_values ~env ~db ~idx name t =
  let aux stmt =
    let row = row_data stmt in
    let id = match row.(0) with Data.INT i -> i | _ -> failwith "TODO:4" in
    let v, _ = parse_row ~env ~db ~id ~name t row 1 in
    id, v in
  let extra = "ORDER BY __id__" in
  let constraints = [ "__idx__", "=", Some (Data.INT idx) ] in
  snd (List.split (process ~extra ~fn:aux ~env ~db ~constraints name t))

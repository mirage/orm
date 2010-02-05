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

let exec_sql ~env ~db = exec_sql ~tag:"delete" ~db ~env

let delete_value ~env ~db ~id v =

  let process table id =
    let sql = sprintf "DELETE FROM %s WHERE __id__=?" table in
	exec_sql ~env ~db sql [ Data.INT id ] (db_must_step db) in

  let rec aux ?name id v = match v, name with
    | Null, _ | Int _, _ | Bool _, _ | Float _, _ | String _, _ | Arrow _, _ | Enum _, None | Var _, _ -> ()
    | Enum t    , Some n    -> (* TODO: process (Name.enum n) id *) ()
    | Ext ((n,i),v), _
    | Rec ((n,i),v), _      -> process n id; aux ~name:n i v
    | Sum (r,tl), Some name -> list_iteri (fun i t -> aux ~name:(Name.sum name r i) id t) tl
    | Dict tl   , Some name -> List.iter (fun (n,t) -> aux ~name:(Name.dict name n) id t) tl
    | Tuple tl  , Some name -> list_iteri (fun i t -> aux ~name:(Name.tuple name i) id t) tl
    | _                     -> failwith "TODO:5" in

  aux id v

(*
 * Copyright (c) 2009 Anil Madhavapeddy <anil@recoil.org>
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

(* --- Type definitions *)

type sql_type = 
  |Int     |Real
  |Text    |Blob
  |Null    |Foreign of string

type env = {
  e_tables: table list;
}
and table = {
  t_name: string;
  t_fields: field list;
}
and field = {
  f_name: string;
  f_typ: sql_type;
}

(* --- String conversion functions *)

let string_map del fn v =
  String.concat del (List.map fn v)

let string_of_sql_type = function
  |Int -> "INTEGER"  |Real -> "REAL"
  |Text -> "TEXT"    |Blob -> "BLOB"
  |Null -> "NULL"    |Foreign f -> "INTEGER"

let string_of_field f =
  sprintf "%s:%s" f.f_name (string_of_sql_type f.f_typ)
let string_of_table t =
  sprintf "%s: [ %s ] " t.t_name (string_map ", " string_of_field t.t_fields)
let string_of_env e =
  string_map "\n" string_of_table e.e_tables

(* --- Helper functions to manipulate environment *)

let empty_env = { e_tables = [] }

let find_table env name =
  try
    Some (List.find (fun t -> t.t_name = name) env.e_tables)
  with
    Not_found -> None

let replace_table env table =
  { env with e_tables =
    table :: (List.filter (fun t -> t.t_name <> table.t_name) env.e_tables)
  }

exception Field_name_not_unique
(* add field to the specified table, and return a modified env *)
let add_field env t field =
  match find_table env t with
  |Some table -> begin
    (* sanity check that the name is unique in the field list *)
    match List.filter (fun f -> f.f_name = field.f_name) table.t_fields with
    |[] -> 
      (* generate new table and replace it in the environment *)
      let table' = {table with t_fields = field :: table.t_fields } in
      replace_table env table'
    |_ -> raise Field_name_not_unique
  end
  |None ->
    prerr_endline (sprintf "warning: add_field: %s:f U %s:T failed" field.f_name t);
    env

(* --- Process functions to convert OCaml types into SQL *)

exception Type_not_allowed of string
let rec process env t ml_field =
  let n = ml_field.Types.f_id in
  let table = find_table env t in
  match ml_field.Types.f_typ with
  (* basic types *)
  |Types.Unit _        -> add_field env t { f_name=n; f_typ=Int  }
  |Types.Int  _        -> add_field env t { f_name=n; f_typ=Int  }
  |Types.Int32 _       -> add_field env t { f_name=n; f_typ=Int  }
  |Types.Int64 _       -> add_field env t { f_name=n; f_typ=Int  }
  |Types.Float _       -> add_field env t { f_name=n; f_typ=Real }
  |Types.Bool _        -> add_field env t { f_name=n; f_typ=Int  }
  |Types.Char _        -> add_field env t { f_name=n; f_typ=Int  }
  |Types.String _      -> add_field env t { f_name=n; f_typ=Text }
  (* complex types *)
  |Types.Apply(_,[],id,[]) ->
    add_field env t { f_name=n ^ "_id"; f_typ=Foreign id }
  |Types.Record (_,fl)
  |Types.Object (_,fl) -> 
    (* add an id to the current table *)
    let env = add_field env t {f_name=n ^ "_id"; f_typ=Foreign n} in
    (* add a new table to the environment *)
    let env = replace_table env {t_name=n; t_fields=[]} in
    (* process the fields in the new record *)
    List.fold_left (fun env field -> process env n field) env fl
  |Types.Array (_,ty)
  |Types.List (_,ty)   ->
    (* create a new table for the list lookup *)
    let t' = sprintf "%s_%s_list" t n in
    (* add table to env to represent the list *)
    let env = replace_table env { t_name=t'; t_fields=[] } in
    (* add in the list fields to the new table *)
    let env = add_field env t' { f_name= t ^ "_id"; f_typ=Foreign t } in
    let env = add_field env t' { f_name= "_idx"; f_typ=Int } in
    process env t' { ml_field with Types.f_typ=ty }
  |Types.Tuple (_,tyl) ->
    let i = ref 0 in
    List.fold_right (fun ty env ->
      incr i;
      process env t { ml_field with Types.f_id=sprintf "%s%d" n !i; f_typ=ty }
    ) tyl env
  |Types.Option (_,ty) ->
    process env t { ml_field with Types.f_typ=ty }
  (* types we dont handle for the moment *)
  |Types.Var _
  |Types.Abstract _ 
  |Types.Apply _
  |Types.Arrow _ as x -> raise (Type_not_allowed (Types.string_of_typ x))

let process_top t =
  let env = { e_tables = [] } in
   process env "top" t

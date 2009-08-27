(*pp camlp4orf *)

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
open Camlp4
open PreCast
open Ast

(* --- Type definitions *)

type sql_type = 
  |Int     |Real
  |Text    |Blob
  |Null    

type table_type =
  |Exposed        (* table is an exposed external type *)
  |Transient      (* table is internal (e.g. list) *)

type field_info =
  |External_and_internal_field
  |External_field
  |Internal_field
  |Internal_autoid
  |External_foreign of string

type env = {
  e_tables: table list;
}
and table = {
  t_name: string;
  t_fields: field list;
  t_type: table_type;
  t_child: string list; (* sub-tables created from this one *)
}
and field = {
  f_name: string;
  f_typ: sql_type;
  f_ctyp: ctyp;
  f_info: field_info;
}

(* --- String conversion functions *)

let string_map del fn v =
  String.concat del (List.map fn v)

let string_of_sql_type f = 
  match f.f_typ, f.f_info with
  |Int,Internal_autoid -> "INTEGER PRIMARY KEY AUTOINCREMENT"
  |Int,_ -> "INTEGER"
  |Real,_ -> "REAL"
  |Text,_ -> "TEXT"
  |Blob,_ -> "BLOB"
  |Null,_ -> "NULL"    

let string_of_field_info = function
  |External_and_internal_field -> "E+I"
  |External_field -> "E"
  |Internal_field -> "I"
  |External_foreign f -> sprintf "F[%s]" f
  |Internal_autoid -> "A"

let string_of_field f =
  sprintf "%s (%s):%s" f.f_name (string_of_field_info f.f_info) (string_of_sql_type f)
let string_of_table t =
  sprintf "%s (children=%s): [ %s ] " t.t_name (String.concat ", " t.t_child) (string_map ", " string_of_field t.t_fields)
let string_of_env e =
  string_map "\n" string_of_table e.e_tables

(* --- Helper functions to manipulate environment *)

let empty_env = { e_tables = [] }

let find_table env name =
  try
    Some (List.find (fun t -> t.t_name = name) env.e_tables)
  with
    Not_found -> None

(* replace the table in the env and return new env *)
let replace_table env table =
  { e_tables = table :: 
    (List.filter (fun t -> t.t_name <> table.t_name) env.e_tables)
  }

let new_table ~name ~fields ~ty ~parent env =
  (* stick in the new table *)
  let env = replace_table env (match find_table env name with 
    |None -> { t_name=name; t_fields=fields; t_type=ty; t_child=[] }
    |Some table -> failwith (sprintf "new_table: clash %s" name) 
  ) in
  (* check if the table is a child and update parent child list if so *)
  match parent with
    |None -> env
    |Some t -> begin
      match find_table env t with
      |None -> env
      |Some ptable -> replace_table env { ptable with t_child=name::ptable.t_child }
    end

exception Field_name_not_unique
(* add field to the specified table, and return a modified env *)
let add_field ~ctyp ~info env t field_name field_type =
  prerr_endline (sprintf "add_field: %s" field_name);
  let _loc = Loc.ghost in
  let field = { f_name=field_name; f_typ=field_type; f_ctyp=ctyp; f_info=info } in
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

(* --- Accessor functions to filter the environment *)

(* list of tables for top-level code generation *)
let exposed_tables env =
  List.filter (fun t ->
     t.t_type = Exposed
   ) env.e_tables

(* helper fn to lookup a table in the env and apply a function to it *)
let with_table fn env t =
  match find_table env t with
  |None -> 
    failwith (sprintf "internal error: exposed fields table '%s' not found" t)
  |Some table ->
    fn env table

let filter_fields_with_table fn =
   with_table (fun env table ->
     List.filter fn table.t_fields
   )

(* list of fields suitable for external ocaml interface *)
let exposed_fields =
   filter_fields_with_table (fun f ->
     match f.f_info with
     |External_and_internal_field
     |External_field 
     |Internal_autoid
     |External_foreign _ -> true
     |Internal_field -> false
   )

(* list of fields suitable for SQL statements *)
let sql_fields =
   filter_fields_with_table (fun f ->
     match f.f_info with
     |External_and_internal_field 
     |External_foreign _
     |Internal_field 
     |Internal_autoid -> true
     |External_field -> false
   )

(* same as sql_fields but with the auto_id field filtered out *)
let sql_fields_no_autoid =
   filter_fields_with_table (fun f ->
     match f.f_info with
     |External_and_internal_field
     |External_foreign _
     |Internal_field -> true
     |Internal_autoid
     |External_field -> false
   )

(* get the foreign single fields (ie foreign tables which arent lists) *)
let foreign_single_fields =
  filter_fields_with_table (fun f ->
    match f.f_info with
    |External_foreign _ -> true
    |_ -> false
  )

(* retrieve the single Auto_id field from a table *)
let auto_id_field =
  with_table (fun env table ->
    match List.filter (fun f -> f.f_info = Internal_autoid) table.t_fields with
    |[f] -> f
    |[] -> failwith (sprintf "auto_id_field: %s: no entry" table.t_name)
    |_ -> failwith (sprintf "auto_id_field: %s: multiple entries" table.t_name)
  )

let is_optional_field f =
  let _loc = Loc.ghost in 
  match f.f_ctyp with
  | <:ctyp< option $t$ >> -> true
  | _ -> false

(* --- Process functions to convert OCaml types into SQL *)

let rec process_type_declarations _loc ctyp env =
  let rec fn ty env =
    match ty with
    |Ast.TyAnd (_loc, tyl, tyr) ->
       (* two types, so process both and add to list *)
       fn tyl (fn tyr env)
    |Ast.TyDcl (_loc, id, _, ty, []) ->
       (* we ignore type variables for the moment *)
       process_toplevel_type _loc id ty env
    |_ -> failwith "process_type_declarations: unexpected type"
   in fn ctyp env

and process_toplevel_type _loc n ctyp env =
  match ctyp with
  | <:ctyp@loc< < $fs$ > >>
  | <:ctyp@loc< { $fs$ } >> ->
    (* add a new table to the environment *)
    let env = new_table ~name:n ~ty:Exposed ~fields:[] ~parent:None env in
    let env = add_field ~ctyp:<:ctyp< option int64 >> ~info:(Internal_autoid) env n "id" Int in
    let rec fn env = function
    | <:ctyp< $t1$; $t2$ >> -> fn (fn env t1) t2
    | <:ctyp< $lid:id$ : mutable $t$ >>
    | <:ctyp< $lid:id$ : $t$ >> ->  process_type _loc n id t env
    | _ -> failwith "process_toplevel_type: unexpected ast"
    in fn env fs
  | _ -> failwith "process_toplevel_type: unknown type"

 and process_type _loc t n ctyp env =
  let info = External_and_internal_field in
  match ctyp with
  | <:ctyp@loc< unit >> -> add_field ~ctyp ~info env t n Int
  | <:ctyp@loc< int >> -> add_field ~ctyp ~info env t n Int 
  | <:ctyp@loc< int32 >> -> add_field ~ctyp ~info env t n Int
  | <:ctyp@loc< int64 >> -> add_field ~ctyp ~info env t n Int
  | <:ctyp@loc< float >> -> add_field ~ctyp ~info env t n Real
  | <:ctyp@loc< bool >> -> add_field ~ctyp ~info env t n Int
  | <:ctyp@loc< char >> -> add_field ~ctyp ~info env t n Int
  | <:ctyp@loc< string >> -> add_field ~ctyp ~info env t n Text
  | <:ctyp@loc< $id:id$ >> ->
    let ids = Ast.list_of_ident id in
    env

let field_to_sql_data _loc f =
  let id = <:expr< $lid:"_" ^ f.f_name$ >> in
  match f.f_ctyp with
  | <:ctyp@loc< unit >> -> <:expr< Sqlite3.Data.INT 1L >>
  | <:ctyp@loc< int >> -> <:expr< Sqlite3.Data.INT (Int64.of_int $id$) >>
  | <:ctyp@loc< int32 >> -> <:expr< Sqlite3.Data.INT (Int64.of_int32 $id$) >>
  | <:ctyp@loc< int64 >> -> <:expr< Sqlite3.Data.INT $id$ >>
  | <:ctyp@loc< float >> -> <:expr< Sqlite3.Data.FLOAT $id$ >>
  | <:ctyp@loc< char >> -> <:expr< Sqlite3.Data.INT (Int64.of_int (Char.code $id$)) >>
  | <:ctyp@loc< string >> -> <:expr< Sqlite3.Data.TEXT $id$ >>
  | <:ctyp@loc< bool >> ->  <:expr< Sqlite3.Data.INT (if $id$ then 1L else 0L) >>
  | _ -> failwith "to_sql_data: unknown type"


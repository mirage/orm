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
  |Auto_id     (* auto incrementing id field primary key *)
  |Hidden_list (* used to mark a list in the original table *)

type table_type =
  |Exposed        (* table is an exposed external type *)
  |Transient      (* table is internal (e.g. list) *)

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
  f_ctyp: Types.typ option;
  f_opt: bool;
}

(* --- String conversion functions *)

let string_map del fn v =
  String.concat del (List.map fn v)

let string_of_sql_type = function
  |Int -> "INTEGER"  |Real -> "REAL"
  |Text -> "TEXT"    |Blob -> "BLOB"
  |Null -> "NULL"    |Foreign f -> "INTEGER"
  |Auto_id -> "INTEGER PRIMARY KEY AUTOINCREMENT"
  |Hidden_list -> "XLIST" (* should never be output in actual SQL *)

let string_of_field f =
  sprintf "%s:%s%s" f.f_name (string_of_sql_type f.f_typ) (if f.f_opt then " opt" else "")
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

let new_table ~name ~fields ~parent env =
  (* stick in the new table *)
  let ty = match parent with |None -> Exposed |Some _ -> Transient in
  let env = replace_table env (match find_table env name with 
    |None -> { t_name=name; t_fields=fields; t_type=ty; t_child=[] }
    |Some table -> failwith (sprintf "new_table: clash %s" name) 
  ) in
  (* check if the table is a child and update parent child list if so *)
  match parent with
    |None -> env
    |Some t -> begin
      match find_table env t with
      |None -> failwith (sprintf "couldnt find parent table %s" t)
      |Some ptable -> replace_table env { ptable with t_child=name::ptable.t_child }
    end

exception Field_name_not_unique
(* add field to the specified table, and return a modified env *)
let add_field ~opt ~ctyp env t field_name field_type =
  let ctyp = match ctyp,opt with |Some ctyp,true -> Some (Types.Option (Types.loc_of_typ ctyp, ctyp)) |_ -> ctyp in
  let field = { f_name=field_name; f_typ=field_type; f_opt=opt; f_ctyp=ctyp } in
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

(* list of fields suitable for external ocaml interface *)
let exposed_fields env t =
  match find_table env t with
  |None -> 
    failwith (sprintf "internal error: exposed fields table '%s' not found" t)
  |Some table ->
    List.fold_left (fun a f ->
      match f.f_ctyp with 
      |None -> a
      |Some ctyp -> (ctyp, f) :: a
    ) [] table.t_fields

(* list of fields suitable for SQL statements *)
let sql_fields env t =
  match find_table env t with
  |None ->
    failwith (sprintf "internal error: exposed fields table '%s' not found" t)
  |Some table -> begin
    List.fold_left (fun a f ->
      match f.f_typ with 
      |Hidden_list -> a
      |_ -> f :: a
    ) [] table.t_fields
  end
 
(* --- Process functions to convert OCaml types into SQL *)

exception Type_not_allowed of string
let rec process ?(opt=false) ?(ctyp=None) env t ml_field =
  let n = ml_field.Types.f_id in
  let ctyp = Some ml_field.Types.f_typ in
  match ml_field.Types.f_typ with
  (* basic types *)
  |Types.Unit _        -> add_field ~opt ~ctyp env t n Int
  |Types.Int  _        -> add_field ~opt ~ctyp env t n Int
  |Types.Int32 _       -> add_field ~opt ~ctyp env t n Int
  |Types.Int64 _       -> add_field ~opt ~ctyp env t n Int
  |Types.Float _       -> add_field ~opt ~ctyp env t n Real
  |Types.Bool _        -> add_field ~opt ~ctyp env t n Int
  |Types.Char _        -> add_field ~opt ~ctyp env t n Int
  |Types.String _      -> add_field ~opt ~ctyp env t n Text 
  (* complex types *)
  |Types.Apply(_,[],id,[]) ->
    add_field ~opt ~ctyp env t (n ^ "_id") (Foreign id)
  |Types.Record (_,fl)
  |Types.Object (_,fl) -> 
    (* add an id to the current table *)
    let env = add_field ~opt ~ctyp env t (n ^ "_id") (Foreign n) in
    (* add a new table to the environment *)
    let env = new_table ~name:n ~fields:[] ~parent:None env in
    (* process the fields in the new record *)
    let env = add_field ~opt:false ~ctyp:None env t "id" Auto_id in
    List.fold_left (fun env field -> process env n field) env fl
  |Types.Array (_,ty)
  |Types.List (_,ty)   ->
    (* create a new table for the list lookup *)
    let t' = sprintf "%s_%s_list" t n in
    (* add table to env to represent the list *)
    let env = new_table ~name:t' ~fields:[] ~parent:(Some t) env in
    (* add in the list fields to the new table *)
    let env = add_field ~opt ~ctyp env t n Hidden_list in
    let env = add_field ~opt ~ctyp env t' (t ^ "_id") (Foreign t) in
    let env = add_field ~opt ~ctyp env t' "_idx" Int in
    process env t' { ml_field with Types.f_typ=ty }
  |Types.Tuple (_,tyl) ->
    let i = ref 0 in
    List.fold_right (fun ty env ->
      incr i;
      process env t { ml_field with Types.f_id=sprintf "%s%d" n !i; f_typ=ty }
    ) tyl env
  |Types.Option (_,ty) ->
    process ~opt:true env t { ml_field with Types.f_typ=ty }
  |Types.Variant (loc, itl) -> 
    (* Just unoptimised with a unique set of columns per branch at the moment *)
    let env = new_table ~name:n ~fields:[] ~parent:(Some t) env in
    let env = add_field ~opt ~ctyp env n "_idx" Int in
    List.fold_right (fun (id,tyl) env ->
       process env n { ml_field with Types.f_id=id; f_typ=Types.Tuple(loc, tyl) }
    ) itl env
  (* types we dont handle for the moment *)
  |Types.Ref _
  |Types.PolyVar _
  |Types.Var _
  |Types.Abstract _ 
  |Types.Apply _
  |Types.Arrow _ as x -> raise (Type_not_allowed (Types.string_of_typ x))

let process_top t =
  let env = { e_tables = [] } in
   process env "top" t


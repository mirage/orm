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
  |List           (* table is an internal list *)
  |Sexp of ctyp   (* generate of/to sexp functions for this type *)

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

let empty_env () = { e_tables = [] }

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
  |None -> env

let is_foreign f = match f.f_info with External_foreign _ -> true | _ -> false
let get_foreign f = match f.f_info with
  | External_foreign f -> f 
  | _ -> failwith (sprintf "%s is not a foreign field" f.f_name)
let is_autoid f = match f.f_info with Internal_autoid -> true | _ -> false

(* --- Accessor functions to filter the environment *)

(* list of tables for top-level code generation *)
let exposed_tables env =
  List.filter (fun t ->
     t.t_type = Exposed
   ) env.e_tables

let sql_tables env =
  List.filter (fun t ->
    match t.t_type with
    |Exposed
    |List -> true
    |Sexp _ -> false
  ) env.e_tables

(* list of tables to generate sexp to/from functions for *)
let sexp_tables env = 
  List.fold_left (fun a t ->
    match t.t_type with
    |Sexp c -> (t.t_name, c) :: a
    |_ -> a
  ) [] env.e_tables
  
(* helper fn to lookup a table in the env and apply a function to it *)
let with_table fn env t =
  match find_table env t with
  |None -> 
    failwith (sprintf "internal error: exposed fields table '%s' not found" t)
  |Some table ->
    fn env table

(* add an option type to a field entry *)
let add_option_to_field n = 
  let _loc = Loc.ghost in
  with_table (fun env table ->
    replace_table env (
      let fs = List.map (fun f -> 
        if f.f_name = n then (
           {f with f_ctyp = <:ctyp<option $f.f_ctyp$>> }
        ) else f
      ) table.t_fields in
      {table with t_fields=fs}
    )
  )

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

let ctyp_is_list = function
  | <:ctyp< list $c$ >> 
  | <:ctyp< array $c$ >> -> true
  | _ -> false

(* get the foreign single fields (ie foreign tables which arent lists) *)
let foreign_single_fields =
  filter_fields_with_table (fun f ->
    match ctyp_is_list f.f_ctyp with 
    |true -> false
    |false -> begin
      match f.f_info with
      |External_foreign _ -> true
      |_ -> false
    end
  )

(* generate the table name for a list type in a table *)
let list_table_name t f =
  sprintf "%s_%s__list" t f 

(* return all the fields which are list types *)
let foreign_many_fields = 
  filter_fields_with_table (fun f ->
    ctyp_is_list f.f_ctyp
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

let rec process tds =
  let _loc = Loc.ghost in
  let env = process_type_declarations _loc tds (empty_env ()) in
  check_foreign_refs env

and process_type_declarations _loc ctyp env =
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
  | _ ->
    (* create an sexpr conversion for an unknown type, so it can be
       used if that type is referenced later on for SQL conversion *)
    new_table ~name:n ~ty:(Sexp ctyp) ~fields:[] ~parent:None env

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
  | <:ctyp@loc< option $ctyp$ >> ->
     let env = process_type _loc t n ctyp env in
     add_option_to_field n env t
  | <:ctyp@loc< $lid:id$ >> -> begin
     (* add an external type as a foreign id. this may not actually
        exist, but we do a second pass later to fix up any invalid
        External_foreign fields and convert them to sexps instead.
        At this stage, we are still processing types so we dont know
        the full set *)
     add_field ~ctyp ~info:(External_foreign id) env t n Int
  end  
  | <:ctyp@loc< list $ctyp$ >>
  | <:ctyp@loc< array $ctyp$ >> as orig_ctyp ->
    let env = add_field ~ctyp:orig_ctyp ~info:External_field env t n Null in
    (* construct the transient list table *)
    let name = sprintf "%s_%s__list" t n in
    let env = new_table ~name ~ty:List ~fields:[] ~parent:None env in
    let env = add_field ~ctyp:<:ctyp< int64 >> ~info:Internal_field env name "id" Int in
    let env = add_field ~ctyp:<:ctyp< int64 >> ~info:Internal_field env name "_idx" Int in
    process_type _loc name n ctyp env 
  | _ -> 
    (* add an unknown field as-is, it will be converted to sexp later on *)
    add_field ~ctyp ~info env t n Text

(* run through the fully-populated environment and check that all
   foreign references do in fact exist, and if not convert them to 
   opaque sexp types *)
and check_foreign_refs env =
  let _loc = Loc.ghost in
  let f_to_sexp f = {f with f_info=External_and_internal_field; f_typ=Text } in
  let f_to_foreign f = 
    match f.f_info with
    |External_foreign t -> {f with f_ctyp= <:ctyp< $lid:t^"_persist"$ >> }
    |_ -> assert false in
  let tables = List.map (fun t ->
    let fields = List.map (fun f ->
      match f.f_info with
      |External_foreign id -> begin
        match find_table env id with
        |None -> (* no type we know about, so make this an sexp *)
          f_to_sexp f
        |Some t' -> begin
          match t'.t_type with
          |Sexp _ ->
             f_to_sexp f (* is sexp type, so convert it *)
          |Exposed |List ->
             f_to_foreign f (* rewrite type to have _persist in it *)
        end
      end
      |_ -> f
    ) t.t_fields in
    {t with t_fields=fields}
  ) env.e_tables in
  {env with e_tables=tables}

let field_to_sql_data _loc f =
  let id = <:expr< $lid:"_" ^ f.f_name$ >> in
  let pid = <:patt< $lid:"_" ^ f.f_name$ >> in
  let rec fn = function
  | <:ctyp@loc< unit >> -> <:expr< Sqlite3.Data.INT 1L >>
  | <:ctyp@loc< int >> -> <:expr< Sqlite3.Data.INT (Int64.of_int $id$) >>
  | <:ctyp@loc< int32 >> -> <:expr< Sqlite3.Data.INT (Int64.of_int32 $id$) >>
  | <:ctyp@loc< int64 >> -> <:expr< Sqlite3.Data.INT $id$ >>
  | <:ctyp@loc< float >> -> <:expr< Sqlite3.Data.FLOAT $id$ >>
  | <:ctyp@loc< char >> -> <:expr< Sqlite3.Data.INT (Int64.of_int (Char.code $id$)) >>
  | <:ctyp@loc< string >> -> <:expr< Sqlite3.Data.TEXT $id$ >>
  | <:ctyp@loc< bool >> ->  <:expr< Sqlite3.Data.INT (if $id$ then 1L else 0L) >>
  | <:ctyp@loc< option $t$ >> ->
      <:expr<
         match $id$ with [
            None -> Sqlite3.Data.NULL
           |Some $pid$ -> $fn t$
         ]
      >>
  | ctyp -> begin
     match (ctyp, f.f_info) with
     | <:ctyp@loc< $lid:tid$ >>, (External_foreign _) -> 
        <:expr< Sqlite3.Data.INT $lid:sprintf "_%s_id" f.f_name$ >>
     |_ ->
       (* convert unknown type to an sexpression *)
       let sexp_binding = Pa_sexp_conv.Generate_sexp_of.sexp_of_td _loc f.f_name [] ctyp in
       let conv_fn = "sexp_of_" ^ f.f_name in
       <:expr< 
          Sqlite3.Data.TEXT (
            Sexplib.Sexp.to_string_hum (let $sexp_binding$ in $lid:conv_fn$ $id$)
          ) 
       >>
  end
  in fn f.f_ctyp

let sql_data_to_field _loc f =
  let id = <:expr< $lid:"_" ^ f.f_name$ >> in
  let pid = <:patt< $lid:"_" ^ f.f_name$ >> in
  let rec fn = function
  | <:ctyp@loc< unit >> -> <:expr< () >>
  | <:ctyp@loc< int >> -> <:expr< match $id$ with [ Sqlite3.Data.INT x -> Int64.to_int x | _ -> failwith "TODO" ] >>
  | <:ctyp@loc< int32 >> -> <:expr< match $id$ with [ Sqlite3.Data.INT x -> Int64.to_int32 x | _ -> failwith "TODO" ] >>
  | <:ctyp@loc< int64 >> -> <:expr< match $id$ with [ Sqlite3.Data.INT x -> x | _ -> failwith "TODO" ] >>
  | <:ctyp@loc< float >> -> <:expr< match $id$ with [ Sqlite3.Data.FLOAT x -> x | _ -> failwith "TODO" ] >>
  | <:ctyp@loc< char >> -> <:expr< match $id$ with [ Sqlite3.Data.INT x -> Char.chr (Int64.to_int x) | _ -> failwith "TODO" ] >>
  | <:ctyp@loc< string >> -> <:expr< match $id$ with [ Sqlite3.Data.TEXT x -> x | _ -> failwith "TODO" ] >>
  | <:ctyp@loc< bool >> ->  <:expr< match $id$ with [ Sqlite3.Data.INT 1L -> True | Sqlite3.Data.INT 0L -> False | _ -> failwith "TODO" ] >>
  | <:ctyp@loc< option $t$ >> ->
      <:expr<
         match $id$ with [
         Sqlite3.Data.NULL -> None
         | x -> Some ($fn t$)
         ]
      >>
  | ctyp -> 
    (* convert unknown type to an sexpression *)
    let internal_binding, external_binding = 
      Pa_sexp_conv.Generate_of_sexp.td_of_sexp _loc f.f_name [] ctyp in
    let conv_fn = f.f_name ^ "_of_sexp" in
      <:expr<
        match $id$ with [ 
          Sqlite3.Data.TEXT $pid$ ->
            let $internal_binding$ in
            let $external_binding$ in 
            $lid:conv_fn$ (Sexplib.Sexp.of_string $id$)
        | _ -> failwith "TODO"
          ]
       >>
  in
  if is_foreign f then
    <:expr< match $id$ with [ Sqlite3.Data.INT x -> x | _ -> failwith "TODO" ] >>
  else
    fn f.f_ctyp

let to_string _loc f =
  let id = <:expr< $lid:f.f_name$ >> in
  let pid = <:patt< $lid:f.f_name$ >> in
  let rec fn = function
  | <:ctyp@loc< unit >> -> <:expr< "1" >>
  | <:ctyp@loc< int >> -> <:expr< string_of_int $id$ >>
  | <:ctyp@loc< int32 >> -> <:expr< Int32.to_string $id$ >>
  | <:ctyp@loc< int64 >> -> <:expr< Int64.to_string $id$ >>
  | <:ctyp@loc< float >> -> <:expr< string_of_float $id$ >>
  | <:ctyp@loc< char >> -> <:expr< String.make 1 $id$ >>
  | <:ctyp@loc< string >> -> <:expr< $id$ >>
  | <:ctyp@loc< bool >> ->  <:expr< string_of_bool $id$ >>
  | <:ctyp@loc< option $t$ >> ->
      <:expr<
         match $id$ with [
            None -> "NULL"
           |Some $pid$ -> $fn t$
         ]
      >>
  | ctyp -> begin
     match (ctyp, f.f_info) with
     | <:ctyp@loc< $lid:tid$ >>, (External_foreign _) -> assert false
     |_ ->
       (* convert unknown type to an sexpression *)
       let sexp_binding = Pa_sexp_conv.Generate_sexp_of.sexp_of_td _loc f.f_name [] ctyp in
       let conv_fn = "sexp_of_" ^ f.f_name in
       <:expr< 
            Sexplib.Sexp.to_string_hum (let $sexp_binding$ in $lid:conv_fn$ $id$)
       >>
    end in

  if f.f_info = Internal_autoid then
    <:expr< Int64.to_string $id$ >>
  else 
    fn f.f_ctyp

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

module PP = Camlp4.Printers.OCaml.Make(Syntax)
let pp = new PP.printer ()
let debug_ctyp ty = Format.eprintf "DEBUG CTYP: %a@." pp#ctyp ty

(* --- Type definitions *)

type sql_type = 
  |Int     |Real
  |Text    |Blob
  |Null    

type variant_info = {
  v_indices: (string , (int64 * bool)) Hashtbl.t
}

type table_type =
  |Exposed        (* table is an exposed external type *)
  |List           (* table is an internal list *)
  |Variant of variant_info (* table is a variant list *)
  |Tuple

type field_info =
  |External_and_internal_field
  |Internal_field
  |Internal_autoid
  |External_foreign of (string * string option)
and env = {
  e_tables: table list;
  e_types: (string * ctyp) list;
}
and table = {
  t_name: string;
  t_fields: field list;
  t_type: table_type;
  t_ctyp: ctyp;
  t_child: string list; (* sub-tables created from this one *)
}
and field = {
  f_name: string;
  f_typ: sql_type;
  f_ctyp: ctyp;
  f_info: field_info;
  f_table: string;
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
  |Internal_field -> "I"
  |External_foreign (f,t) -> sprintf "F[%s/%s]" f 
      (match t with |None ->"?" |Some x -> x)
  |Internal_autoid -> "A"

let string_of_field f =
  sprintf "%s (%s):%s" f.f_name (string_of_field_info f.f_info) (string_of_sql_type f)
let string_of_table t =
  sprintf "%s / %s (child=%s): [ %s ] " t.t_name (match t.t_type with |Exposed -> "Exposed" |List -> "List" |Variant _ -> "Variant" |Tuple -> "Tuple") (String.concat ", " t.t_child) (string_map ", " string_of_field t.t_fields)
let string_of_env e =
  List.iter (fun (n,t) -> eprintf "%s : " n; debug_ctyp t) e.e_types;
  string_map "\n" string_of_table e.e_tables 

(* --- Helper functions to manipulate environment *)

let empty_env () = { e_tables = []; e_types=[] }

let find_table env name =
  try
    Some (List.find (fun t -> t.t_name = name) env.e_tables)
  with
    Not_found -> None

(* replace the table in the env and return new env *)
let replace_table env table =
  { env with e_tables = table :: 
    (List.filter (fun t -> t.t_name <> table.t_name) env.e_tables)
  }

let new_table ~name ~ty ~ctyp ~parent env =
  (* stick in the new table *)
  let env = replace_table env (match find_table env name with 
    |None -> { t_name=name; t_fields=[]; t_type=ty; t_ctyp=ctyp; t_child=[] }
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

(* helper fn to lookup a table in the env and apply a function to it *)
let with_table fn env t =
  match find_table env t with
  |None -> 
    failwith (sprintf "internal error: exposed fields table '%s' not found" t)
  |Some table ->
    fn env table

exception Field_name_not_unique
(* add field to the specified table, and return a modified env *)
let add_field ~ctyp ~info env t field_name field_type =
  if field_name = "" then failwith ("empty field name for " ^ t);
  let _loc = Loc.ghost in
  match find_table env t with
  |Some table -> begin
    let field = { f_name=field_name; f_typ=field_type; f_ctyp=ctyp; f_info=info; f_table=t } in
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
let is_foreign_exposed env f = match f.f_info with
  |External_foreign (_, (Some t)) ->
     with_table (fun env t -> t.t_type = Exposed) env t
  |_ -> false
let get_foreign f = match f.f_info with
  | External_foreign (f,_) -> f 
  | _ -> failwith (sprintf "%s is not a foreign field" f.f_name)
let get_foreign_table env f = match f.f_info with
  | External_foreign (_,(Some t)) -> with_table (fun env t -> t) env t
  | _ -> failwith (sprintf "%s is not a foreign field" f.f_name)
let is_autoid f = match f.f_info with Internal_autoid -> true | _ -> false

(* --- Accessor functions to filter the environment *)

(* list of tables for top-level code generation *)
let exposed_tables env =
  List.filter (fun t ->
     t.t_type = Exposed
   ) env.e_tables

let not_exposed_tables env =
  List.filter (fun t ->
     t.t_type <> Exposed
   ) env.e_tables

let sql_tables env =
  List.filter (fun t ->
    match t.t_type with
    |Exposed
    |List |Tuple |Variant _ -> true
  ) env.e_tables

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

let ctyp_is_list = function
  | <:ctyp< list $c$ >> 
  | <:ctyp< array $c$ >> -> true
  | _ -> false

(* list of fields suitable for external ocaml interface *)
let exposed_fields =
   filter_fields_with_table (fun f ->
     match f.f_info with
     |External_and_internal_field
     |Internal_autoid
     |External_foreign _ -> true
     |Internal_field -> false
   )

(* list of fields suitable for SQL statements *)
let sql_fields =
   filter_fields_with_table (fun f ->
     not (ctyp_is_list f.f_ctyp)
   )

(* list of fields which are foreign ids *)
let id_fields =
   filter_fields_with_table (fun f ->
     match f.f_info with
     |External_foreign _
     |Internal_autoid -> true
     |_ -> false
   )

(* same as sql_fields but with the auto_id field filtered out *)
let sql_fields_no_autoid =
   filter_fields_with_table (fun f ->
     not ((ctyp_is_list f.f_ctyp) || f.f_info = Internal_autoid)
   )

let sql_fields_no_internal =
  filter_fields_with_table (fun f ->
    not ((ctyp_is_list f.f_ctyp) || f.f_info = Internal_autoid || 
    f.f_info = Internal_field)
  )

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

let ctyp_of_table t =
  let _loc = Loc.ghost in
  match t.t_type with
  |Exposed |Variant _ -> <:ctyp< $lid:t.t_name$ >>
  |_ -> t.t_ctyp

(* follow all the links in a table fields to determine
   if it ever points back to itself or not *)
exception Is_recursive
let is_recursive_table env t =
  let h = Hashtbl.create 1 in
  let rec fn t =
    if Hashtbl.mem h t.t_name then
       raise Is_recursive
    else begin
      Hashtbl.add h t.t_name ();
      List.iter (fun f ->
        match f.f_info with
        |External_foreign (id,(Some t')) ->
          with_table (fun env t -> fn t) env t'
        |External_foreign (_,None) -> 
          failwith (sprintf "incomplete type for EF: %s" (string_of_field f))
        |_ -> ()
      ) t.t_fields
    end
  in
  try fn t; false
  with Is_recursive -> true
    
let field_accessor f =
  let _loc = Loc.ghost in
  <:expr< $lid:"_"^f.f_name$ >>

(* --- Name functions *)

let savefn t    = sprintf "__%s_to_db" t.t_name
let extsavefn t = sprintf "%s_to_db" t.t_name

let getfn  t    = sprintf "%s_of_db"  t.t_name
let tidfn t     = sprintf "%s__id"    t.t_name
let tnewfn t    = sprintf "%s__new"   t.t_name
let fidfn  f    = sprintf "%s__id_%s" f.f_table f.f_name
let fcachefn t  = sprintf "C_%s"      t.t_name
let fpcachefn t = sprintf "P_%s"      t.t_name
let fautofn env t = fidfn (auto_id_field env t.t_name)
let whashfn t   = sprintf "W_%s"      t.t_name

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
    let env = new_table ~name:n ~ty:Exposed ~ctyp ~parent:None env in
    let env = add_field ~ctyp:<:ctyp< option int64 >> ~info:(Internal_autoid) env n "id" Int in
    let rec fn env = function
    | <:ctyp< $t1$; $t2$ >> -> fn (fn env t1) t2
    | <:ctyp< $lid:id$ : mutable $t$ >>
    | <:ctyp< $lid:id$ : $t$ >> ->  process_type _loc n id t env
    | _ -> failwith "process_toplevel_type: unexpected ast"
    in fn env fs
  | <:ctyp< [< $row_fields$ ] >> 
  | <:ctyp< [> $row_fields$ ] >>
  | <:ctyp< [= $row_fields$ ] >> 
  | <:ctyp< [ $row_fields$ ] >> ->
    let vi = { v_indices=Hashtbl.create 1 } in
    let env = new_table ~name:n ~ty:(Variant vi) ~ctyp ~parent:None env in
    let env = add_field ~ctyp:<:ctyp< option int64 >> ~info:(Internal_autoid) env n "id" Int in
    let env = add_field ~ctyp:<:ctyp< int64 >> ~info:Internal_field env n "_idx" Int in
    let pos = ref 0 in
    let register_id id args = 
      Hashtbl.add vi.v_indices id ((Int64.of_int !pos),args); incr pos in
    let rec fn env = function
    | <:ctyp< $t1$ | $t2$ >> -> fn (fn env t1) t2
    | <:ctyp< $uid:id$ of $t$ >> ->
       register_id id true;
       let id = String.uncapitalize id in
       process_type _loc n id t env
    | <:ctyp< $uid:id$ >> ->
       register_id id false;
       env
    | _ -> failwith "unknown variant AST"
    in fn env row_fields
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
  | <:ctyp@loc< option $ctyp$ >> ->
     let env = process_type _loc t n ctyp env in
     add_option_to_field n env t
  | <:ctyp@loc< ( $tup:tp$ ) >> -> 
     let name = sprintf "%s_%s__t" t n in
     let env = new_table ~name ~ty:Tuple ~ctyp ~parent:(Some t) env in
     let env = add_field ~ctyp:<:ctyp< option int64 >> ~info:Internal_autoid env name "id" Int in
     let env = add_field ~ctyp ~info:(External_foreign(name,None)) env t n Int in
     let tys = list_of_ctyp tp [] in
     let pos = ref 0 in
     List.fold_left (fun env ctyp -> 
       let tuple_name = sprintf "c%d" !pos in
       incr pos;
       process_type _loc name tuple_name ctyp env
     ) env tys
  | <:ctyp@loc< $lid:id$ >> -> begin
     add_field ~ctyp ~info:(External_foreign (id,None)) env t n Int
  end  
  | <:ctyp@loc< list $ctyp$ >>
  | <:ctyp@loc< array $ctyp$ >> as orig_ctyp ->
    (* construct the transient list table *)
    let name = sprintf "%s_%s" t n in
    let env = add_field ~ctyp:orig_ctyp ~info:(External_foreign (name,None)) env t n Int in
    let env = new_table ~name ~ty:List ~ctyp:orig_ctyp ~parent:(Some t) env in
    let env = add_field ~ctyp:<:ctyp< int64 >> ~info:Internal_field env name "id" Int in
    let env = add_field ~ctyp:<:ctyp< int64 >> ~info:Internal_field env name "_idx" Int in
    process_type _loc name "lst" ctyp env 
  | _ -> 
    failwith "unknown type"

(* run through the fully-populated environment and check that all
   foreign references do in fact exist, and if not convert them to 
   opaque sexp types *)
and check_foreign_refs env =
  let _loc = Loc.ghost in
  let tables = List.map (fun t ->
    let fields = List.map (fun f ->
      match f.f_info with
      | External_foreign (id,None) -> begin
        debug_ctyp f.f_ctyp;
        match find_table env id with
        |None ->
          eprintf "UNKNOWN: %s -- %s\n" t.t_name f.f_name;
          f
        |Some t' ->
          {f with f_info = (External_foreign (id, (Some t'.t_name))) }
      end
      | _ -> f
    ) t.t_fields in
    {t with t_fields=fields}
  ) env.e_tables in
  {env with e_tables=tables}

let field_to_sql_data _loc f =
  let id = <:expr< $lid:"_" ^ f.f_name$ >> in
  let pid = <:patt< $lid:"_" ^ f.f_name$ >> in
  let rec fn = function
  | <:ctyp@loc< unit >>   -> <:expr< Sqlite3.Data.INT 1L >>
  | <:ctyp@loc< int >>    -> <:expr< Sqlite3.Data.INT (Int64.of_int $id$) >>
  | <:ctyp@loc< int32 >>  -> <:expr< Sqlite3.Data.INT (Int64.of_int32 $id$) >>
  | <:ctyp@loc< int64 >>  -> <:expr< Sqlite3.Data.INT $id$ >>
  | <:ctyp@loc< float >>  -> <:expr< Sqlite3.Data.FLOAT $id$ >>
  | <:ctyp@loc< char >>   -> <:expr< Sqlite3.Data.INT (Int64.of_int (Char.code $id$)) >>
  | <:ctyp@loc< string >> -> <:expr< Sqlite3.Data.TEXT $id$ >>
  | <:ctyp@loc< bool >>   ->  <:expr< Sqlite3.Data.INT (if $id$ then 1L else 0L) >>
  | <:ctyp@loc< option $t$ >> ->
      <:expr<
         match $id$ with [
            None -> Sqlite3.Data.NULL
           |Some $pid$ -> $fn t$
         ]
      >>
  | ctyp ->
    <:expr< Sqlite3.Data.INT $lid:"_"^f.f_name^"_id"$ >>
  in fn f.f_ctyp

let sql_data_to_field _loc f =
  let id  = <:expr< $lid:"__" ^ f.f_name$ >> in
  let rec fn = function
  | <:ctyp< unit >> -> <:expr< () >>
  | <:ctyp< int >> -> 
      <:expr< match $id$ with 
          [ Sqlite3.Data.INT x -> Int64.to_int x | _ -> failwith "TODO" ] >>
  | <:ctyp< int32 >> ->
      <:expr< match $id$ with 
          [ Sqlite3.Data.INT x -> Int64.to_int32 x | _ -> failwith "TODO" ] >>
  | <:ctyp< int64 >> ->
      <:expr< match $id$ with 
          [ Sqlite3.Data.INT x -> x | _ -> failwith "TODO" ] >>
  | <:ctyp< float >> -> 
      <:expr< match $id$ with 
          [ Sqlite3.Data.FLOAT x -> x | _ -> failwith "TODO" ] >>
  | <:ctyp< char >> -> 
      <:expr< match $id$ with 
          [ Sqlite3.Data.INT x -> Char.chr (Int64.to_int x) | _ -> failwith "TODO" ] >>
  | <:ctyp< string >> -> 
      <:expr< match $id$ with 
          [ Sqlite3.Data.TEXT x -> x | _ -> failwith "TODO" ] >>
  | <:ctyp< bool >> ->  
      <:expr< match $id$ with 
          [ Sqlite3.Data.INT 1L -> True | Sqlite3.Data.INT 0L -> False | _ -> failwith "TODO" ] >>
  | <:ctyp< option $t$ >> ->
      <:expr<
         match $id$ with [
         Sqlite3.Data.NULL -> None
         | x -> Some ($fn t$)
         ]
      >>
  | _ -> <:expr< "FAIL" >>
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
  | _ -> failwith "to_string: unknown type"
  in
  if f.f_info = Internal_autoid then
    <:expr< Int64.to_string $id$ >>
  else 
    fn f.f_ctyp
 
let ocaml_variant_to_sql_request _loc f =
  let id = <:expr< $lid:f.f_name$ >> in
  let pid = <:patt< $lid:f.f_name$ >> in
  let int_like_type conv =
    <:expr< match $id$ with [ 
        `Eq  i -> Printf.sprintf $str:f.f_name ^ "=%s" $ ($conv$ i)
      | `Neq i -> Printf.sprintf $str:f.f_name ^ "!=%s"$ ($conv$ i)
      | `Le  i -> Printf.sprintf $str:f.f_name ^ "<%s" $ ($conv$ i)
      | `Leq i -> Printf.sprintf $str:f.f_name ^ "<=%s"$ ($conv$ i)
      | `Ge  i -> Printf.sprintf $str:f.f_name ^ ">%s" $ ($conv$ i)
      | `Geq i -> Printf.sprintf $str:f.f_name ^ ">=%s"$ ($conv$ i)
      | `Between (b,e) ->
                  Printf.sprintf $str:f.f_name^">%s AND "^f.f_name^"<%s"$ ($conv$ b) ($conv$ e) ] >> in
  let rec fn = function
  | <:ctyp@loc< unit >>   -> <:expr< $str:f.f_name^"=1"$ >>
  | <:ctyp@loc< int >>    -> int_like_type <:expr< $lid:"string_of_int"$ >>
  | <:ctyp@loc< int32 >>  -> int_like_type <:expr< $uid:"Int32"$.$lid:"to_string"$ >>
  | <:ctyp@loc< int64 >>  -> int_like_type <:expr< $uid:"Int64"$.$lid:"to_string"$ >>
  | <:ctyp@loc< float >>  -> int_like_type <:expr< $lid:"string_of_float"$ >>
  | <:ctyp@loc< char >>   -> int_like_type <:expr< $uid:"Char"$.$lid:"escaped"$ >>
  | <:ctyp@loc< string >> -> 
      <:expr< match $id$ with [ 
          `Eq e           -> Printf.sprintf $str:f.f_name^"='%s'"$ e
        | `Contains e     -> $str:f.f_name^" like '%"$ ^ e ^ "%'" ]
      >>
  | <:ctyp@loc< bool >>   -> <:expr< Printf.sprintf $str:f.f_name^"=%b"$ $id$ >>
  | <:ctyp@loc< option $t$ >> ->
      <:expr< match $id$ with [
          `Is_none        -> $str:f.f_name^" IS NULL"$
        | `Exists $pid$   -> $fn t$ ]
      >>
  | _                     -> <:expr< assert False >>
  in
  fn f.f_ctyp

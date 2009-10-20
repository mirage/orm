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
open P4_utils

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
  |List_items     (* table with actual contents of lists *)
  |Variant of variant_info (* table is a variant list *)
  |Optional
  |Tuple

type field_info =
  |External_and_internal_field
  |Internal_field
  |Internal_autoid
  |External_foreign of (string * string option)
and env = {
  e_tables: table list;
  e_types: (string * ctyp) list;
  e_indices: (bool * string * string list) list;
  e_name: string;
  debug_sql: bool;
  debug_binds: bool;
  debug_cache: bool;
  debug_dot: string option;
  e_ctyp: ctyp;
}
and table = {
  t_name: string;
  t_fields: field list;
  t_type: table_type;
  t_ctyp: ctyp;
  t_child: string list; (* sub-tables created from this one *)
  t_parent: string option;
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
  |Int,Internal_autoid -> "INTEGER"
  |Int,_ -> "INTEGER"
  |Real,_ -> "REAL"
  |Text,_ -> "TEXT"
  |Blob,_ -> "BLOB"
  |Null,_ -> "NULL"    

let string_of_field_info = function
  |External_and_internal_field -> "E+I"
  |Internal_field -> "I"
  |External_foreign (f,t) -> sprintf "F[%s%s]" f 
      (match t with |None ->"??" |Some x -> "")
  |Internal_autoid -> "A"

let string_of_table_type = function
 |Exposed -> "Exposed" |List -> "List" 
 |List_items -> "List_items" |Variant _ -> "Variant" 
 |Optional -> "Optional" |Tuple -> "Tuple"

let string_of_field f =
  sprintf "%s (%s):%s" f.f_name (string_of_field_info f.f_info) (string_of_sql_type f)
let string_of_table t =
  sprintf "%-30s / %-10s (child=%s): [ %s ] " t.t_name (string_of_table_type t.t_type)
    (String.concat ", " t.t_child) (string_map ", " string_of_field t.t_fields)
let string_of_env e =
  List.iter (fun (n,t) -> eprintf "%s : " n; debug_ctyp t) e.e_types;
  string_map "\n" string_of_table e.e_tables 

let dot_of_table env t =
  let record_fields t =
    String.concat " | " (sprintf "<%s> %s (%s)" t.t_name t.t_name (string_of_table_type t.t_type) :: List.rev_map (fun f -> 
      sprintf "<%s> %s (%s)" f.f_name f.f_name (string_of_field_info f.f_info)) t.t_fields) in
  let table_child t = 
    String.concat "\n" (List.map (fun t' -> sprintf "\"%s\":%s -> \"%s\":%s [color=red];" 
      t.t_name t.t_name t' t') t.t_child) in
  let record_of_table t =
    sprintf "\"%s\" [\nlabel = \"%s\"\nshape = \"record\"\nstyle=rounded];" 
      t.t_name (record_fields t) in
  let edges_of_table t =
    String.concat "\n" (List.fold_left (fun a f ->
      match f.f_info with
      |External_foreign (id,_) -> 
        sprintf "\"%s\":%s -> \"%s\":id [penwidth=1];" t.t_name f.f_name id :: a
      |_ -> a
      ) [] t.t_fields) in
  sprintf "%s\n%s%s" (record_of_table t) (edges_of_table t) (table_child t)

let dot_of_env e =
  sprintf "digraph ORM { graph [ rankdir=\"LR\" overlap=false ];\n%s }" (String.concat "\n" (List.map (dot_of_table e) e.e_tables))

(* --- Helper functions to manipulate environment *)

let empty_env ctyp = { e_tables = []; e_types=[]; e_indices=[]; e_name="orm";
  debug_sql=false; debug_binds=false; debug_cache=false; debug_dot=None; e_ctyp=ctyp }

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
    |None -> { t_name=name; t_fields=[]; t_type=ty; t_ctyp=ctyp; t_child=[]; t_parent=parent }
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
  let _loc = loc_of_ctyp ctyp in
  if field_name = "" then failwith ("empty field name for " ^ t);
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

(* return index in list or Not_found *)
exception Found_item of int
let listi fn l = try 
    ignore(List.fold_left (fun a b -> 
      if fn b then raise (Found_item a) else a+1
    ) 0 l); 
    raise Not_found 
  with Found_item x -> x

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
    |Exposed |List |Tuple |List_items |Optional |Variant _ -> true
  ) env.e_tables

let tables_no_list_item env =
  List.filter (fun t ->
    match t.t_type with
    |List_items -> false
    |Exposed | List |Tuple |Optional |Variant _ -> true
  ) env.e_tables

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

let list_or_option_fields env =
  filter_fields_with_table (fun f ->
    match f.f_info with
    |External_foreign (_,(Some ft)) -> 
      with_table (fun env ft' -> 
          match ft'.t_type with
          |List |Optional -> true
          |_ -> false
        ) env ft
    |_ -> false
  ) env

(* list of fields suitable for external ocaml interface
   with autoid filtered out (e.g. to construct objects) *)
let exposed_fields_no_autoid =
   filter_fields_with_table (fun f ->
     match f.f_info with
     |External_and_internal_field
     |External_foreign _ -> true
     |Internal_autoid
     |Internal_field -> false
   )


(* list of fields suitable for SQL statements *)
let sql_fields =
   filter_fields_with_table (fun f ->
     true
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
     f.f_info <> Internal_autoid
   )

let sql_fields_no_internal =
  filter_fields_with_table (fun f ->
    not (f.f_info = Internal_autoid || f.f_info = Internal_field)
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

(* retrieve the single value field from a list table *)
let list_item_field =
  with_table (fun env table ->
    assert(table.t_type = List_items);
    match List.filter (fun f -> f.f_name = "_item") table.t_fields with
    |[f] -> f
    |[] -> failwith (sprintf "list_item_type: %s: no entry" table.t_name)
    |_ -> failwith (sprintf "list_item_type: %s: multiple entries" table.t_name)
  )

let ctyp_of_table t =
  let _loc = loc_of_ctyp t.t_ctyp in
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
      let fields = match t.t_type with
      |List -> with_table (fun env t' -> t'.t_fields) env
         (match t.t_child with [x] -> x |_ -> assert false)
      |_ -> t.t_fields in
      List.iter (fun f ->
        match f.f_info with
        |External_foreign (id,(Some t')) ->
          with_table (fun env t -> fn t) env t'
        |External_foreign (_,None) -> 
          failwith (sprintf "incomplete type for EF: %s" (string_of_field f))
        |_ -> ()
      ) fields
    end
  in
  try fn t; false
  with Is_recursive -> true
    
let field_accessor f =
  let _loc = loc_of_ctyp f.f_ctyp in
  <:expr< $lid:"_"^f.f_name$ >>

(* --- Name functions *)

let savefn t    = sprintf "__%s_save" t.t_name
let extsavefn t = sprintf "%s_save" t.t_name

let getfn  t    = sprintf "__%s_get" t.t_name
let extgetfn t  = sprintf "%s_get"  t.t_name
let tidfn t     = sprintf "%s__id"    t.t_name
let tridfn t    = sprintf "%s__val"   t.t_name
let tnewfn t    = sprintf "%s__new"   t.t_name
let fidfn  f    = sprintf "%s__id_%s" f.f_table f.f_name
let fcachefn t  = sprintf "C_%s"      t.t_name
let fautofn env t = fidfn (auto_id_field env t.t_name)
let whashfn t   = sprintf "W_%s"      t.t_name
let rhashfn t   = sprintf "R_%s"      t.t_name
let tparentidfn t = match t.t_parent with 
  |None ->  failwith "no parent table"
  |Some p -> "_"^p^"_id"

let whashex fn t =
  let _loc = loc_of_ctyp t.t_ctyp in
  <:expr< $uid:whashfn t$.$lid:fn$ db.OS.cache.$lid:tidfn t$ >>
let rhashex fn t =
  let _loc = loc_of_ctyp t.t_ctyp in
  <:expr< $uid:rhashfn t$.$lid:fn$ db.OS.cache.$lid:tridfn t$ >>

(* --- Process functions to convert OCaml types into SQL *)

let rec process tds env =
  let _loc = loc_of_ctyp tds in
  let env = process_type_declarations tds env in
  check_foreign_refs env

and process_type_declarations ctyp env =
  let _loc = loc_of_ctyp ctyp in
  let rec fn ty env =
    match ty with
    |Ast.TyAnd (_loc, tyl, tyr) ->
       (* two types, so process both and add to list *)
       fn tyl (fn tyr env)
    |Ast.TyDcl (_loc, id, _, ty, []) ->
       (* we ignore type variables for the moment *)
       process_toplevel_type id ty env
    |_ -> failwith "process_type_declarations: unexpected type"
   in fn ctyp env

and process_toplevel_type n ctyp env =
  let _loc = loc_of_ctyp ctyp in
  match ctyp with
  | <:ctyp@loc< < $fs$ > >>
  | <:ctyp@loc< { $fs$ } >> ->
    (* add a new table to the environment *)
    let env = new_table ~name:n ~ty:Exposed ~ctyp ~parent:None env in
    let env = add_field ~ctyp:<:ctyp< option int64 >> ~info:(Internal_autoid) env n "id" Int in
    let rec fn env = function
    | <:ctyp< $t1$; $t2$ >> -> fn (fn env t1) t2
    | <:ctyp< $lid:id$ : mutable $t$ >>
    | <:ctyp< $lid:id$ : $t$ >> ->  process_type n id t env
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
       process_type n id t env
    | <:ctyp< $uid:id$ >> ->
       register_id id false;
       env
    | _ -> failwith "unknown variant AST"
    in fn env row_fields
  | _ -> failwith "process_toplevel_type: unknown type"

and process_type t n ctyp env =
  let _loc = loc_of_ctyp ctyp in
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
  | <:ctyp@loc< option $ty$ >> -> 
     let name = sprintf "%s_%s__o" t n in
     let env = add_field ~ctyp ~info:(External_foreign(name,None)) env t n Int in
     let env = new_table ~name ~ty:Optional ~ctyp ~parent:(Some t) env in
     let env = add_field ~ctyp:<:ctyp< option int64 >> ~info:Internal_autoid env name "id" Int in
     let env = add_field ~ctyp:<:ctyp< bool >> ~info:Internal_field env name "_isset" Int in
     process_type name n ty env
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
       process_type name tuple_name ctyp env
     ) env tys
  | <:ctyp@loc< $lid:id$ >> -> begin
     add_field ~ctyp ~info:(External_foreign (id,None)) env t n Int
  end  
  | <:ctyp@loc< list $ctyp$ >>
  | <:ctyp@loc< array $ctyp$ >> as orig_ctyp ->
    (* add field to current table marking the list as a foreign *)
    let name = sprintf "%s_%s" t n in
    let env = add_field ~ctyp:orig_ctyp ~info:(External_foreign (name,None)) env t n Int in
    (* construct a table which holds the list ids to mark list identity *)
    let env = new_table ~name ~ty:List ~ctyp:orig_ctyp ~parent:(Some t) env in 
    let env = add_field ~ctyp:<:ctyp< option int64 >> ~info:Internal_autoid env name "id" Int in
    (* construct a table which holds the actual list items *)
    let name_items = sprintf "%s_items" name in
    let env = new_table ~name:name_items ~ty:List_items ~ctyp ~parent:(Some name) env in
    let env = add_field ~ctyp:<:ctyp< int64 >> ~info:Internal_field env name_items "id" Int in
    let env = add_field ~ctyp:<:ctyp< int64 >> ~info:Internal_field env name_items "_idx" Int in
    process_type name_items "_item" ctyp env 
  | x -> 
    debug_ctyp x;
    failwith "unknown type"

(* run through the fully-populated environment and check that all
   foreign references do in fact exist, and if not convert them to 
   opaque sexp types *)
and check_foreign_refs env =
  let tables = List.map (fun t ->
    let fields = List.map (fun f ->
      match f.f_info with
      | External_foreign (id,None) -> begin
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

let debug env ty n e =
  let _loc = Loc.ghost in
  let d () = <:binding< () = prerr_endline ($str:n^": "$ ^ $e$) >> in
  let b () = <:binding< () = () >> in
  if (match ty with
    |`Sql -> env.debug_sql
    |`Cache -> env.debug_cache
    |`Binds -> env.debug_binds
  ) then d() else b()

let field_to_sql_data f =
  let _loc = loc_of_ctyp f.f_ctyp in
  let id = <:expr< $lid:"_" ^ f.f_name$ >> in
  let rec fn = function
  | <:ctyp@loc< unit >>   -> <:expr< Sqlite3.Data.INT 1L >>
  | <:ctyp@loc< int >>    -> <:expr< Sqlite3.Data.INT (Int64.of_int $id$) >>
  | <:ctyp@loc< int32 >>  -> <:expr< Sqlite3.Data.INT (Int64.of_int32 $id$) >>
  | <:ctyp@loc< int64 >>  -> <:expr< Sqlite3.Data.INT $id$ >>
  | <:ctyp@loc< float >>  -> <:expr< Sqlite3.Data.FLOAT $id$ >>
  | <:ctyp@loc< char >>   -> <:expr< Sqlite3.Data.INT (Int64.of_int (Char.code $id$)) >>
  | <:ctyp@loc< string >> -> <:expr< Sqlite3.Data.TEXT $id$ >>
  | <:ctyp@loc< bool >>   ->  <:expr< Sqlite3.Data.INT (if $id$ then 1L else 0L) >>
  | ctyp ->
    <:expr< Sqlite3.Data.INT $lid:"_"^f.f_name^"_id"$ >>
  in fn f.f_ctyp

let empty_list_expr_of_ctyp = function
  | <:ctyp@loc< list $_$ >> -> <:expr@loc< [] >>
  | <:ctyp@loc< array $_$ >> -> <:expr@loc< [| |] >>
  | _ -> assert false

let empty_list_patt_of_ctyp = function
  | <:ctyp@loc< list $_$ >> -> <:patt@loc< [] >>
  | <:ctyp@loc< array $_$ >> -> <:patt@loc< [| |] >>
  | _ -> assert false

let sql_data_to_field ~null_foreigns env f =
  let _loc = loc_of_ctyp f.f_ctyp in
  let error = <:match_case< x -> failwith ("unexpected res: " ^ (Sqlite3.Data.to_string x)) >> in
  let id  = <:expr< $lid:"__" ^ f.f_name$ >> in
  let rec fn = function
  | <:ctyp< unit >> -> <:expr< () >>
  | <:ctyp< int >> -> 
      <:expr< match $id$ with 
          [ Sqlite3.Data.INT x -> Int64.to_int x | $error$ ] >>
  | <:ctyp< int32 >> ->
      <:expr< match $id$ with 
          [ Sqlite3.Data.INT x -> Int64.to_int32 x | $error$] >>
  | <:ctyp< int64 >> ->
      <:expr< match $id$ with 
          [ Sqlite3.Data.INT x -> x | $error$ ] >>
  | <:ctyp< float >> -> 
      <:expr< match $id$ with 
          [ Sqlite3.Data.FLOAT x -> x | $error$] >>
  | <:ctyp< char >> -> 
      <:expr< match $id$ with 
          [ Sqlite3.Data.INT x -> Char.chr (Int64.to_int x) | $error$ ] >>
  | <:ctyp< string >> -> 
      <:expr< match $id$ with 
          [ Sqlite3.Data.TEXT x -> x | $error$ ] >>
  | <:ctyp< bool >> ->  
      <:expr< match $id$ with 
          [ Sqlite3.Data.INT 1L -> True | Sqlite3.Data.INT 0L -> False | $error$] >>
  | _ -> <:expr< "FAIL" >>
  in
  match f.f_info with
  |External_foreign (_,(Some ft)) ->
    with_table (fun env ft -> 
      match ft.t_type, null_foreigns with
      | Optional,true ->
        <:expr< None >>
      | List, true -> empty_list_expr_of_ctyp f.f_ctyp
      | _ ->
        <:expr< 
          match $id$ with [ 
            Sqlite3.Data.INT id -> 
              match $lid:getfn ft$ ~id:(`Id id) db with [ 
                [x] -> x 
              | [] -> failwith "no results" 
              | _ -> failwith "too many results for id search" ]
          | _ -> assert False ] 
        >>
    ) env ft
  |_ -> fn f.f_ctyp

let to_string f =
  let _loc = loc_of_ctyp f.f_ctyp in
  let id = <:expr< $lid:f.f_name$ >> in
  let rec fn = function
  | <:ctyp@loc< unit >> -> <:expr< "1" >>
  | <:ctyp@loc< int >> -> <:expr< string_of_int $id$ >>
  | <:ctyp@loc< int32 >> -> <:expr< Int32.to_string $id$ >>
  | <:ctyp@loc< int64 >> -> <:expr< Int64.to_string $id$ >>
  | <:ctyp@loc< float >> -> <:expr< string_of_float $id$ >>
  | <:ctyp@loc< char >> -> <:expr< String.make 1 $id$ >>
  | <:ctyp@loc< string >> -> <:expr< $id$ >>
  | <:ctyp@loc< bool >> ->  <:expr< string_of_bool $id$ >>
  | _ -> <:expr< "???" >>
  in
  if f.f_info = Internal_autoid then
    <:expr< Int64.to_string $id$ >>
  else 
    fn f.f_ctyp
 
let ocaml_variant_to_sql_request f =
  let _loc = loc_of_ctyp f.f_ctyp in
  let id = <:expr< $lid:f.f_name$ >> in
  let int_like_type conv =
    <:expr< match $id$ with [ 
        `Eq  _ -> $str:f.f_name^" = ?"$
      | `Neq _ -> $str:f.f_name ^ " != ?"$
      | `Le  _ -> $str:f.f_name ^ " < ?"$
      | `Leq _ -> $str:f.f_name ^ " <= ?"$
      | `Ge  _ -> $str:f.f_name ^ " > ?"$
      | `Geq _ -> $str:f.f_name ^ " >= ?"$
      | `Between _ ->
          $str:sprintf "%s >= ? AND %s <= ?" f.f_name f.f_name$ ] >> in
  let rec fn = function
  | <:ctyp@loc< unit >>   -> <:expr< $str:f.f_name^"=1"$ >>
  | <:ctyp@loc< int >>    -> int_like_type <:expr< $lid:"string_of_int"$ >>
  | <:ctyp@loc< int32 >>  -> int_like_type <:expr< $uid:"Int32"$.$lid:"to_string"$ >>
  | <:ctyp@loc< int64 >>  -> int_like_type <:expr< $uid:"Int64"$.$lid:"to_string"$ >>
  | <:ctyp@loc< char >>   -> int_like_type <:expr< $uid:"Char"$.$lid:"escaped"$ >>
  | <:ctyp@loc< float >>  -> 
      <:expr< match $id$ with [
          `Geq _ -> $str:f.f_name ^ " >= ?"$
        | `Leq _ -> $str:f.f_name ^ " <= ?"$
        | `Between _ -> $str:sprintf "%s >= ? AND %s <= ?" f.f_name f.f_name$ ] >>
  | <:ctyp@loc< string >> -> 
      <:expr< match $id$ with [ 
          `Eq _           -> $str:f.f_name^"=?"$
        | `Contains _     -> $str:f.f_name^" like '%?%'"$ ]
      >>
  | <:ctyp@loc< bool >>   -> <:expr< $str:f.f_name^"=?"$ >>
  | _                     -> <:expr< assert False >>
  in
  match f.f_info with
  |Internal_autoid ->
     <:expr< match $id$ with [
        `Id _ -> $str:f.f_name ^ "=?"$
      ] >>
  |External_foreign _ ->
     <:expr< match $id$ with [
        `Id _ -> $str:f.f_name ^ "=?"$
       |`Eq _ -> $str:f.f_name ^ "=?"$
      ] >>
  |_ -> fn f.f_ctyp

let ocaml_variant_to_sql_binds env f =
  let _loc = loc_of_ctyp f.f_ctyp in
  let id = <:expr< $lid:f.f_name$ >> in
  let bind e = <:expr<
     incr sql_bind_pos;
     let __e = $e$ in
     let $debug env `Binds "bind" <:expr< string_of_int !sql_bind_pos ^ " <- "
                                     ^ (Sqlite3.Data.to_string __e) >>$ in
     OS.db_must_bind db stmt !sql_bind_pos __e >> in
  let int_like_type conv =  <:expr< match $id$ with [
     `Eq i | `Neq i | `Le i | `Leq i |`Ge i |`Geq i ->
        do { $bind <:expr< Sqlite3.Data.INT $conv "i"$ >>$ }
   | `Between (l,u) -> 
        do { $bind <:expr< Sqlite3.Data.INT $conv "l"$>>$;
             $bind <:expr< Sqlite3.Data.INT $conv "u"$>>$; } ] >> in
  let fn = function
  | <:ctyp@loc< unit >>   -> <:expr< () >>
  | <:ctyp@loc< int >>    -> int_like_type (fun i -> <:expr< Int64.of_int $lid:i$ >>)
  | <:ctyp@loc< int32 >>  -> int_like_type (fun i -> <:expr< Int64.of_int32 $lid:i$ >>)
  | <:ctyp@loc< int64 >>  -> int_like_type (fun i -> <:expr< $lid:i$ >>)
  | <:ctyp@loc< char >>   -> int_like_type (fun i -> <:expr< Int64.of_int (Char.code $lid:i$) >>)
  | <:ctyp@loc< float >>  -> 
      <:expr< match $id$ with [
          `Leq i |`Geq i ->  do { $bind <:expr< Sqlite3.Data.FLOAT i >>$ }
        | `Between (l,u) ->  do { $bind <:expr< Sqlite3.Data.FLOAT l >>$;
                                  $bind <:expr< Sqlite3.Data.FLOAT u >>$; } ] >> 
  | <:ctyp@loc< string >> -> 
      <:expr< match $id$ with [ 
          `Eq e           -> do { $bind <:expr< Sqlite3.Data.TEXT e >>$ }
        | `Contains e     -> do { $bind <:expr< Sqlite3.Data.TEXT e >>$ } ]
      >>
  | <:ctyp@loc< bool >>   -> 
      <:expr< do { $bind <:expr< 
        if $id$ then Sqlite3.Data.INT 1L else Sqlite3.Data.INT 0L >>$ } >>
  | _                     -> <:expr< assert False >>
  in
  match f.f_info with
  |Internal_autoid ->
     <:expr< match $id$ with [
        `Id i -> do { $bind <:expr< Sqlite3.Data.INT i >>$ }
      ] >>
  |External_foreign (_,(Some ft)) ->
     with_table (fun env ft ->
       <:expr< match $id$ with [
          `Id i -> do { $bind <:expr< Sqlite3.Data.INT i >>$ }
         |`Eq x -> 
             let i = try 
               $uid:whashfn ft$.find db.OS.cache.$lid:tidfn ft$ x 
             with [ Not_found -> Int64.minus_one ] in
             do { $bind <:expr< Sqlite3.Data.INT i >>$ }
        ] >>
     ) env ft 
  |_ -> fn f.f_ctyp


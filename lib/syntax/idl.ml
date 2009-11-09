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
open Type

(* --- Type definitions *)
(** The type of tables *)
type table_info =
  | Toplevel (** top-level table                                             *)
  | Normal   (** table is normal, ie. it has an autoid field and some fields *)
  | List     (** table contains list values                                  *)
  | Variant  (** table contains variant values                               *)

(** The type of fields *)
type foreign = Internal | External
type data = F_unit | F_int | F_int32 | F_int64 | F_float | F_bool | F_char | F_string
type field_info =
  | Autoid                      (** field contains the unique auto-ID          *)
  | Data of data                (** field contains simple data                 *)
  | Foreign of foreign * string (** field contains a link to an internal table *)
  | List_counter                (** used by list tables only                   *)
  | Row_name of (string * int) list (** used by variant tables only            *)

(** The environment type *)
and env = {
  e_tables: table list;                  
  e_indices: (bool * string * string list) list;
  e_name: string;
  debug_leak: bool;
  debug_sql: bool;
  debug_binds: bool;
  debug_cache: bool;
  debug_dot: string option;
  e_loc: Camlp4.PreCast.Ast.loc;
  e_type: (Camlp4.PreCast.Ast.loc * string * t) list;
  e_aliases: (string * string) list  (* Alias table for foreign tables *)
}
and table = {
  t_name: string;
  t_exposed: bool;
  t_fields: field list;
  t_info: table_info;
  t_child: string list; (* sub-tables created from this one *)
  t_parent: string list;
  t_loc: Camlp4.PreCast.Ast.loc;
}
and field = {
  f_optional: bool;
  f_name: string;
  f_info: field_info;
  f_table: string;
  f_loc: Camlp4.PreCast.Ast.loc;
}

(* --- String conversion functions *)

let string_map del fn v =
  String.concat del (List.map fn v)

module String_of = struct

  let data = function
    | F_unit   -> "UNIT"
    | F_int    -> "INT"
    | F_int32  -> "INT32"
    | F_int64  -> "INT64"
    | F_float  -> "FLOAT"
    | F_bool   -> "BOOL"
    | F_char   -> "CHAR"
    | F_string -> "STRING"

  let foreign = function
    | External  -> "E"
    | Internal  -> "I"

  let field_info = function
    | Autoid        -> "a"
    | Data s        -> sprintf "D(%s)" (data s)
    | Foreign (f,s) -> sprintf "F%s(%s)" (foreign f) s
    | List_counter  -> "c"
    | Row_name _    -> "n"

  let table_info = function
    | Toplevel -> "Toplevel"
    | Normal   -> "Normal"
    | List     -> "List" 
    | Variant  -> "Variant"

  let field f =
    sprintf "%s: %s" f.f_name (field_info f.f_info)

  let table t =
    sprintf "%-30s %-10s %s (child=%s) [ %s ]"
      t.t_name
      (table_info t.t_info)
      (if t.t_exposed then "E" else "I")
      (String.concat ", " t.t_child)
      (string_map ", " field t.t_fields)

  let alias (t1, t2) =
    sprintf "(%s, %s)" t1 t2

  let env e =
    sprintf "TABLES:\n%s\nALIASES: { %s }"
      (string_map "\n" table e.e_tables)
      (string_map ", " alias e.e_aliases)

end

let error env (s:('a,unit,string,'b) format4) =
  kprintf (fun s ->
    eprintf "ERROR:%s\n%s\n" s (String_of.env env);
    (* Printexc.print_backtrace stderr; *)
    exit (-1)
    ) s

(* return index in list or Not_found *)
(*exception Found_item of int
exception Not_found
let listi fn l = try 
    ignore(List.fold_left (fun a b -> 
      if fn b then raise (Found_item a) else a+1
    ) 0 l); 
    raise Not_found 
  with Found_item x -> x
*)

(* --- Accessor functions to filter the environment *)
module Env = struct

  let toplevel_table_name = "__toplevel__"  

  let toplevel_table ctyp = {
    t_name    = toplevel_table_name;
    t_exposed = false;
    t_fields  = [];
    t_info    = Toplevel;
    t_child   = [];
    t_parent  = [];
    t_loc     = Camlp4.PreCast.Ast.loc_of_ctyp ctyp;
  }

  let empty ctyp = {
    e_tables    = [ toplevel_table ctyp ];
    e_indices   = [];
    e_name      = "orm";
    debug_leak  = false;
    debug_sql   = false;
    debug_binds = false;
    debug_cache = false;
    debug_dot   = None;
    e_type      = P4_type.create ctyp;
    e_loc       = Camlp4.PreCast.Ast.loc_of_ctyp ctyp;
    e_aliases   = [];
    }

  let mem_table env name =
    List.exists (fun t -> t.t_name = name || List.mem_assoc t.t_name env.e_aliases) env.e_tables

  let find_table env name =
    try List.find (fun t -> t.t_name = name || List.mem_assoc t.t_name env.e_aliases) env.e_tables
    with _ -> error env "Table %s not found" name

  (* replace the table in the env and return new env *)
  let replace_table env table =
    { env with e_tables = table :: 
      (List.filter (fun t -> t.t_name <> table.t_name) env.e_tables)
    }

  exception Loc of Camlp4.PreCast.Ast.loc
  let loc_of_table env t_name =
    try List.iter (fun (l,n,_) -> if n=t_name then raise (Loc l)) env.e_type; error env "Cannot find the location of table %s" t_name
    with Loc l -> l

  (* helper fn to lookup a table in the env and apply a function to it *)
  let with_table fn env t =
    fn env (find_table env t)

  let assert_alias_does_not_exist env t_name =
    if List.mem_assoc t_name env.e_aliases then
      error env "Alias %s already exists for table %s" (List.assoc t_name env.e_aliases) t_name

  (* make the table t1 become the parent of the table t2 in env *)
  let link_table ~env ~parent ~child =
    let t1 = find_table env parent in
    let t2 = find_table env child in
    let env =
      if not (List.mem t2.t_name t1.t_child) then
        replace_table env { t1 with t_child = t2.t_name :: t1.t_child }
      else env in
    let env =
      if not (List.mem t1.t_name t2.t_parent) then
        replace_table env { t2 with t_parent = t1.t_name :: t2.t_parent }
      else env in
    env

  let add_table_alias ~env ~t_name ~alias =
    if alias = toplevel_table_name || t_name = alias then
      env
    else begin
      assert_alias_does_not_exist env t_name;
      { env with e_aliases = (t_name, alias) :: env.e_aliases }
    end

  let remove_table_alias ~env ~t_name ~alias =
    { env with e_aliases = List.remove_assoc t_name env.e_aliases }

  let with_table_alias ~env ~t_name ~alias fn =
    let env = add_table_alias ~env ~t_name ~alias in
    let env = fn env in
    remove_table_alias ~env ~t_name ~alias

  let mem_alias env t =
    List.mem_assoc t env.e_aliases

  let find_alias env t =
    if mem_alias env t then
      List.assoc t env.e_aliases
    else
      error env "Table %s does not have an alias" t

end

module Field = struct

  let data_of_t env = function
    | Unit   -> F_unit
    | Int    -> F_int
    | Int32  -> F_int32
    | Int64  -> F_int64
    | Float  -> F_float
    | String -> F_string
    | Char   -> F_char
    | Bool   -> F_bool
    | t      -> error env "Failed to convert type %s to data" (to_string t)

  let mem_field ~env ~t_name ~f_name =
    let table = Env.find_table env t_name in
    List.exists (fun f -> f.f_name = f_name) table.t_fields

  let assert_field_does_not_exist ~env ~t_name ~f_name =
    if mem_field ~env ~t_name ~f_name then
      error env "field %s is not unique in %s" f_name t_name

  (* add field to the specified table, and return a modified env *)
  let add ~env ~t_name ~f_name ~f_info =
    let table = Env.find_table env t_name in
    if f_name = "" then
      error env "empty field name for %s" table.t_name;
    let field = {
      f_optional = f_info = Autoid;
      f_name     = f_name;
      f_info     = f_info;
      f_table    = table.t_name;
      f_loc      = table.t_loc;
    } in
    (* sanity check that the name is unique in the field list *)
    assert_field_does_not_exist ~env ~t_name ~f_name;
    Env.replace_table env { table with t_fields = field :: table.t_fields }

  let add_list_counter ~env ~t_name =
    add ~env ~t_name ~f_name:"__idx__" ~f_info:List_counter

  let add_row_name ~env ~t_name s =
    let info = List.map (fun (n,sl) -> n, List.length sl) s in
    add ~env ~t_name ~f_name:"__row_name__" ~f_info:(Row_name info)

  let add_foreign_internal ~env ~t_name ~f_name ~foreign =
    let env = Env.link_table ~env ~parent:t_name ~child:foreign in
    add ~env ~t_name ~f_name ~f_info:(Foreign (Internal, foreign))

  let add_foreign_external ~env ~t_name ~f_name ~foreign =
    let env = Env.link_table ~env ~parent:t_name ~child:foreign in
    add ~env ~t_name ~f_name ~f_info:(Foreign (External, foreign))

  let add_autoid ~env ~t_name =
    add ~env ~t_name ~f_name:"__id__" ~f_info:Autoid

  let set_optional ~env ~t_name ~f_name =
    let table = Env.find_table env t_name in
    let fields = List.map (fun f -> if f.f_name = f_name then { f with f_optional = true } else f) table.t_fields in
    Env.replace_table env { table with t_fields = fields }

  let is_autoid env f =
    match f.f_info with
    | Autoid -> true
    | _      -> false

  let is_foreign env f =
    match f.f_info with
    | Foreign _ -> true
    | _         -> false

  let get_foreign env f =
    match f.f_info with
    | Foreign (_,t) -> Env.find_table env t
    | _             -> error env "%s.%s is not a foreign field" f.f_table f.f_name

(*  let is_internal f = not (is_exposed f)

  let is_optional env f =
    is_foreign f && match (get_foreign env f).t_type with
    | Optional -> true
    | _ -> false

  let is_list env f =
    is_foreign f && match (get_foreign env f).t_type with
    | List -> true
    | _ -> false

  let is_variant env f =
    is_foreign f && match (get_foreign env f).t_type with
    | Variant _ -> true
    | _ -> false

  let is_tuple env f =
    is_foreign f && match (get_foreign env f).t_type with
    | Tuple -> true
    | _ -> false

  let internal = filter_fields_with_table is_internal
  let autoid = filter_fields_with_table is_autoid
  let no_autoid = filter_fields_with_table (fun f -> not (is_autoid f))
  let optional env = filter_fields_with_table (is_optional env) env
  let is_variant env = filter_fields_with_table (is_variant env) env
  let tuple env = filter_fields_with_table (is_tuple env) env

  (* list of fields suitable for SQL statements *)
  let get_all = filter_fields_with_table (fun f -> true)

  (* retrieve the single Auto_id field from a table *)
  let get_autoid env =
    match autoid env with
    | [f] -> f
    | [] -> error env "auto_id_field: %s: no entry" table.t_name
    | _ -> error env "auto_id_field: %s: multiple entries" table.t_name

  let field_accessor f =
    let _loc = loc_of_ctyp f.f_ctyp in
    <:expr< $lid:"_"^f.f_name$ >>
*)
end

(* list of tables for top-level code generation *)
module Table = struct

  let replace = Env.replace_table
  let find = Env.find_table
  let mem = Env.mem_table
  let link = Env.link_table

  (* filter the fields of a specific table *)
  let filter fn env t : field list =
    Env.with_table (fun env table -> List.filter fn table.t_fields) env t

  let no_autoid_fields env t = filter (fun f -> not (Field.is_autoid env f)) env t

  let foreign_fields env t = filter (Field.is_foreign env) env t

  let assert_table_does_not_exist env t_name =
    if mem env t_name then
      error env "Table %s already exists" t_name

  let add ~env ~t_name ~t_info ~t_exposed ~parent =
    (* check if the table does not already exists *)
    assert_table_does_not_exist env t_name;
    let table = {
      t_name    = t_name;
      t_fields  = [];
      t_info    = t_info;
      t_child   = [];
      t_parent  = [];
      t_exposed = t_exposed;
      t_loc     = Camlp4.PreCast.Loc.ghost; (* TODO: Env.loc_of_table env t_name; *)
    } in
    let env = { env with e_tables = table :: env.e_tables } in
    let env = Field.add_autoid ~env ~t_name in
    let p_table = fst parent in
    let p_field = snd parent in
    if p_table = Env.toplevel_table_name then
      env
    else
      Field.add_foreign_internal ~env ~t_name:p_table ~f_name:p_field ~foreign:t_name

(*  let is_exposed t = t.t_type = Exposed
  let internal t = t.t_type <> Exposed
  let exposed env = List.filter is_exposed env.e_tables
  let internal env = List.filter is_exposed env.e_tables
  let get_all env = env.e_tables

  let ctyp t =
    let _loc = loc_of_ctyp t.t_ctyp in
    match t.t_type with
    | Exposed | Variant _ -> <:ctyp< $lid:t.t_name$ >>
    |_ -> t.t_ctyp

  (* follow all the links in a table fields to determine
     if it ever points back to itself or not *)
  exception Is_recursive
  let is_recursive env t =
    let h = Hashtbl.create 1 in
    let rec aux t =
      if Hashtbl.mem h t.t_name then
        raise Is_recursive
      else begin
        Hashtbl.add h t.t_name ();
        List.iter (fun c -> with_table (fun env t -> aux t) env c) t.t_child;
      end
  in
  try fn t; false
  with Is_recursive -> true
*)
end


(* output dot files *)
module Dot_of = struct

  let field f =
    sprintf "<%s> %s (%s)" f.f_name f.f_name (String_of.field_info f.f_info)

  let table_summary t =
    sprintf "<%s> %s (%s)" t.t_name t.t_name (String_of.table_info t.t_info)

  let record_of_fields t =
    String.concat " | " (table_summary t :: List.map field t.t_fields)

  let child parent child =
    sprintf "\"%s\":%s -> \"%s\":%s [color=red];" parent.t_name parent.t_name child.t_name child.t_name

  let childs env t = 
    string_map "\n" (fun c -> child t (Table.find env c)) t.t_child

  let record_of_table t =
    sprintf "\"%s\" [\nlabel = \"%s\"\nshape = \"record\"\nstyle=rounded];" t.t_name (record_of_fields t)
  
  let edge_foreign env t f =
    sprintf "\"%s\":%s -> \"%s\":id [penwidth=1];" t.t_name f.f_name (Field.get_foreign env f).t_name

  let edges_of_table env t =
    string_map "\n" (edge_foreign env t) (Table.foreign_fields env t.t_name)

  let table env t =
    sprintf "%s\n%s%s" (record_of_table t) (edges_of_table env t) (childs env t)

  let env e =
    sprintf "digraph ORM { graph [ rankdir=\"LR\" overlap=false ];\n%s }" (String.concat "\n" (List.map (table e) e.e_tables))
end

(* --- Process functions to convert OCaml types into SQL *)

let foldi fn accu l =
  let accu, _ = List.fold_left (fun (accu, i) x -> fn accu i x, i + 1) (accu, 0) l in accu

let (>>) s t  = if s = Env.toplevel_table_name then t else sprintf "%s__%s" s t
let (>>>) s t = sprintf "%s__%i" s t

let rec process_sig ~env ~t_name ~t_exposed ~name s =
(*  Printf.eprintf "env = %s\nt_name = %s; t_exposed = %b; name = %s\ns = %s\n"
    (String_of.env env) t_name t_exposed name (Type.to_string s); *)

  match s with
  | Unit | Int | Int32 | Int64 | Bool | Float | Char | String ->
     (* Simpler case: just add a new field to the current table *)
     Field.add ~env ~t_name ~f_name:name ~f_info:(Data (Field.data_of_t env s))

  | Product sl ->
    (* Create new tables containing the tuple values, and link them to the current table *)
    let env = Table.add ~env ~t_name:(t_name >> name) ~t_info:Normal ~t_exposed ~parent:(t_name, name) in
    foldi (fun env pos t ->
      process_sig ~env ~t_name:(t_name >> name) ~t_exposed:false ~name:(name >>> pos) t
      ) env sl

  | Collection s ->
    (* Create a new table containing an ordered collection of values, and link it to the current table *)
    let env = Table.add ~env ~t_name:(t_name >> name) ~t_info:List ~t_exposed ~parent:(t_name, name) in 
    let env = Field.add_list_counter ~env ~t_name:(t_name >> name) in
    process_sig ~env ~t_name:(t_name >> name) ~t_exposed:false ~name s

  | Named_product sl ->
    (* Create a new table containing the fields' values, and link them to the current table *)
    let env = Table.add ~env ~t_name:(t_name >> name) ~t_info:Normal ~t_exposed ~parent:(t_name, name) in
    List.fold_left (fun env (field_name, is_mutable, s) ->
      process_sig ~env ~t_name:(t_name >> name) ~t_exposed:false ~name:field_name s
      ) env sl

  | Named_sum sl ->
    (* Create a new table for each element of the sum type, and link them to the current table *)
    let env = Table.add ~env ~t_name:(t_name >> name) ~t_info:Variant ~t_exposed ~parent:(t_name, name) in
    let env = Field.add_row_name ~env ~t_name:(t_name >> name) sl in
    List.fold_left (fun env (row_name, sl) ->
      foldi (fun env pos s ->
        process_sig ~env ~t_name:(t_name >> name) ~t_exposed:false ~name:(row_name >>> pos) s
        ) env sl
      ) env sl

  | Option s ->
    let env = process_sig ~env ~t_name ~t_exposed ~name s in
    Field.set_optional ~env ~t_name ~f_name:name

  | Rec (v, s) ->
    Env.with_table_alias ~env ~t_name:v ~alias:(t_name >> name)
      (fun env -> process_sig ~env ~t_name ~t_exposed ~name s)

  | Var v      ->
    if Env.mem_alias env v then
	  Field.add_foreign_internal ~env ~t_name ~f_name:name ~foreign:(Env.find_alias env v)
    else
      Field.add_foreign_external ~env ~t_name ~f_name:name ~foreign:v

let process env =
  List.fold_left
    (fun env (_, name, s) -> process_sig ~env ~t_name:Env.toplevel_table_name ~t_exposed:true ~name s)
    env env.e_type

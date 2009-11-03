(*pp camlp4orf *)
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

open Camlp4
open PreCast
open Ast
open P4_utils

open Printf
open Type
open Idl

(* --- Name functions *)

module Name = struct

  let savefn t       = sprintf "__%s_save" t.t_name
  let extsavefn t    = sprintf "%s_save" t.t_name

  let getfn env t    = sprintf "__%s_get" t.t_name
  let extgetfn env t = sprintf "%s_get"   t.t_name

  let whashfn env t  = sprintf "W_%s" t.t_name
  let rhashfn env t  = sprintf "R_%s" t.t_name

  let tidfn env t    = sprintf "%s__id"  t.t_name
  let tridfn env t   = sprintf "%s__val" t.t_name
(*  let tnewfn t    = sprintf "%s__new"   t.t_name
  let fidfn  f    = sprintf "%s__id_%s" f.f_table f.f_name
  let fcachefn t  = sprintf "C_%s"      t.t_name
  let fautofn env t = fidfn (auto_id_field env t.t_name)
*)
  let count = ref 0
  let create () =
      let _loc = Loc.ghost in
      incr count; <:expr< $lid:sprintf "__var%i__" !count$ >>

end


let debug env ty n e =
  let _loc = Loc.ghost in
  let d () = <:binding< () = prerr_endline ($str:n^": "$ ^ $e$) >> in
  let b () = <:binding< () = () >> in
  if (match ty with
    | `Sql   -> env.debug_sql
    | `Cache -> env.debug_cache
    | `Binds -> env.debug_binds
  ) then d() else b()

let map_strings sep fn sl = String.concat sep (List.map fn sl)

module Save = struct

  let field ~env ~foreign ~recursive f =
    let _loc = f.f_loc in
    let id =  <:expr< $lid:f.f_table$ . $lid:f.f_name$ >> in
    match f.f_info with
    | Autoid                 -> <:expr< Sqlite3.Data.INT $id$ >>
    | Foreign (Internal , t) -> foreign env t
    | Foreign (External , t) -> foreign env t
    | Foreign (Recursive, t) -> recursive env t
    | Row_name _             -> <:expr< Sqlite3.Data.TEXT $id$ >>
    | List_counter           -> <:expr< Sqlite3.Data.INT $id$ >>
    | Data d                 ->
      match d with
      | F_unit   -> <:expr< Sqlite3.Data.INT 1L >>
      | F_int    -> <:expr< Sqlite3.Data.INT (Int64.of_int $id$) >>
      | F_int32  -> <:expr< Sqlite3.Data.INT (Int64.of_int32 $id$) >>
      | F_int64  -> <:expr< Sqlite3.Data.INT $id$ >>
      | F_float  -> <:expr< Sqlite3.Data.FLOAT $id$ >>
      | F_char   -> <:expr< Sqlite3.Data.INT (Int64.of_int (Char.code $id$)) >>
      | F_string -> <:expr< Sqlite3.Data.TEXT $id$ >>
      | F_bool   -> <:expr< Sqlite3.Data.INT (if $id$ then 1L else 0L) >>

  let bind ~env ~foreign ~recursive fields =
    let bind_one i f = 
      let _loc = f.f_loc in
      <:expr<
        let __e__ = $field ~env ~foreign ~recursive f$ in
        let $debug env `Binds "bind" <:expr< $str:string_of_int i$ ^ " <- " ^ Sqlite3.Data.to_string __e__ >>$ in
        OS.db_must_bind db __stmt__ $int:string_of_int i$ __e__
      >> in
    let exprs = mapi (fun i f -> bind_one i f) fields in
    exSem_of_list exprs

  (* the INSERT statement for a table. This let the recursive foreign fields blanked. *)
  let rec insert env t =
    let _loc = t.t_loc in
    let foreign env t = insert env (Table.find env t) in
    let recursive env t = <:expr< Sqlit3.Data.NULL >> in
    let fields = Field.no_autoid env t.t_name in
    <:expr< do {
      let __sql__ = sprintf "INSERT INTO %s (%s) VALUES(%s);"
        t.t_name
        (map_strings "," (fun f -> f.f_name) fields)
        (map_strings "," (fun f -> "?") fields) in
      let $debug env `Sql "insert" <:expr< __sql__ >>$ in
      let __stmt__ = Sqlite3.prepare db.OS.db __sql__ in
      $bind ~env ~foreign ~recursive fields$
      OS.db_must_step db __stmt__;
      let __id__ = Sqlite3.last_insert_rowid db.OS.db in
      $uid:Name.whashfn env t$.add db.OS.cache.$lid:Name.tidfn env t$ $lid:t.t_name$ __id__;
      $uid:Name.rhashfn env t$.add db.OS.cache.$lid:Name.tridfn env t$ __id__ $lid:t.t_name$;
      Sqlite3.Data.INT __id__
    } >>

(*  (* the UPDATE statement for a table *)
  let rec update env t =
    let foreign env t = update in
    let recursive =
      <:expr<
        let __id__ = $uid:Name.whashfn t$.find db.OS.cache.$lid:Name.tidfn t$ $lid:t.t_name$ in
        if Hashtbl.mem __cache__ ( $str:t.t_name$, __id__) then
            ()
        else do {
          Hashtbl.replace __cache__ ( $str:t.t_name$, __id__);
          $update_recursive_foreigns env t$ };
        Sqlite3.Data.INT __id__
      >> in 
        let fields = Field.recursive_foreigns env t @ [ Field.autoid env t ] in
        <:expr< do {
          let __sql__ = sprintf "UPDATE %s SET %s WHERE id=?;"
            t.t_name
            (map_strings "," (fun f -> sprintf "%s=?" f.f_name) fields) in
          let $debug env `Sql "update_aux" <:expr< __sql__ >>$ in
          let __stmt__ = Sqlite3.prepare db.OS.db __sql__ in
          $bind ~env ~foreign ~recursive fields$
          OS.db_must_step db __stmt__;
        } >> in
     aux t

   (* if there are no fields to update, then that would generate invalid 
      SQL above, so set no_update true so the code gen can skip update in this case *)
   let no_update = List.length t.t_fields = 1 in

    <:expr<
        let __id__ = $uid:Name.whashfn t$.find db.OS.cache.$lid:Name.tidfn t$ $lid:t.t_name$ in
        if Hashtbl.mem __cache__ ( $str:t_tname$, __id__ ) then
          __id__
        else (

          $lid:Name.savefn t$ ~__cache__ db >>
        
*)        

end

module Foo = struct
  let get ~env ~internal_table ~external_table ~recursive_table id f =
    let _loc = f.f_loc in
    let qerror = <:match_case< x -> failwith ("unexpected res: " ^ (Sqlite3.Data.to_string x)) >> in
    match f.f_info with
    | Autoid                 -> <:expr< match $id$ with [ Sqlite3.Data.INT x -> Some x | $qerror$ ] >>
    | Foreign (Internal , t) -> internal_table env id t
    | Foreign (External , t) -> external_table env id t
    | Foreign (Recursive, t) -> recursive_table env id t
    | Row_name _             -> <:expr< match $id$ with [ Sqlite3.Data.TEXT x -> x | $qerror$ ] >>
    | List_counter           -> <:expr< match $id$ with [ Sqlite3.Data.INT x -> x | $qerror$ ] >>
    | Data d                 ->
      match d with
      | F_unit   -> <:expr< () >>
      | F_int    -> <:expr< match $id$ with [ Sqlite3.Data.INT x -> Int64.to_int x | $qerror$ ] >>
      | F_int32  -> <:expr< match $id$ with [ Sqlite3.Data.INT x -> Int64.to_int32 x | $qerror$] >>
      | F_int64  -> <:expr< match $id$ with [ Sqlite3.Data.INT x -> x | $qerror$ ] >>
      | F_float  -> <:expr< match $id$ with [ Sqlite3.Data.FLOAT x -> x | $qerror$] >>
      | F_char   -> <:expr< match $id$ with [ Sqlite3.Data.INT x -> Char.chr (Int64.to_int x) | $qerror$ ] >>
      | F_string -> <:expr< match $id$ with [ Sqlite3.Data.TEXT x -> x | $qerror$ ] >>
      | F_bool   -> <:expr< match $id$ with [ Sqlite3.Data.INT 1L -> True | Sqlite3.Data.INT 0L -> False | $qerror$] >>

  let to_string ~env ~internal_table ~external_table ~recursive_table id f =
    let _loc = f.f_loc in
    match f.f_info with
    | Autoid                 -> <:expr< Int64.to_string $id$ >>
    | Foreign (Internal , t) -> internal_table env id t
    | Foreign (External , t) -> external_table env id t
    | Foreign (Recursive, t) -> recursive_table env id t
    | Row_name _             -> <:expr< $id$ >>
    | List_counter           -> <:expr< Int64.to_string $id$ >>
    | Data d                 -> 
      match d with
      | F_unit   -> <:expr< "1" >>
      | F_int    -> <:expr< string_of_int $id$ >>
      | F_int32  -> <:expr< Int32.to_string $id$ >>
      | F_int64  -> <:expr< Int64.to_string $id$ >>
      | F_float  -> <:expr< string_of_float $id$ >>
      | F_char   -> <:expr< String.make 1 $id$ >>
      | F_string -> <:expr< $id$ >>
      | F_bool   -> <:expr< string_of_bool $id$ >>

  let select ~env id f =
    let _loc = f.f_loc in
    let none =
      if f.f_optional then
        <:match_case< Some `None -> $str:f.f_name ^ "IS NULL"$ | None -> "" >>
      else
        <:match_case< None -> "" >> in
    match f.f_info with
    | Autoid                 -> <:expr< match $id$ with [ Some (`Id _) -> $str:f.f_name ^ "=?"$ | $none$ ] >>
    | Foreign (Internal , t) -> error env "select internal table %s" t
    | Foreign (External , t) -> error env "select external table %s" t
    | Foreign (Recursive, t) -> error env "select recursive table %s" t
    | Row_name s             -> failwith "TODO"
    | List_counter           -> error env "select list counter" 
    | Data d                 -> 
      match d with
      | F_unit -> <:expr< match $id$ with [ Some `Unit -> $str:f.f_name^"=1"$ | $none$ ] >>
      | F_bool -> <:expr< match $id$ with [ Some `Eq   -> $str:f.f_name^"=?"$ | $none$ ] >>
      | F_int | F_int32 | F_int64 | F_char | F_float ->
        <:expr< match $id$ with [ 
          Some (`Eq  _)     -> $str:f.f_name^" = ?"$
        | Some (`Neq _)     -> $str:f.f_name ^ " != ?"$
        | Some (`Le  _)     -> $str:f.f_name ^ " < ?"$
        | Some (`Leq _)     -> $str:f.f_name ^ " <= ?"$
        | Some (`Ge  _)     -> $str:f.f_name ^ " > ?"$
        | Some (`Geq _)     -> $str:f.f_name ^ " >= ?"$
        | Some (`Between _) -> $str:sprintf "%s >= ? AND %s <= ?" f.f_name f.f_name$ 
        | $none$ ] >>
      | F_string -> 
        <:expr< match $id$ with [ Some (`Eq _) -> $str:f.f_name^"=?"$ | Some (`Contains _) -> $str:f.f_name^" like '%?%'"$ ] >>



(*      <:expr< match $id$ with [
          `Id i -> do { $bind <:expr< Sqlite3.Data.INT i >>$ }
         |`Eq x -> 
             let i = try $uid:Name.whashfn env t$.find db.OS.cache.$lid:Name.tidfn env t$ x 
             with [ Not_found -> Int64.minus_one ] in
             do { $bind <:expr< Sqlite3.Data.INT i >>$ }
        ] >> *)

end

(*
module Table = struct

  let field_names fields = List.map (fun f -> sprintf "%s.%s" f.f_table f.f_name) fields

  let select ~env t =
    let _loc = t.t_loc in
    let fields = Field.no_autoid env t.t_name in

    let names = field_names fields in
    let select = match t.t_info with
      | Normal  -> sprintf "SELECT %s FROM %s" (String.concat "," names) t.t_name
      | List    -> sprintf "SELECT %s FROM %s ORDER BY __idx__" (String.concat "," names) t.t_name
      | Variant -> sprintf "SELECT %s FROM %s" (String.concat "," names) t.t_name in

   let where = List.map (fun f -> Field.select ~env <:expr< $lid:f.f_name$ >> f) fields in
    let where = "TODO" in
    let where = if where = "" then "" else " WHERE " ^ where in ()
 end 
*)

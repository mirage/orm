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

  let savefn t    = sprintf "__%s_save" t.t_name
  let extsavefn t = sprintf "%s_save" t.t_name

  let getfn env t    = sprintf "__%s_get" (Table.find env t).t_name
  let extgetfn env t = sprintf "%s_get"   (Table.find env t).t_name

  let whashfn env t  = sprintf "W_%s" (Table.find env t).t_name
  let rhashfn env t  = sprintf "R_%s" (Table.find env t).t_name

  let tidfn env t  = sprintf "%s__id"  (Table.find env t).t_name
(*  let tridfn env t = sprintf "%s__val"   t.t_name
  let tnewfn t    = sprintf "%s__new"   t.t_name
  let fidfn  f    = sprintf "%s__id_%s" f.f_table f.f_name
  let fcachefn t  = sprintf "C_%s"      t.t_name
  let fautofn env t = fidfn (auto_id_field env t.t_name)
*)
  let field f = sprintf "_%s" f.f_name
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

module Field = struct

  let save ~env ~internal_table ~external_table ~recursive_table id f =
    let _loc = f.f_loc in
    match f.f_info with
    | Autoid                 -> <:expr< Sqlite3.Data.INT $id$ >>
    | Foreign (Internal , t) -> <:expr< $internal_table env t id$ >>
    | Foreign (External , t) -> <:expr< $external_table env t id$ >>
    | Foreign (Recursive, t) -> <:expr< $recursive_table env t id$ >>
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

  let bind ~env id i f =
    let _loc = f.f_loc in

    let bind e = <:expr<
      let __e = $e$ in
      let $debug env `Binds "bind" <:expr< string_of_int $int:i$ ^ " <- " ^ Sqlite3.Data.to_string __e >>$ in
      OS.db_must_bind db stmt $int:i$ __e >> in

    let int_like_type t conv =
      <:expr< match $id$ with [
        `Eq i | `Neq i | `Le i | `Leq i |`Ge i |`Geq i ->
        do { $bind <:expr< Sqlite3.Data.$uid:t$ $conv "i"$ >>$ }
      | `Between (l,u) -> 
        do { $bind <:expr< Sqlite3.Data.$uid:t$ $conv "l"$ >>$;
             $bind <:expr< Sqlite3.Data.$uid:t$ $conv "u"$ >>$; } ] >> in

    match f.f_info with
    | Autoid                 -> <:expr< match $id$ with [ `Id i -> do { $bind <:expr< Sqlite3.Data.INT i >>$ } ] >>
    | Foreign (Internal , t) -> error env "bind internal table %s" t
    | Foreign (External , t) -> error env "bind external table %s" t
    | Foreign (Recursive, t) -> error env "bind recursive table%s" t
    | Row_name _             -> failwith "TODO"
    | List_counter           -> error env "bind list counter"
    | Data d                 ->
      match d with
      | F_unit   -> <:expr< () >>
      | F_int    -> int_like_type "INT" (fun i -> <:expr< Int64.of_int $lid:i$ >>)
      | F_int32  -> int_like_type "INT" (fun i -> <:expr< Int64.of_int32 $lid:i$ >>)
      | F_int64  -> int_like_type "INT" (fun i -> <:expr< $lid:i$ >>)
      | F_char   -> int_like_type "INT" (fun i -> <:expr< Int64.of_int (Char.code $lid:i$) >>)
      | F_float  -> int_like_type "FLOAT" (fun i -> <:expr< $lid:i$ >>)
      | F_bool   -> <:expr< do { $bind <:expr< if $id$ then Sqlite3.Data.INT 1L else Sqlite3.Data.INT 0L >>$ } >>
      | F_string -> 
        <:expr< match $id$ with [ 
            `Eq e       -> do { $bind <:expr< Sqlite3.Data.TEXT e >>$ }
          | `Contains e -> do { $bind <:expr< Sqlite3.Data.TEXT e >>$ } ]
        >>

(*      <:expr< match $id$ with [
          `Id i -> do { $bind <:expr< Sqlite3.Data.INT i >>$ }
         |`Eq x -> 
             let i = try $uid:Name.whashfn env t$.find db.OS.cache.$lid:Name.tidfn env t$ x 
             with [ Not_found -> Int64.minus_one ] in
             do { $bind <:expr< Sqlite3.Data.INT i >>$ }
        ] >> *)

end

module Table = struct

  let field_names fields = List.map (fun f -> sprintf "%s.%s" f.f_table f.f_name) fields

  let select ~env t =
    let _loc = t.t_loc in
    let fields = Idl.Field.exposed env t.t_name in

    let names = field_names fields in
    let select = match t.t_info with
      | Normal  -> sprintf "SELECT %s FROM %s" (String.concat "," names) t.t_name
      | List    -> sprintf "SELECT %s FROM %s ORDER BY __idx__" (String.concat "," names) t.t_name
      | Variant -> sprintf "SELECT %s FROM %s" (String.concat "," names) t.t_name in

    let where = List.map (fun f -> Field.select ~env <:expr< $lid:f.f_name$ >> f) fields in
    let where = "TODO" in
    let where = if where = "" then "" else " WHERE " ^ where in ()
end

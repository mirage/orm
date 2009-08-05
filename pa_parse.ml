(*
 * This file is part of orpc, OCaml signature to ONC RPC generator
 * Copyright (C) 2008-9 Skydeck, Inc
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA
 *)

open Camlp4.PreCast
open Ast
open Types
open Error

let parse_ident id =
  let ids = Ast.list_of_ident id [] in
  match List.rev ids with
    | <:ident< $lid:id$ >> :: uids ->
      let mdl =
        List.map
          (function <:ident< $uid:uid$ >> -> uid | _ -> assert false)
          (List.rev uids) in
      (mdl, id)
    | _ -> assert false

let rec parse_type t =
  match t with
    | <:ctyp@loc< unit >> -> Unit loc
    | <:ctyp@loc< int >> -> Int loc
    | <:ctyp@loc< int32 >> -> Int32 loc
    | <:ctyp@loc< int64 >> -> Int64 loc
    | <:ctyp@loc< float >> -> Float loc
    | <:ctyp@loc< bool >> -> Bool loc
    | <:ctyp@loc< char >> -> Char loc
    | <:ctyp@loc< string >> -> String loc

    | <:ctyp@loc< '$v$ >> -> Var (loc, v)

    | <:ctyp@loc< $id:id$ >> ->
      let (mdl, id) = parse_ident id in
      Apply (loc, mdl, id, [])

    (* I don't see how to do this one with quotations; $t1$ * $t2$
       gives both the TyTup and the TySta *)
    | TyTup (loc, ts) ->
        let rec parts = function
          | TySta (_, t1, t2) -> parts t1 @ parts t2
          | TyTup (_, t) -> parts t
          | t -> [ parse_type t ] in
        Tuple (loc, parts ts)

    | <:ctyp@loc< { $fs$ } >> ->
      let rec fields = function
        | <:ctyp< $t1$; $t2$ >> -> fields t1 @ fields t2
        | <:ctyp< $lid:id$ : mutable $t$ >> -> [ { f_id = id; f_mut = true; f_typ = parse_type t } ]
        | <:ctyp< $lid:id$ : $t$ >> -> [ { f_id = id; f_mut = false; f_typ = parse_type t } ]
        | t -> ctyp_error t "expected TySem or TyCol" in
      Record (loc, fields fs)

    (* syntax for TySum? *)
    | TySum (loc, ams) ->
        let rec arms = function
          | <:ctyp< $t1$ | $t2$ >> -> arms t1 @ arms t2
          | <:ctyp< $uid:id$ of $t$ >> ->
              let rec parts = function
                | <:ctyp< $t1$ and $t2$ >> -> parts t1 @ parts t2
                | t -> [ parse_type t ] in
              [ id, parts t ]
          | <:ctyp< $uid:id$ >> -> [ id, [] ]
          | t -> ctyp_error t "expected TyOr, TyOf, or TyId" in
        Variant (loc, arms ams)

    | TyVrnEq (loc, ams) | TyVrnInf (loc, ams) | TyVrnSup (loc, ams) ->
        let rec arms = function
          | <:ctyp< $t1$ | $t2$ >> -> arms t1 @ arms t2
          | <:ctyp< `$id$ of $t$ >> ->
              let rec parts = function
                | <:ctyp< $t1$ and $t2$ >> -> parts t1 @ parts t2
                | t -> [ parse_type t ] in
              [ Pv_of (id, parts t) ]
          | <:ctyp< `$id$ >> -> [ Pv_of (id, []) ]
          | t -> [ Pv_pv (parse_type t) ] in
        let kind = match t with
          | TyVrnEq _ -> Pv_eq
          | TyVrnInf _ -> Pv_inf
          | TyVrnSup _ -> Pv_sup
          | _ -> assert false in
        PolyVar (loc, kind, arms ams)

    | <:ctyp@loc< $t$ array >> -> Array (loc, parse_type t)
    | <:ctyp@loc< $t$ list >> -> List (loc, parse_type t)
    | <:ctyp@loc< $t$ option >> -> Option (loc, parse_type t)
    | <:ctyp@loc< $t$ ref >> -> Ref (loc, parse_type t)

    | <:ctyp@loc< $_$ $_$ >> ->
        let rec apps args = function
            (* TyApp is used for both tupled and nested type application *)
          | <:ctyp< $t2$ $t1$ >> -> apps (parse_type t2 :: args) t1

          | <:ctyp< $id:id$ >> ->
              let (mdl, id) = parse_ident id in
              Apply (loc, mdl, id, args)

          | t -> ctyp_error t "expected TyApp or TyId" in
        apps [] t

    | <:ctyp@loc< $t1$ -> $t2$ >> -> Arrow (loc, parse_type t1, parse_type t2)

    | t -> ctyp_error t "unsupported type"

let parse_typedef loc t =
  let rec types t a =
    match t with
      | TyAnd (_, t1, t2) -> types t1 (types t2 a)
      | TyDcl (loc, id, tvars, t, []) ->
          let tvars =
            List.map
              (function
                | TyQuo (_, v) -> v
                | t -> ctyp_error t "expected type variable")
              tvars in
          let eq, t =
            match t with
              | TyMan (_, TyId (_, eq), t) -> Some eq, t
              | _ -> None, t in
          let t = parse_type t in
          { td_loc = loc; td_vars = tvars; td_id = id; td_typ = t; td_eq = eq } ::a
    | t -> ctyp_error t "expected type declaration" in
  types t []

let parse_exception loc t =
  match t with
    | <:ctyp< $uid:id$ of $t$ >> ->
      let rec parts = function
        | <:ctyp< $t1$ and $t2$ >> -> parts t1 @ parts t2
        | t -> [ parse_type t ] in
      (loc, id, parts t )
    | <:ctyp< $uid:id$ >> -> (loc, id, [])
    | t -> ctyp_error t "expected TyOr, TyOf, or TyId"

let parse_val loc id t =
  let rec args t a =
    match t with
      | TyArr (_, t1, t2) ->
          let arg =
            match t1 with
              | TyLab (loc, label, t1) -> Labelled (loc, label, parse_type t1)
              | TyOlb (loc, label, t1) -> Optional (loc, label, parse_type t1)
              | _ -> Unlabelled (loc_of_ctyp t1, parse_type t1) in
          args t2 (arg :: a)
      | t -> List.rev a, parse_type t in
  match args t [] with
    | [], _ -> loc_error loc "function must have at least one argument"
    | args, ret -> (loc, id, args, ret)

type s = {
  typedefs : typedefs list;
  exceptions : exc list;
  funcs : func list;
  module_types : module_type list;
}

let rec parse_sig_items i a =
  match i with
    | SgNil _ -> a
    | SgSem (_, i1, i2) -> parse_sig_items i1 (parse_sig_items i2 a)
    | SgTyp (loc, t) -> { a with typedefs = parse_typedef loc t :: a.typedefs }
    | SgExc (loc, t) -> { a with exceptions = parse_exception loc t :: a.exceptions }
    | SgVal (loc, id, t) -> { a with funcs = parse_val loc id t :: a.funcs }
    | SgMty (loc, id, MtSig (_, i)) -> { a with module_types = parse_module_type loc id i :: a.module_types }
    | i -> sig_item_error i "expected type, function declaration, or module type"

and parse_module_type loc id i =
  let rec parse_sig_items i a =
    match i with
      | SgNil _ -> a
      | SgSem (_, i1, i2) -> parse_sig_items i1 (parse_sig_items i2 a)
      | SgVal (loc, id, t) -> parse_val loc id t :: a
      | i -> sig_item_error i "expected function declaration" in
  let kind =
    match id with
      | "Sync" -> Sync
      | "Async" -> Async
      | "Lwt" -> Lwt
      | _ -> loc_error loc "unknown interface kind" in
  (loc, kind, parse_sig_items i [])

let parse_interface i =
  let s = { typedefs = []; exceptions = []; funcs = []; module_types = [] } in
  let s = parse_sig_items i s in
  let { typedefs = typedefs; exceptions = excs; funcs = funcs; module_types = mts } = s in
  match s with
    | { funcs = []; module_types = (_, _, _::_)::_ } -> (typedefs, excs, funcs, mts)
    | { funcs = _::_; module_types = [] } -> (typedefs, excs, funcs, mts)
    | _ -> loc_error Loc.ghost "expected simple interface or modules interface"

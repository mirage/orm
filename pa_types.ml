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

type ident = string

type typ =
    | Abstract of Loc.t
    | Var of Loc.t * ident
    | Unit of Loc.t
    | Int of Loc.t
    | Int32 of Loc.t
    | Int64 of Loc.t
    | Float of Loc.t
    | Bool of Loc.t
    | Char of Loc.t
    | String of Loc.t
    | Tuple of Loc.t * typ list
    | Record of Loc.t * field list
    | Variant of Loc.t * (ident * (typ list)) list
    | PolyVar of Loc.t * polyvar_kind * polyvar_arm list
    | Array of Loc.t * typ
    | List of Loc.t * typ
    | Option of Loc.t * typ
    | Ref of Loc.t * typ
    | Apply of Loc.t * ident list * ident * typ list
    | Arrow of Loc.t * typ * typ

and field = {
  f_id : ident;
  f_mut : bool;
  f_typ : typ;
}

and polyvar_arm = Pv_of of ident * typ list | Pv_pv of typ

and polyvar_kind = Pv_eq | Pv_sup | Pv_inf

type argtyp =
    | Unlabelled of Loc.t * typ
    | Labelled of Loc.t * ident * typ
    | Optional of Loc.t * ident * typ

type typedef = {
  td_loc : Loc.t;
  td_vars : ident list;
  td_id : ident;
  td_typ : typ;
  td_eq : Ast.ident option;
}

type typedefs = typedef list

type exc = Loc.t * ident * (typ list)

type func = Loc.t * ident * (argtyp list) * typ

type interface_kind = Sync | Async | Lwt

type module_type = Loc.t * interface_kind * (func list)

type pre_interface = typedefs list * exc list * func list * module_type list

type mode = Simple | Modules of interface_kind list

type interface = typedefs list * exc list * func list * mode

let loc_of_typ = function
  | Abstract loc -> loc
  | Var (loc, _) -> loc
  | Unit loc -> loc
  | Int loc -> loc
  | Int32 loc -> loc
  | Int64 loc -> loc
  | Float loc -> loc
  | Bool loc -> loc
  | Char loc -> loc
  | String loc -> loc
  | Tuple (loc, _) -> loc
  | Record (loc, _) -> loc
  | Variant (loc, _) -> loc
  | PolyVar (loc, _, _) -> loc
  | Array (loc, _) -> loc
  | List (loc, _) -> loc
  | Option (loc, _) -> loc
  | Ref (loc, _) -> loc
  | Apply (loc, _, _, _) -> loc
  | Arrow (loc, _, _) -> loc

let g = Loc.ghost

(* Camlp4LocationStripper is suggestive but I can't figure out how to use it. *)
let rec strip_locs_typ = function
  | Abstract _ -> Abstract g
  | Var (_, id) -> Var (g, id)
  | Unit _ -> Unit g
  | Int _ -> Int g
  | Int32 _ -> Int32 g
  | Int64 _ -> Int64 g
  | Float _ -> Float g
  | Bool _ -> Bool g
  | Char _ -> Char g
  | String _ -> String g
  | Tuple (_, parts) -> Tuple (g, List.map strip_locs_typ parts)
  | Record (_, fields) -> Record (g, List.map (fun f -> { f with f_typ = strip_locs_typ f.f_typ }) fields)
  | Variant (_, arms) -> Variant (g, List.map (fun (id,ts) -> (id, List.map strip_locs_typ ts)) arms)
  | PolyVar (_, kind, arms) ->
      let arms =
        List.map
          (function Pv_of (id,ts) -> Pv_of (id, List.map strip_locs_typ ts) | pv -> pv)
          arms in
      PolyVar (g, kind, arms)
  | Array (_, t) -> Array (g, strip_locs_typ t)
  | List (_, t) -> List (g, strip_locs_typ t)
  | Option (_, t) -> Option (g, strip_locs_typ t)
  | Ref (_, t) -> Ref (g, strip_locs_typ t)
  | Apply (_, mdl, id, args) -> Apply (g, mdl, id, List.map strip_locs_typ args)
  | Arrow (_, t1, t2) -> Arrow (g, strip_locs_typ t1, strip_locs_typ t2)

let typ_of_argtyp = function
  | Unlabelled (_, t) -> t
  | Labelled (_, _, t) -> t
  | Optional (_, _, t) -> t

let typ_of_argtyp_option = function
  | Unlabelled (_, t) -> t
  | Labelled (_, _, t) -> t
  | Optional (loc, _, t) -> Option (loc, t)

let loc_of_argtyp = function
  | Unlabelled (loc, _) -> loc
  | Labelled (loc, _, _) -> loc
  | Optional (loc, _, _) -> loc

let strip_locs_argtyp = function
  | Unlabelled (_, t) -> Unlabelled (g, strip_locs_typ t)
  | Labelled (_, label, t) -> Labelled (g, label, strip_locs_typ t)
  | Optional (_, label, t) -> Optional (g, label, strip_locs_typ t)

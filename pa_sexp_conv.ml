(*pp camlp4orf *)

(* File: pa_sexp_conv.ml

    Copyright (C) 2005-

      Jane Street Holding, LLC
      Author: Markus Mottl
      email: mmottl\@janestcapital.com
      WWW: http://www.janestcapital.com/ocaml

   This file is derived from file "pa_tywith.ml" of version 0.45 of the
   library "Tywith".

   Tywith is Copyright (C) 2004, 2005 by

      Martin Sandin  <msandin@hotmail.com>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(* Pa_sexp_conv: Preprocessing Module for Automated S-expression Conversions *)

open StdLabels
open MoreLabels

open Printf
open Lexing

open Camlp4
open PreCast
open Ast
open Pa_type_conv

(* Utility functions *)

let ( *** ) f g x = f (g x)

let mk_rev_bindings _loc fps =
  let coll (i, bindings, patts, vars) fp =
    let name = "v" ^ string_of_int i in
    let var_expr = Gen.ide _loc name in
    let expr =
      match fp with
      | `Fun fun_expr -> <:expr< $fun_expr$ $var_expr$ >>
      | `Match matchings -> <:expr< match $var_expr$ with [ $matchings$ ] >>
    in
    let patt = Gen.idp _loc name in
    let bindings = <:binding< $patt$ = $expr$ and $bindings$ >> in
    i - 1, bindings, patt :: patts, var_expr :: vars
  in
  let n = List.length fps in
  let _, bindings, patts, expr =
    List.fold_left ~f:coll ~init:(n, BiNil _loc, [], []) fps
  in
  bindings, patts, expr

let mk_bindings _loc fps = mk_rev_bindings _loc (List.rev fps)

let unroll_cnv_fp _loc var = function
  | `Fun fun_expr -> <:expr< $fun_expr$ $var$ >>
  | `Match matchings -> <:expr< match $var$ with [ $matchings$ ] >>

let unroll_fun_matches _loc fp1 fp2 =
  match fp1, fp2 with
  | `Fun fun_expr1, `Fun fun_expr2 -> <:expr< $fun_expr1$ $fun_expr2$ >>
  | `Fun fun_expr, `Match matching -> <:expr< $fun_expr$ (fun [ $matching$ ]) >>
  | _ -> assert false  (* impossible *)

(* Generators for S-expressions *)

(* Generator for converters of OCaml-values to S-expressions *)
module Generate_sexp_of = struct
  let mk_abst_call _loc tn rev_path =
    <:expr< $id:Gen.ident_of_rev_path _loc (("sexp_of_" ^ tn) :: rev_path)$ >>

  (* Conversion of type paths *)
  let sexp_of_path_fun _loc id =
    match Gen.get_rev_id_path id [] with
    | ["unit"] -> <:expr< Sexplib.Conv.sexp_of_unit >>
    | ["bool"] -> <:expr< Sexplib.Conv.sexp_of_bool >>
    | ["string"] -> <:expr< Sexplib.Conv.sexp_of_string >>
    | ["char"] -> <:expr< Sexplib.Conv.sexp_of_char >>
    | ["int"] -> <:expr< Sexplib.Conv.sexp_of_int >>
    | ["float"] -> <:expr< Sexplib.Conv.sexp_of_float >>
    | ["int32"] -> <:expr< Sexplib.Conv.sexp_of_int32 >>
    | ["int64"] -> <:expr< Sexplib.Conv.sexp_of_int64 >>
    | ["nativeint"] -> <:expr< Sexplib.Conv.sexp_of_nativeint >>
    | ["big_int"; "Big_int"] -> <:expr< Sexplib.Conv.sexp_of_big_int >>
    | ["nat"; "Nat"] -> <:expr< Sexplib.Conv.sexp_of_nat >>
    | ["num"; "Num"] -> <:expr< Sexplib.Conv.sexp_of_num >>
    | ["ratio"; "Ratio"] -> <:expr< Sexplib.Conv.sexp_of_ratio >>
    | ["ref"] -> <:expr< Sexplib.Conv.sexp_of_ref >>
    | ["t"; "Lazy"] | ["lazy_t"] -> <:expr< Sexplib.Conv.sexp_of_lazy >>
    | ["option"] -> <:expr< Sexplib.Conv.sexp_of_option >>
    | ["list"] -> <:expr< Sexplib.Conv.sexp_of_list >>
    | ["array"] -> <:expr< Sexplib.Conv.sexp_of_array >>
    | ["t"; "Hashtbl"] -> <:expr< Sexplib.Conv.sexp_of_hashtbl >>
    | ["bigstring"] -> <:expr< Sexplib.Conv.sexp_of_bigstring >>
    | ["vec"] -> <:expr< Sexplib.Conv.sexp_of_vec >>
    | ["float32_vec"] -> <:expr< Sexplib.Conv.sexp_of_float32_vec >>
    | ["float64_vec"] -> <:expr< Sexplib.Conv.sexp_of_float64_vec >>
    | ["mat"] -> <:expr< Sexplib.Conv.sexp_of_mat >>
    | ["float32_mat"] -> <:expr< Sexplib.Conv.sexp_of_float32_mat >>
    | ["float64_mat"] -> <:expr< Sexplib.Conv.sexp_of_float64_mat >>
    | ["exn"] -> <:expr< Sexplib.Conv.sexp_of_exn >>
    | tn :: rev_path -> mk_abst_call _loc tn rev_path
    | [] -> assert false  (* impossible *)

  (* Conversion of types *)
  let rec sexp_of_type _loc = function
    | <:ctyp< sexp_opaque $_$ >> -> `Fun <:expr< Sexplib.Conv.sexp_of_opaque >>
    | <:ctyp< $tp1$ $tp2$ >> -> `Fun (sexp_of_appl_fun _loc tp1 tp2)
    | <:ctyp< ( $tup:tp$ ) >> -> sexp_of_tuple _loc tp
    | <:ctyp< '$parm$ >> -> `Fun (Gen.ide _loc ("_of_" ^ parm))
    | <:ctyp< $id:id$ >> -> `Fun (sexp_of_path_fun _loc id)
    | <:ctyp< $_$ -> $_$ >> -> `Fun <:expr< Sexplib.Conv.sexp_of_fun >>
    | <:ctyp< [< $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
    | <:ctyp< [= $row_fields$ ] >> -> sexp_of_variant _loc row_fields
    | <:ctyp< ! $parms$ . $poly_tp$ >> -> sexp_of_poly _loc parms poly_tp
    | _ ->
        prerr_endline (get_loc_err _loc "sexp_of_type: unknown type construct");
        exit 1

  (* Conversion of polymorphic types *)
  and sexp_of_appl_fun _loc tp1 tp2 =
    match sexp_of_type _loc tp1, sexp_of_type _loc tp2 with
    | `Fun fun_expr1, `Fun fun_expr2 -> <:expr< $fun_expr1$ $fun_expr2$ >>
    | `Fun fun_expr, `Match matching ->
        <:expr< $fun_expr$ (fun [ $matching$ ]) >>
    | _ -> assert false  (* impossible *)


  (* Conversion of tuples *)
  and sexp_of_tuple _loc tp =
    let fps = List.map ~f:(sexp_of_type _loc) (list_of_ctyp tp []) in
    let bindings, patts, vars = mk_bindings _loc fps in
    let in_expr = <:expr< Sexplib.Sexp.List $Gen.mk_expr_lst _loc vars$ >> in
    let expr = <:expr< let $bindings$ in $in_expr$ >> in
    `Match <:match_case< ( $tup:paCom_of_list patts$ ) -> $expr$ >>


  (* Conversion of variant types *)

  and mk_cnv_expr _loc tp =
    match sexp_of_type _loc tp with
    | `Fun fun_expr -> <:expr< $fun_expr$ >>
    | `Match matchings -> <:expr< fun el -> match el with [ $matchings$ ] >>

  and sexp_of_variant _loc row_fields =
    let rec loop = function
      | <:ctyp< $tp1$ | $tp2$ >> -> <:match_case< $loop tp1$ | $loop tp2$ >>
      | <:ctyp< `$cnstr$ >> ->
          <:match_case< `$cnstr$ -> Sexplib.Sexp.Atom $str:cnstr$ >>
      | <:ctyp< `$cnstr$ of $tps$ >> ->
          let fps = List.map ~f:(sexp_of_type _loc) (list_of_ctyp tps []) in
          let bindings, patts, vars = mk_bindings _loc fps in
          let cnstr_expr = <:expr< Sexplib.Sexp.Atom $str:cnstr$ >> in
          let expr =
            <:expr<
              let $bindings$ in
              Sexplib.Sexp.List $Gen.mk_expr_lst _loc (cnstr_expr :: vars)$
            >>
          in
          <:match_case< `$cnstr$ $paSem_of_list patts$ -> $expr$ >>
      | <:ctyp< [= $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
      | <:ctyp< [< $row_fields$ ] >> -> loop row_fields
      | <:ctyp< $tp1$ $tp2$ >> ->
          let id_path = Gen.get_appl_path _loc tp1 in
          let call = sexp_of_appl_fun _loc tp1 tp2 in
          <:match_case< #$id_path$ as v -> $call$ v >>
      | <:ctyp< $id:id$ >> | <:ctyp< #$id:id$ >> ->
          let call =
            match Gen.get_rev_id_path id [] with
            | tn :: rev_path -> mk_abst_call _loc tn rev_path
            | [] -> assert false  (* impossible *)
          in
          <:match_case< #$id$ as v -> $call$ v >>
      | _ -> failwith "sexp_of_variant: unknown type"
    in
    `Match (loop row_fields)


  (* Polymorphic record fields *)

  and sexp_of_poly _loc parms tp =
    let bindings =
      let mk_binding parm =
        <:binding<
          $Gen.idp _loc ("_of_" ^ parm)$ = Sexplib.Conv.sexp_of_opaque
        >>
      in
      List.map ~f:mk_binding (Gen.ty_var_list_of_ctyp parms [])
    in
    match sexp_of_type _loc tp with
    | `Fun fun_expr -> `Fun <:expr< let $list:bindings$ in $fun_expr$ >>
    | `Match matchings ->
        `Match
          <:match_case<
            arg ->
              let $list:bindings$ in
              match arg with
              [ $matchings$ ]
          >>


  (* Conversion of sum types *)

  let rec branch_sum _loc = function
    | <:ctyp< $tp1$ | $tp2$ >> ->
        <:match_case< $branch_sum _loc tp1$ | $branch_sum _loc tp2$ >>
    | <:ctyp< $uid:cnstr$ >> ->
        <:match_case< $uid:cnstr$ -> Sexplib.Sexp.Atom $str:cnstr$ >>
    | <:ctyp< $uid:cnstr$ of $tps$ >> ->
        let fps = List.map ~f:(sexp_of_type _loc) (list_of_ctyp tps []) in
        let cnstr_expr = <:expr< Sexplib.Sexp.Atom $str:cnstr$ >> in
        let bindings, patts, vars = mk_bindings _loc fps in
        let patt =
          match patts with
          | [patt] -> patt
          | _ -> <:patt< ( $tup:paCom_of_list patts$ ) >>
        in
        <:match_case<
          $uid:cnstr$ $patt$ ->
            let $bindings$ in
            Sexplib.Sexp.List $Gen.mk_expr_lst _loc (cnstr_expr :: vars)$
        >>
    | _ -> failwith "branch_sum: unknown type"

  let sexp_of_sum _loc alts = `Match (branch_sum _loc alts)


  (* Conversion of record types *)

  let mk_rec_patt _loc patt name =
    let p = <:patt< $lid:name$ = $lid:"v_" ^ name$ >> in
    <:patt< $patt$; $p$ >>

  let sexp_of_record _loc flds_ctyp =
    let flds = list_of_ctyp flds_ctyp [] in
    let rec coll (patt, expr) = function
      | <:ctyp< $lid:name$ : mutable sexp_option $tp$ >>
      | <:ctyp< $lid:name$ : sexp_option $tp$ >> ->
          let patt = mk_rec_patt _loc patt name in
          let vname = <:expr< v >> in
          let cnv_expr = unroll_cnv_fp _loc vname (sexp_of_type _loc tp) in
          let expr =
            <:expr<
              let bnds =
                match $lid:"v_" ^ name$ with
                [ None -> bnds
                | Some v ->
                    let arg = $cnv_expr$ in
                    let bnd =
                      Sexplib.Sexp.List [Sexplib.Sexp.Atom $str:name$; arg]
                    in
                    [ bnd :: bnds ] ]
              in
              $expr$
            >>
          in
          patt, expr
      | <:ctyp< $lid:name$ : mutable sexp_list $tp$ >>
      | <:ctyp< $lid:name$ : sexp_list $tp$ >> ->
          let patt = mk_rec_patt _loc patt name in
          let cnv_expr =
            match sexp_of_type _loc tp with
            | `Fun fun_expr -> <:expr< $fun_expr$ >>
            | `Match matchings ->
                <:expr< fun el -> match el with [ $matchings$ ] >>
          in
          let expr =
            let v_name = <:expr< $lid: "v_" ^ name$ >> in
            <:expr<
              let bnds =
                if $v_name$ = [] then bnds
                else
                  let arg = sexp_of_list $cnv_expr$ $v_name$ in
                  let bnd =
                    Sexplib.Sexp.List [Sexplib.Sexp.Atom $str:name$; arg]
                  in
                  [ bnd :: bnds ]
              in
              $expr$
            >>
          in
          patt, expr
      | <:ctyp< $lid:name$ : mutable $tp$ >>
      | <:ctyp< $lid:name$ : $tp$ >> ->
          let patt = mk_rec_patt _loc patt name in
          let vname = <:expr< $lid:"v_" ^ name$ >> in
          let cnv_expr = unroll_cnv_fp _loc vname  (sexp_of_type _loc tp) in
          let expr =
            <:expr<
              let arg = $cnv_expr$ in
              let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom $str:name$; arg] in
              let bnds = [ bnd :: bnds ] in
              $expr$
            >>
          in
          patt, expr
      | _ -> assert false  (* impossible *)
    in
    let init_expr = <:expr< Sexplib.Sexp.List bnds >> in
    let patt, expr = List.fold_left ~f:coll ~init:(<:patt<>>, init_expr) flds in
    `Match
      <:match_case<
        { $patt$ } ->
          let bnds = [] in
          $expr$
      >>


  (* Empty type *)
  let sexp_of_nil _loc = `Fun <:expr< fun _v -> assert False >>


  (* Generate code from type definitions *)

  let sexp_of_td _loc type_name tps rhs =
    let is_alias_ref = ref false in
    let handle_alias _loc tp = is_alias_ref := true; sexp_of_type _loc tp in
    let body =
      let rec loop _loc =
        Gen.switch_tp_def _loc
          ~alias:handle_alias
          ~sum:sexp_of_sum
          ~record:sexp_of_record
          ~variants:sexp_of_variant
          ~mani:(fun _loc _tp1 -> loop _loc)
          ~nil:sexp_of_nil
      in
      match loop _loc rhs with
      | `Fun fun_expr ->
          (* Prevent violation of value restriction through eta-expansion *)
          if !is_alias_ref && tps = [] then <:expr< fun [ v -> $fun_expr$ v ] >>
          else <:expr< $fun_expr$ >>
      | `Match matchings -> <:expr< fun [ $matchings$ ] >>
    in
    let patts =
      List.map ~f:(Gen.idp _loc *** (^) "_of_" *** Gen.get_tparam_id) tps
    in
    let bnd = Gen.idp _loc ("sexp_of_" ^ type_name) in
    <:binding< $bnd$ = $Gen.abstract _loc patts body$ >>

  let rec sexp_of_tds = function
    | TyDcl (_loc, type_name, tps, rhs, _cl) ->
        sexp_of_td _loc type_name tps rhs
    | TyAnd (_loc, tp1, tp2) ->
        <:binding< $sexp_of_tds tp1$ and $sexp_of_tds tp2$ >>
    | _ -> assert false  (* impossible *)

  let sexp_of tds =
    let binding, recursive, _loc =
      match tds with
      | TyDcl (_loc, type_name, tps, rhs, _cl) ->
          sexp_of_td _loc type_name tps rhs,
          Gen.type_is_recursive _loc type_name rhs, _loc
      | TyAnd (_loc, _, _) as tds -> sexp_of_tds tds, true, _loc
      | _ -> assert false  (* impossible *)
    in
    if recursive then <:str_item< value rec $binding$ >>
    else <:str_item< value $binding$ >>

  let string_of_ident id =
    let str_lst = Gen.get_rev_id_path id [] in
    String.concat ~sep:"." str_lst

  let sexp_of_exn tp =
    let _loc = loc_of_ctyp tp in
    let get_full_cnstr cnstr = get_conv_path () ^ "." ^ cnstr in
    let cnstr, patt, expr =
      match tp with
      | <:ctyp< $uid:cnstr$ >> ->
          (
            cnstr,
            <:patt< $uid:cnstr$ >>,
            <:expr< Sexplib.Sexp.Atom $str: get_full_cnstr cnstr $ >>
          )
      | <:ctyp< $uid:cnstr$ of $tps$ >> ->
          let fps = List.map ~f:(sexp_of_type _loc) (list_of_ctyp tps []) in
          let cnstr_expr =
            <:expr< Sexplib.Sexp.Atom $str: get_full_cnstr cnstr $ >>
          in
          let bindings, patts, vars = mk_bindings _loc fps in
          let patt =
            match patts with
            | [patt] -> <:patt< $uid:cnstr$ $patt$ >>
            | _ -> <:patt< ( $uid:cnstr$ $tup:paCom_of_list patts$ ) >>
          in
          let expr =
            <:expr<
              let $bindings$ in
              Sexplib.Sexp.List $Gen.mk_expr_lst _loc (cnstr_expr :: vars)$
            >>
          in
          cnstr, patt, expr
      | _ -> failwith "sexp_of_exn: unknown type"
    in
    let lccnstr = String.uncapitalize cnstr in
    <:str_item<
      value $lid: "exn_id_sexp_of_" ^ lccnstr $ =
        Sexplib.Conv.add_exn_converter (fun
          [ $patt$ -> Some $expr$
          | _ -> None ])
    >>

end


(* Generator for converters of S-expressions to OCaml-values *)
module Generate_of_sexp = struct
  let mk_abst_call _loc tn ?(internal = false) rev_path =
    let tns = tn ^ "_of_sexp" in
    let tns_suff = if internal then tns ^ "__" else tns in
    <:expr< $id:Gen.ident_of_rev_path _loc (tns_suff :: rev_path)$ >>


  (* Utility functions for polymorphic variants *)

  (* Handle backtracking when variants do not match *)
  let handle_no_variant_match _loc expr =
    <:match_case< Sexplib.Conv_error.No_variant_match _ -> $expr$ >>

  let is_wildcard = function [_] -> true | _ -> false

  (* Generate code depending on whether to generate a match for the last
     case of matching a variant *)
  let handle_variant_match_last _loc match_last matches =
    if match_last || is_wildcard matches then
      match matches with
      | <:match_case< $_$ -> $expr$ >> :: _ -> expr
      | _ -> assert false  (* impossible *)
    else <:expr< match atom with [ $list:matches$ ] >>

  (* Generate code for matching malformed S-expressions *)
  let mk_variant_other_matches _loc rev_els call =
    let coll_structs acc (_loc, cnstr) =
      <:match_case< $str:cnstr$ -> Sexplib.Conv_error.$lid:call$ _loc sexp >>
        :: acc
    in
    let exc_no_variant_match =
      <:match_case< _ -> Sexplib.Conv_error.no_variant_match _loc sexp >>
    in
    List.fold_left ~f:coll_structs ~init:[exc_no_variant_match] rev_els

  (* Split the row fields of a variant type into lists of atomic variants,
     structured variants, atomic variants + included variant types,
     and structured variants + included variant types. *)
  let rec split_row_field _loc (atoms, structs, ainhs, sinhs as acc) = function
    | <:ctyp< `$cnstr$ >> ->
        let tpl = _loc, cnstr in
        (
          tpl :: atoms,
          structs,
          `A tpl :: ainhs,
          sinhs
        )
    | <:ctyp< `$cnstr$ of $tps$ >> ->
        (
          atoms,
          (_loc, cnstr) :: structs,
          ainhs,
          `S (_loc, cnstr, tps) :: sinhs
        )
    | <:ctyp< [= $row_fields$ ] >>
    | <:ctyp< [> $row_fields$ ] >>
    | <:ctyp< [< $row_fields$ ] >> ->
        List.fold_left ~f:(split_row_field _loc)
          ~init:acc (list_of_ctyp row_fields [])
    | <:ctyp< $_$ $_$ >>
    | (<:ctyp< $id:_$ >> | <:ctyp< #$id:_$ >>) as inh ->
        let iinh = `I (_loc, inh) in
        (
          atoms,
          structs,
          iinh :: ainhs,
          iinh :: sinhs
        )
    | _ -> failwith "split_row_field: unknown type"

  (* Conversion of type paths *)
  let path_of_sexp_fun _loc id =
    match Gen.get_rev_id_path id [] with
    | ["unit"] -> <:expr< Sexplib.Conv.unit_of_sexp >>
    | ["string"] -> <:expr< Sexplib.Conv.string_of_sexp >>
    | ["int"] -> <:expr< Sexplib.Conv.int_of_sexp >>
    | ["float"] -> <:expr< Sexplib.Conv.float_of_sexp >>
    | ["bool"] -> <:expr< Sexplib.Conv.bool_of_sexp >>
    | ["int32"] -> <:expr< Sexplib.Conv.int32_of_sexp >>
    | ["int64"] -> <:expr< Sexplib.Conv.int64_of_sexp >>
    | ["nativeint"] -> <:expr< Sexplib.Conv.nativeint_of_sexp >>
    | ["big_int"; "Big_int"] -> <:expr< Sexplib.Conv.big_int_of_sexp >>
    | ["nat"; "Nat"] -> <:expr< Sexplib.Conv.nat_of_sexp >>
    | ["num"; "Num"] -> <:expr< Sexplib.Conv.num_of_sexp >>
    | ["ratio"; "Ratio"] -> <:expr< Sexplib.Conv.ratio_of_sexp >>
    | ["list"] -> <:expr< Sexplib.Conv.list_of_sexp >>
    | ["array"] -> <:expr< Sexplib.Conv.array_of_sexp >>
    | ["option"] -> <:expr< Sexplib.Conv.option_of_sexp >>
    | ["char"] -> <:expr< Sexplib.Conv.char_of_sexp >>
    | ["t"; "Lazy"] | ["lazy_t"] -> <:expr< Sexplib.Conv.lazy_of_sexp >>
    | ["t"; "Hashtbl"] -> <:expr< Sexplib.Conv.hashtbl_of_sexp >>
    | ["ref"] -> <:expr< Sexplib.Conv.ref_of_sexp >>
    | ["bigstring"] -> <:expr< Sexplib.Conv.bigstring_of_sexp >>
    | ["vec"] -> <:expr< Sexplib.Conv.vec_of_sexp >>
    | ["float32_vec"] -> <:expr< Sexplib.Conv.float32_vec_of_sexp >>
    | ["float64_vec"] -> <:expr< Sexplib.Conv.float64_vec_of_sexp >>
    | ["mat"] -> <:expr< Sexplib.Conv.mat_of_sexp >>
    | ["float32_mat"] -> <:expr< Sexplib.Conv.float32_mat_of_sexp >>
    | ["float64_mat"] -> <:expr< Sexplib.Conv.float64_mat_of_sexp >>
    | tn :: rev_path -> mk_abst_call _loc tn rev_path
    | [] -> assert false  (* no empty paths *)

  (* Conversion of types *)
  let rec type_of_sexp _loc = function
    | <:ctyp< sexp_opaque $_$ >> -> `Fun <:expr< Sexplib.Conv.opaque_of_sexp >>
    | <:ctyp< sexp_option >> ->
        `Fun <:expr< fun a_of_sexp v -> Some (a_of_sexp v) >>
    | <:ctyp< sexp_list >> ->
        `Fun <:expr< fun a_of_sexp v -> list_of_sexp a_of_sexp v >>
    | <:ctyp< $tp1$ $tp2$ >> ->
        let fp1 = type_of_sexp _loc tp1 in
        let fp2 = type_of_sexp _loc tp2 in
        `Fun (unroll_fun_matches _loc fp1 fp2)
    | <:ctyp< ( $tup:tp$ ) >> -> tuple_of_sexp _loc tp
    | <:ctyp< '$parm$ >> -> `Fun (Gen.ide _loc ("_of_" ^ parm))
    | <:ctyp< $id:id$ >> -> `Fun (path_of_sexp_fun _loc id)
    | <:ctyp< $_$ -> $_$ >> -> `Fun <:expr< Sexplib.Conv.fun_of_sexp >>
    | <:ctyp< [< $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
    | <:ctyp< [= $row_fields$ ] >> ->
        variant_of_sexp _loc ?full_type:None row_fields
    | <:ctyp< ! $parms$ . $poly_tp$ >> -> poly_of_sexp _loc parms poly_tp
    | _ ->
        prerr_endline (get_loc_err _loc "type_of_sexp: unknown type construct");
        exit 1

  (* Conversion of tuples *)
  and tuple_of_sexp _loc tps =
    let fps = List.map ~f:(type_of_sexp _loc) (list_of_ctyp tps []) in
    let bindings, patts, vars = mk_bindings _loc fps in
    let n = string_of_int (List.length fps) in
    `Match
      <:match_case<
          Sexplib.Sexp.List $Gen.mk_patt_lst _loc patts$ ->
            let $bindings$ in
            ( $tup:exCom_of_list vars$ )
        | sexp -> Sexplib.Conv_error.tuple_of_size_n_expected _loc $int:n$ sexp
      >>

  (* Generate internal call *)
  and mk_internal_call _loc = function
    | <:ctyp< $id:id$ >> | <:ctyp< #$id:id$ >> ->
        let call =
          match Gen.get_rev_id_path id [] with
          | tn :: rev_path -> mk_abst_call _loc tn ~internal:true rev_path
          | [] -> assert false  (* impossible *)
        in
        call
    | <:ctyp< $tp1$ $tp2$ >> ->
        let fp1 = `Fun (mk_internal_call _loc tp1) in
        let fp2 = type_of_sexp _loc tp2 in
        unroll_fun_matches _loc fp1 fp2
    | _ -> assert false  (* impossible *)

  (* Generate code for matching included variant types *)
  and handle_variant_inh _loc full_type match_last other_matches inh =
    let fun_expr = mk_internal_call _loc inh in
    let match_exc =
      handle_no_variant_match _loc (
        handle_variant_match_last _loc match_last other_matches) in
    let new_other_matches =
      [
        <:match_case<
          _ -> try ($fun_expr$ sexp :> $full_type$) with [ $match_exc$ ]
        >>
      ]
    in
    new_other_matches, true

  (* Generate code for matching atomic variants *)
  and mk_variant_match_atom _loc full_type rev_atoms_inhs rev_structs =
    let coll (other_matches, match_last) = function
      | `A (_loc, cnstr) ->
          let new_match = <:match_case< $str:cnstr$ -> `$cnstr$ >> in
          new_match :: other_matches, false
      | `I (_loc, inh) ->
          handle_variant_inh _loc full_type match_last other_matches inh
    in
    let other_matches =
      mk_variant_other_matches _loc rev_structs "ptag_no_args"
    in
    let match_atoms_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_atoms_inhs in
    handle_variant_match_last _loc match_last match_atoms_inhs


  (* Variant conversions *)

  (* Match arguments of constructors (variants or sum types) *)
  and mk_cnstr_args_match _loc ~is_variant cnstr tps =
    let fps = List.map ~f:(type_of_sexp _loc) (list_of_ctyp tps []) in
    let bindings, patts, vars = mk_bindings _loc fps in
    let good_arg_match_expr =
      let vars_expr =
        match vars with
        | [var_expr] -> var_expr
        | _ -> <:expr< ( $tup:exCom_of_list vars$ ) >>
      in
      if is_variant then <:expr< `$cnstr$ $vars_expr$ >>
      else <:expr< $uid:cnstr$ $vars_expr$ >>
    in
    let handle_exc =
      if is_variant then "ptag_incorrect_n_args" else "stag_incorrect_n_args"
    in
    <:expr<
      match sexp_args with
      [ $Gen.mk_patt_lst _loc patts$ -> let $bindings$ in $good_arg_match_expr$
      | _ -> Sexplib.Conv_error.$lid:handle_exc$ _loc tag sexp ]
    >>

  (* Generate code for matching structured variants *)
  and mk_variant_match_struct _loc full_type rev_structs_inhs rev_atoms =
    let has_structs_ref = ref false in
    let coll (other_matches, match_last) = function
      | `S (_loc, cnstr, tps) ->
          has_structs_ref := true;
          let expr = mk_cnstr_args_match _loc ~is_variant:true cnstr tps in
          let new_match = <:match_case< ($str:cnstr$ as tag) -> $expr$ >> in
          new_match :: other_matches, false
      | `I (_loc, inh) ->
          handle_variant_inh _loc full_type match_last other_matches inh
    in
    let other_matches =
      mk_variant_other_matches _loc rev_atoms "ptag_no_args"
    in
    let match_structs_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_structs_inhs
    in
    (
      handle_variant_match_last _loc match_last match_structs_inhs,
      !has_structs_ref
    )

  (* Generate code for handling atomic and structured variants (i.e. not
     included variant types) *)
  and handle_variant_tag _loc full_type row_fields =
    let rev_atoms, rev_structs, rev_atoms_inhs, rev_structs_inhs =
      List.fold_left ~f:(split_row_field _loc) ~init:([], [], [], []) row_fields
    in
    let match_struct, has_structs =
      mk_variant_match_struct _loc full_type rev_structs_inhs rev_atoms in
    let maybe_sexp_args_patt =
      if has_structs then <:patt< sexp_args >>
      else <:patt< _ >>
    in
    <:match_case<
        Sexplib.Sexp.Atom atom as sexp ->
          $mk_variant_match_atom _loc full_type rev_atoms_inhs rev_structs$
      | Sexplib.Sexp.List
          [Sexplib.Sexp.Atom atom :: $maybe_sexp_args_patt$] as sexp ->
            $match_struct$
      | Sexplib.Sexp.List [Sexplib.Sexp.List _ :: _] as sexp ->
          Sexplib.Conv_error.nested_list_invalid_poly_var _loc sexp
      | Sexplib.Sexp.List [] as sexp ->
          Sexplib.Conv_error.empty_list_invalid_poly_var _loc sexp
    >>

  (* Generate matching code for variants *)
  and variant_of_sexp _loc ?full_type row_tp =
    let row_fields = list_of_ctyp row_tp [] in
    let is_contained, full_type =
      match full_type with
      | None -> true, <:ctyp< [= $row_tp$ ] >>
      | Some full_type -> false, full_type
    in
    let top_match =
      match row_fields with
      | (<:ctyp< $id:_$ >> | <:ctyp< $_$ $_$ >>) as inh :: rest ->
          let rec loop inh row_fields =
            let call =
              <:expr< ( $mk_internal_call _loc inh$ sexp :> $full_type$ ) >>
            in
            match row_fields with
            | [] -> call
            | h :: t ->
                let expr =
                  match h with
                  | <:ctyp< $id:_$ >> | <:ctyp< $_$ $_$ >> -> loop h t
                  | _ ->
                     let rftag_matches =
                       handle_variant_tag _loc full_type row_fields
                     in
                     <:expr< match sexp with [ $rftag_matches$ ] >>
                in
                <:expr<
                  try $call$ with
                  [ $handle_no_variant_match _loc expr$ ]
                >>
          in
          <:match_case< sexp -> $loop inh rest$ >>
      | _ :: _ -> handle_variant_tag _loc full_type row_fields
      | [] -> assert false  (* impossible *)
    in
    if is_contained then
      `Fun
        <:expr<
          fun sexp ->
            try match sexp with [ $top_match$ ]
            with
            [ Sexplib.Conv_error.No_variant_match (msg, sexp) ->
                Sexplib.Conv.of_sexp_error msg sexp ]
        >>
    else `Match top_match

  and poly_of_sexp _loc parms tp =
    let bindings =
      let mk_binding parm =
        <:binding<
          $Gen.idp _loc ("_of_" ^ parm)$ =
            fun sexp -> Sexplib.Conv_error.record_poly_field_value _loc sexp
        >>
      in
      List.map ~f:mk_binding (Gen.ty_var_list_of_ctyp parms [])
    in
    match type_of_sexp _loc tp with
    | `Fun fun_expr -> `Fun <:expr< let $list:bindings$ in $fun_expr$ >>
    | `Match matchings ->
        `Match
          <:match_case<
            arg ->
              let $list:bindings$ in
              match arg with
              [ $matchings$ ]
          >>


  (* Sum type conversions *)

  (* Generate matching code for well-formed S-expressions wrt. sum types *)
  let rec mk_good_sum_matches _loc = function
    | <:ctyp< $uid:cnstr$ >> ->
        let lccnstr = String.uncapitalize cnstr in
        <:match_case<
          Sexplib.Sexp.Atom ($str:lccnstr$ | $str:cnstr$) -> $uid:cnstr$
        >>
    | <:ctyp< $uid:cnstr$ of $tps$ >> ->
        let lccnstr = String.uncapitalize cnstr in
        <:match_case<
          (Sexplib.Sexp.List
            [Sexplib.Sexp.Atom ($str:lccnstr$ | $str:cnstr$ as tag) ::
              sexp_args] as sexp) ->
                $mk_cnstr_args_match _loc ~is_variant:false cnstr tps$
        >>
    | <:ctyp< $tp1$ | $tp2$ >> ->
        <:match_case<
            $mk_good_sum_matches _loc tp1$
          | $mk_good_sum_matches _loc tp2$
        >>
    | _ -> assert false  (* impossible *)

  (* Generate matching code for malformed S-expressions with good tags
     wrt. sum types *)
  let rec mk_bad_sum_matches _loc = function
    | <:ctyp< $uid:cnstr$ >> ->
        let lccnstr = String.uncapitalize cnstr in
        <:match_case<
          Sexplib.Sexp.List
            [Sexplib.Sexp.Atom ($str:lccnstr$ | $str:cnstr$) :: _] as sexp ->
              Sexplib.Conv_error.stag_no_args _loc sexp
        >>
    | <:ctyp< $uid:cnstr$ of $_$ >> ->
        let lccnstr = String.uncapitalize cnstr in
        <:match_case<
          Sexplib.Sexp.Atom ($str:lccnstr$ | $str:cnstr$) as sexp ->
            Sexplib.Conv_error.stag_takes_args _loc sexp
        >>
    | <:ctyp< $tp1$ | $tp2$ >> ->
        <:match_case<
            $mk_bad_sum_matches _loc tp1$
          | $mk_bad_sum_matches _loc tp2$
        >>
    | _ -> assert false  (* impossible *)

  (* Generate matching code for sum types *)
  let sum_of_sexp _loc alts =
    `Match
      <:match_case<
          $mk_good_sum_matches _loc alts$
        | $mk_bad_sum_matches _loc alts$
        | Sexplib.Sexp.List [Sexplib.Sexp.List _ :: _] as sexp ->
            Sexplib.Conv_error.nested_list_invalid_sum _loc sexp
        | Sexplib.Sexp.List [] as sexp ->
            Sexplib.Conv_error.empty_list_invalid_sum _loc sexp
        | sexp -> Sexplib.Conv_error.unexpected_stag _loc sexp
      >>


  (* Record conversions *)

  (* Generate code for extracting record fields *)
  let rec mk_extract_fields _loc = function
    | <:ctyp< $tp1$; $tp2$ >> ->
        <:match_case<
            $mk_extract_fields _loc tp1$
          | $mk_extract_fields _loc tp2$
        >>
    | <:ctyp< $lid:nm$ : mutable sexp_option $tp$ >>
    | <:ctyp< $lid:nm$ : sexp_option $tp$ >>
    | <:ctyp< $lid:nm$ : mutable $tp$ >>
    | <:ctyp< $lid:nm$ : $tp$ >> ->
        let unrolled =
          unroll_cnv_fp _loc <:expr< field_sexp >> (type_of_sexp _loc tp)
        in
        <:match_case<
          $str:nm$ ->
            match $lid:nm ^ "_field"$.val with
            [ None ->
                let fvalue = $unrolled$ in
                $lid:nm ^ "_field"$.val := Some fvalue
            | Some _ -> duplicates.val := [ field_name :: duplicates.val ] ]
        >>
    | _ -> assert false  (* impossible *)

  (* Generate code for handling the result of matching record fields *)
  let mk_handle_record_match_result _loc has_poly flds =
    let has_nonopt_fields = ref false in
    let res_tpls, bi_lst, good_patts =
      let rec loop (res_tpls, bi_lst, good_patts as acc) = function
        | <:ctyp< $lid:nm$ : $tp$ >> ->
            let fld = <:expr< $lid:nm ^ "_field"$.val >> in
            let new_bi_lst, new_good_patts =
              match tp with
              | <:ctyp< sexp_option $_$ >>
              | <:ctyp< mutable sexp_option $_$ >>
              | <:ctyp< sexp_list $_$ >>
              | <:ctyp< mutable sexp_list $_$ >> ->
                  bi_lst, <:patt< $lid:nm ^ "_value"$ >> :: good_patts
              | _ ->
                  has_nonopt_fields := true;
                  (
                    <:expr< (Pervasives.(=) $fld$ None, $str:nm$) >> :: bi_lst,
                    <:patt< Some $lid:nm ^ "_value"$ >> :: good_patts
                  )
            in
            (
              <:expr< $fld$ >> :: res_tpls,
              new_bi_lst,
              new_good_patts
            )
        | <:ctyp< $tp1$; $tp2$ >> -> loop (loop acc tp2) tp1
        | _ -> assert false  (* impossible *)
      in
      loop ([], [], []) flds
    in
    let match_good_expr =
      if has_poly then
        let rec loop acc = function
          | <:ctyp< $tp1$; $tp2$ >> -> loop (loop acc tp2) tp1
          | <:ctyp< $lid:nm$ : $_$ >> -> <:expr< $lid:nm ^ "_value"$ >> :: acc
          | _ -> assert false  (* impossible *)
        in
        match loop [] flds with
        | [match_good_expr] -> match_good_expr
        | match_good_exprs -> <:expr< $tup:exCom_of_list match_good_exprs$ >>
      else
        let rec loop = function
          | <:ctyp< $tp1$; $tp2$ >> -> <:rec_binding< $loop tp1$; $loop tp2$ >>
          | <:ctyp< $lid:nm$ : sexp_list $_$ >> ->
              <:rec_binding<
                $lid:nm$ =
                  match $lid:nm ^ "_value"$ with
                  [ None -> [] | Some v -> v ]
              >>
          | <:ctyp< $lid:nm$ : $_$ >> ->
              <:rec_binding< $lid:nm$ = $lid:nm ^ "_value"$ >>
          | _ -> assert false  (* impossible *)
        in
        <:expr< { $loop flds$ } >>
    in
    let expr, patt =
      match res_tpls, good_patts with
      | [res_expr], [res_patt] -> res_expr, res_patt
      | _ ->
          <:expr< $tup:exCom_of_list res_tpls$ >>,
          <:patt< $tup:paCom_of_list good_patts$ >>
    in
    if !has_nonopt_fields then
      <:expr<
        match $expr$ with
        [ $patt$ -> $match_good_expr$
        | _ ->
            Sexplib.Conv_error.record_undefined_elements _loc sexp
              $Gen.mk_expr_lst _loc bi_lst$
        ]
      >>
    else <:expr< match $expr$ with [ $patt$ -> $match_good_expr$ ] >>

  (* Generate code for converting record fields *)
  let mk_cnv_fields has_poly _loc flds =
    let field_refs =
      let rec loop = function
        | <:ctyp< $tp1$; $tp2$ >> -> <:binding< $loop tp1$ and $loop tp2$ >>
        | <:ctyp< $lid:nm$ : $_$ >> ->
            <:binding< $lid:nm ^ "_field"$ = ref None >>
        | _ -> assert false  (* impossible *)
      in
      loop flds
    in
    <:expr<
      let $field_refs$ and duplicates = ref [] and extra = ref [] in
      let rec iter = fun
        [ [
            Sexplib.Sexp.List
              [(Sexplib.Sexp.Atom field_name); field_sexp] ::
            tail
          ] ->
            do {
              match field_name with
              [ $mk_extract_fields _loc flds$
              | _ ->
                  if Sexplib.Conv.record_check_extra_fields.val then
                    extra.val := [ field_name :: extra.val ]
                  else () ];
              iter tail }
        | [sexp :: _] -> Sexplib.Conv_error.record_only_pairs_expected _loc sexp
        | [] -> () ]
      in
      do {
        iter field_sexps;
        if Pervasives.(<>) duplicates.val [] then
          Sexplib.Conv_error.record_duplicate_fields
            _loc duplicates.val sexp
        else if Pervasives.(<>) extra.val [] then
          Sexplib.Conv_error.record_extra_fields _loc extra.val sexp
        else $mk_handle_record_match_result _loc has_poly flds$
      }
    >>

  let rec is_poly = function
    | <:ctyp< $_$ : ! $_$ . $_$ >> -> true
    | <:ctyp< $flds1$; $flds2$ >> -> is_poly flds1 || is_poly flds2
    | _ -> false

  (* Generate matching code for records *)
  let record_of_sexp _loc flds =
    let handle_fields =
      let has_poly = is_poly flds in
      let cnv_fields = mk_cnv_fields has_poly _loc flds in
      if has_poly then
        let is_singleton_ref = ref true in
        let patt =
          let rec loop = function
            | <:ctyp< $tp1$; $tp2$ >> ->
                is_singleton_ref := false;
                <:patt< $loop tp1$, $loop tp2$ >>
            | <:ctyp< $lid:nm$ : $_$ >> -> <:patt< $lid:nm$ >>
            | _ -> assert false  (* impossible *)
          in
          let patt = loop flds in
          if !is_singleton_ref then patt
          else <:patt< $tup:patt$ >>
        in
        let record_def =
          let rec loop = function
            | <:ctyp< $tp1$; $tp2$ >> ->
                <:rec_binding< $loop tp1$; $loop tp2$ >>
            | <:ctyp< $lid:nm$ : $_$ >> -> <:rec_binding< $lid:nm$ = $lid:nm$ >>
            | _ -> assert false  (* impossible *)
          in
          loop flds
        in
        <:expr<
          let $patt$ = $cnv_fields$ in
          { $record_def$ }
        >>
      else cnv_fields
    in
    `Match
      <:match_case<
          Sexplib.Sexp.List field_sexps as sexp -> $handle_fields$
        | Sexplib.Sexp.Atom _ as sexp ->
            Sexplib.Conv_error.record_list_instead_atom _loc sexp
      >>


  (* Empty type *)
  let nil_of_sexp _loc =
    `Fun <:expr< fun sexp -> Sexplib.Conv_error.empty_type _loc sexp >>


  (* Generate code from type definitions *)

  let rec is_poly_call = function
    | <:expr< $f$ $_$ >> -> is_poly_call f
    | <:expr< $lid:name$ >> -> name.[0] = '_' && name.[1] = 'o'
    | _ -> false

  let td_of_sexp _loc type_name tps rhs =
    let is_alias_ref = ref false in
    let handle_alias _loc tp =
      is_alias_ref := true;
      type_of_sexp _loc tp
    in
    let coll_args tp param = <:ctyp< $tp$ $param$ >> in
    let full_type =
      List.fold_left ~f:coll_args ~init:<:ctyp< $lid:type_name$ >> tps
    in
    let is_variant_ref = ref false in
    let handle_variant row_fields =
      is_variant_ref := true;
      variant_of_sexp ~full_type row_fields
    in
    let body =
      let rec loop _loc =
        Gen.switch_tp_def _loc
          ~alias:handle_alias
          ~sum:sum_of_sexp
          ~record:record_of_sexp
          ~variants:handle_variant
          ~mani:(fun _loc _tp1 -> loop _loc)
          ~nil:nil_of_sexp
      in
      match loop _loc rhs with
      | `Fun fun_expr ->
          (* Prevent violation of value restriction through eta-expansion *)
          if !is_alias_ref && tps = [] then
            <:expr< fun [ sexp -> $fun_expr$ sexp ] >>
          else <:expr< $fun_expr$ >>
      | `Match matchings -> <:expr< fun [ $matchings$ ] >>
    in
    let internal_name = type_name ^ "_of_sexp" ^ "__" in
    let arg_patts, arg_exprs =
      List.split (
        List.map ~f:(function tp ->
            let name = "_of_" ^ Gen.get_tparam_id tp in
            Gen.idp _loc name, Gen.ide _loc name
          )
          tps)
    in
    let with_poly_call = !is_alias_ref && is_poly_call body in
    let internal_fun_body =
      let full_type_name = sprintf "%s.%s" (get_conv_path ()) type_name in
      if with_poly_call then
        Gen.abstract _loc arg_patts
          <:expr<
            fun sexp ->
              Sexplib.Conv_error.silly_type $str:full_type_name$ sexp
          >>
      else
        <:expr<
          let _loc = $str:full_type_name$ in
          $Gen.abstract _loc arg_patts body$
        >>
    in
    let pre_external_fun_body =
      let internal_call =
        let internal_expr = Gen.ide _loc internal_name in
        <:expr< $Gen.apply _loc internal_expr arg_exprs$ sexp >>
      in
      let no_variant_match_mc =
        <:match_case<
          Sexplib.Conv_error.No_variant_match (msg, sexp) ->
            Sexplib.Conv.of_sexp_error msg sexp
        >>
      in
      if with_poly_call then
        <:expr< try $body$ sexp with [ $no_variant_match_mc$ ] >>
      (* Type alias may refer to variant, therefore same handling here! *)
      else if !is_variant_ref || !is_alias_ref then
        <:expr< try $internal_call$ with [ $no_variant_match_mc$ ] >>
      else internal_call
    in
    let internal_binding =
      <:binding< $lid:internal_name$ = $internal_fun_body$ >>
    in
    let external_fun_patt = Gen.idp _loc (type_name ^ "_of_sexp") in
    let external_fun_body =
      Gen.abstract _loc arg_patts <:expr< fun sexp -> $pre_external_fun_body$ >>
    in
    let external_binding =
      <:binding< $external_fun_patt$ = $external_fun_body$ >>
    in
    internal_binding, external_binding

  let rec tds_of_sexp _loc acc = function
    | TyDcl (_loc, type_name, tps, rhs, _cl) ->
        td_of_sexp _loc type_name tps rhs :: acc
    | TyAnd (_loc, tp1, tp2) -> tds_of_sexp _loc (tds_of_sexp _loc acc tp2) tp1
    | _ -> assert false  (* impossible *)

  (* Generate code from type definitions *)
  let of_sexp = function
    | TyDcl (_loc, type_name, tps, rhs, _cl) ->
        let internal_binding, external_binding =
          td_of_sexp _loc type_name tps rhs
        in
        let recursive = Gen.type_is_recursive _loc type_name rhs in
        if recursive then
          <:str_item<
            value rec $internal_binding$
            and $external_binding$
          >>
        else
          <:str_item<
            value $internal_binding$;
            value $external_binding$
          >>
    | TyAnd (_loc, _, _) as tds ->
        let two_bindings = tds_of_sexp _loc [] tds in
        let bindings =
          List.map ~f:(fun (b1, b2) -> <:binding< $b1$ and $b2$ >>) two_bindings
        in
        <:str_item< value rec $list:bindings$ >>
    | _ -> assert false  (* impossible *)

end

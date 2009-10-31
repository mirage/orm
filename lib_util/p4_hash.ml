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

open Camlp4
open PreCast
open Ast
open Syntax

let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

let rec t ~envfn depth ctyp =
  let _loc = loc_of_ctyp ctyp in
  let default = <:expr< Hashtbl.hash x >> in
  let again y = if depth > 10 then 
      <:expr< 0 >> 
    else 
      t ~envfn (depth+1) y in

  match ctyp with
    <:ctyp< unit >> | <:ctyp< int >> 
  | <:ctyp< int32 >> | <:ctyp< int64 >>
  | <:ctyp< float >> | <:ctyp< bool >> 
  | <:ctyp< char >> | <:ctyp< string >> -> default

  | <:ctyp< option $t$ >> ->
      <:expr< match x with [ None -> 0 | Some x -> $again t$ ] >>

  | <:ctyp< { $fs$ } >> ->
      let rec fn acc = function
        | <:ctyp< $t1$; $t2$ >> -> fn (fn acc t1) t2
        | <:ctyp< $lid:id$ : mutable $t$ >> -> acc
        | <:ctyp< $lid:id$ : $t$ >> -> (id,t) :: acc
        | _ -> assert false in
      let ext id t = <:expr< (let x = x.$lid:id$ in $again t$) >> in
      (match fn [] fs with
         [] -> <:expr< 0 >>
       | [(fid,t)] -> ext fid t
       | (fid,t) :: tl ->
           List.fold_left (fun a (fid,t) ->
             <:expr< _combine $a$ $ext fid t$ >>
           ) (ext fid t) tl
      )

  | <:ctyp< [< $rf$ ] >> | <:ctyp< [> $rf$ ] >>
  | <:ctyp< [= $rf$ ] >> | <:ctyp< [ $rf$ ] >> ->
    let rec fn acc = function
        <:ctyp< $t1$ | $t2$ >> -> fn (fn acc t1) t2
      | <:ctyp< $uid:id$ of $t$ >> ->
          <:match_case< $uid:id$ x -> 
             _combine $`int:hash_variant id$ $again t$ >> :: acc
      | <:ctyp< $uid:id$ >> ->
          <:match_case< $uid:id$ -> $`int:hash_variant id$ >> :: acc
      | <:ctyp< ` $uid:id$ >> ->
          <:match_case< `$uid:id$ -> $`int:hash_variant id$ >> :: acc
      | <:ctyp< `$uid:id$ of $t$ >> ->
          <:match_case< `$uid:id$ x -> 
             _combine $`int:hash_variant id$ $again t$ >> :: acc
      | _ -> assert false in
    <:expr< match x with [ $mcOr_of_list (fn [] rf)$ ] >>

  | <:ctyp< < $_$ > >> -> default

  | <:ctyp@loc< ( $tup:tp$ ) >> ->
     let tys = list_of_ctyp tp [] in
     let pos = ref 0 in
     let vn p = "c" ^ (string_of_int p) in
     let mcp = paCom_of_list (List.rev (List.fold_left (fun a t ->
       incr pos;
       <:patt< $lid:vn !pos$ >> :: a
     ) [] tys)) in
     let ext p t = <:expr< (let x = $lid:vn p$ in $again t$) >> in
     let tubis = match tys with 
       | hd :: tl -> 
           let pos = ref 1 in
           List.fold_left (fun a t ->
             incr pos;
             <:expr< _combine $a$ $ext !pos t$ >> ) (ext 1 hd) tl
       | _ -> assert false in
     <:expr< match x with [ $tup:mcp$ -> $tubis$ ] >>

  | <:ctyp@loc< list $t$ >> ->
     <:expr< List.fold_left (fun a x -> _combine a $again t$) 0 x >>

  | <:ctyp@loc< array $t$ >> ->
     <:expr< Array.fold_left (fun a x -> _combine a $again t$) 0 x >>

  | <:ctyp< $lid:id$ >> ->
      <:expr< $again (envfn id)$ >>

  | _ -> failwith "unknown type"

let gen1 ~envfn ctyp =
  let _loc = loc_of_ctyp ctyp in
  <:expr< 
    let _combine acc h = ((acc lsl 5) + acc) + h in
    $t ~envfn 0 ctyp$
  >>

let gen ?(fun_name=(fun x -> x)) ctyp =
  let _loc = loc_of_ctyp ctyp in
  (* make a list of all the terms *)
  let rec fn ty acc =
    match ty with
      Ast.TyAnd (_loc, tyl, tyr) ->
        fn tyl (fn tyr acc)
    | Ast.TyDcl (_loc, id, _, ty, []) ->
      (id,ty) :: acc
    | _ -> assert false in
  let env = fn ctyp [] in
  let envfn = fun id -> List.assoc id env in
  let bis = List.map (fun (id,ctyp) ->
     <:binding<
        $lid:(fun_name id)$ (x : $lid:id$) = $gen1 ~envfn ctyp$ >>
  ) env in
  <:str_item< value $biAnd_of_list bis$ >>

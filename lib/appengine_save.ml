(*
 * Copyright (c) 2009-2010
 *     Anil Madhavapeddy <anil@recoil.org>
 *     Thomas Gazagnaire <thomas@gazagnaire.com>
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

open Appengine_datastore

let to_jlong i = (new lang_long (`Long i) :> CadmiumObj.jObject)
let to_jbool b = (new lang_bool (`Bool b) :> CadmiumObj.jObject)
let to_jfloat v = (new lang_float (`Float v) :> CadmiumObj.jObject)
let to_jstring s = (new lang_string (`String s) :> CadmiumObj.jObject)
let to_jlist fn l = 
    let v = new lang_vector (`Int (Int32.of_int (List.length l))) in
    List.iter (fun e -> v#addElement (fn e)) l;
    (v :> CadmiumObj.jObject)

let to_entity ty va =
  match ty, va with
  | (Type.Ext (n, Type.Dict ts)), (Value.Ext (_, Value.Dict vs)) ->
      let ent = new entity (`String n) in
      List.iter (fun (f, v) ->
          match v with
          | Value.Unit -> ent#setProperty f (to_jlong 0L)
          | Value.Int i -> ent#setProperty f (to_jlong i)
          | Value.Bool b -> ent#setProperty f (to_jbool b)
          | Value.Float v -> ent#setProperty f (to_jfloat v)
          | Value.String s -> ent#setProperty f (to_jstring s)
          | Value.Tuple vl
          | Value.Enum vl ->
              let l = to_jlist (function 
                | Value.Int i -> to_jlong i 
                | Value.Bool b -> to_jbool b
                | Value.Float v -> to_jfloat v
                | Value.String s -> to_jstring s
                | Value.Unit -> to_jlong 0L
                | x -> to_jstring (Json.to_string x)
              ) vl in
              ent#setProperty f l
      ) vs;
      ent
  | _ -> failwith "eek"

let update_value ~env ~db ty va =
  let ent = to_entity ty va in
  let _ = db.Appengine_backend.svc#put ent in
  ()

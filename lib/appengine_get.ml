(*
 * Copyright (c) 2009-2010 Anil Madhavapeddy <anil@recoil.org>
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
open Appengine_datastore

(*
let string_of_constraint (name, c) =
	let make name = String.concat "__" name in
	let bool name = function
		| `T  -> make name, "=", Some (Data.INT 1L)
		| `F  -> make name, "=", Some (Data.INT 0L)
	and int_like name conv = function
		| `Eq i  -> make name, "=", Some (conv i)
		| `Neq i -> make name, "!=", Some (conv i)
		| `Le i  -> make name, "<", Some (conv i)
		| `Ge i  -> make name, ">", Some (conv i)
		| `Leq i -> make name, "<=", Some (conv i)
		| `Geq i -> make name, ">=", Some (conv i)
	and string name = function
		| `Eq s       -> make name, "IS", Some (Data.TEXT s)
		| `Contains s -> make name, "IS", Some (Data.TEXT (sprintf "*%s*" s)) in
	match c with
		| `Bool b    -> bool name b
		| `String s  -> string name s
		| `Float f   -> int_like name (fun f -> Data.FLOAT f) f
		| `Char c    -> int_like name (fun c -> Data.INT (Int64.of_int (Char.code c))) c
		| `Int i     -> int_like name (fun i -> Data.INT (Int64.of_int i)) i
		| `Int32 i   -> int_like name (fun i -> Data.INT (Int64.of_int32 i)) i
		| `Int64 i   -> int_like name (fun i -> Data.INT i) i
		| `Big_int i -> int_like name (fun i -> Data.TEXT (Big_int.string_of_big_int i)) i
*)

let of_jlong (j:CadmiumObj.jObject) = (new lang_long (`Cd'initObj j))#longValue
let of_jbool (j:CadmiumObj.jObject) = (new lang_bool (`Cd'initObj j))#booleanValue
let of_jfloat (j:CadmiumObj.jObject) = (new lang_float (`Cd'initObj j))#floatValue
let of_jstring (j:CadmiumObj.jObject) = (new lang_string (`Cd'initObj j))#toString

let foldIter fn i o =
  let r = ref i in
  while o#hasNext do
     r := fn !r o#next
  done;
  List.rev !r

let foldIter2 fn i o l =
  let r = ref i in
  List.iter (fun l' ->
    if o#hasNext then
      r := fn !r o#next l'
    else
      Printf.printf "foldIter2 fail\n%!"
  ) l;
  List.rev !r

let rec to_value prop ty =
  let cl = prop#getClass#getName in
  match ty, cl with
    | Type.Unit, _ -> Value.Unit
    | Type.Int _, _ -> Value.Int (of_jlong prop)
    | Type.Bool, _ -> Value.Bool (of_jbool prop)
    | Type.Char, _ | Type.String, _ -> Value.String (of_jstring prop)
    | Type.Enum ty', "java.util.ArrayList" ->
        let l = new util_arraylist (`Cd'initObj prop) in
        Value.Enum (foldIter (fun a v -> to_value v ty' :: a) [] l#iterator)
    | Type.Tuple tyl, "java.util.ArrayList" ->
        let l = new util_arraylist (`Cd'initObj prop) in
        Value.Tuple (foldIter2 (fun a v ty' -> (to_value v ty') :: a) [] l#iterator tyl)
    | tyl, "java.lang.String" ->
        Json.of_string tyl (of_jstring prop) in
    | ty,cl -> Printf.printf "Unknown ty/cl: %s %s, returning null\n%!" (Type.to_string ty) cl; failwith ""

let entity_to_value n ty (ent:entity) =
  let ty_fields = match ty with
   | Type.Dict ts -> List.map (fun (k,_,v) -> (k,v)) ts
   | x -> Printf.printf "ent_to_val: %s\n%!" (Type.to_string x); failwith ""
  in
  let ps = foldIter (fun a o ->
      let key = (new lang_string (`Cd'initObj o))#toString in
      let prop = ent#getProperty key in
      let v = to_value prop (List.assoc key ty_fields) in
      (key,v) :: a
    ) [] ent#getProperties#keySet#iterator in
   Value.Ext ((n,0L), Value.Dict ps)

let get_values ~env ~db ?id ?(constraints=[]) ?custom_fn (t:Type.t) =
(*  let gql = String.concat " AND " (List.map string_of_constraint constraints) in *)
   match t with
   | Type.Rec (n, t)
   | Type.Ext (n, t) ->
         let gql = n in
         let query = new query (`String gql) in
         let pq = db.Appengine_backend.svc#prepare query in
         let iter = pq#asIterator in
         foldIter (fun a o ->
             let ent = new entity (`Cd'initObj o) in
             (0L, entity_to_value n t ent) :: a
         ) [] iter
   | _ -> failwith "TODO"

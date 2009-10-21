(*
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

type t =
    [ `Unit | `Int | `Int32 | `Int64 | `Bool | `Float | `Char | `String
    | `Product of t list
    | `Collection of t
    | `Named_product of (string * t) list
    | `Named_sum of (string * t list) list
    | `Option of t
    | `Rec ]

(* If there are still some `Var v, then the type is recursive *)
let is_recursive (t:t) =
  let rec aux = function
    | `Rec -> true
    | `Collection t | `Option t -> aux t
    | `Product t -> List.exists aux t
    | `Named_product t -> List.exists (fun (_,t) -> aux t) t
    | `Named_sum t -> List.exists (fun (_,t) -> List.exists aux t) t
    | `Unit | `Int | `Int32 | `Int64 | `Bool | `Float | `Char | `String -> false in
  aux t

let rec to_string (t:t) = match t with                                                                    
  | `Unit -> "unit"
  | `Int -> "int"
  | `Int32 -> "int32"
  | `Int64 -> "int64"
  | `Bool -> "bool"
  | `Float -> "float"
  | `Char -> "char"
  | `String -> "string"
  | `Product ts -> Printf.sprintf "(%s)" (String.concat "*" (List.map to_string ts))
  | `Collection t -> Printf.sprintf "[%s]" (to_string t)
  | `Named_product ts -> Printf.sprintf "{%s}" (String.concat "*" (List.map (fun (s,t) -> Printf.sprintf "%s:%s" s (to_string t)) ts))
  | `Named_sum ts -> Printf.sprintf "<%s>" (String.concat "*" (List.map (fun (s,t) -> Printf.sprintf "%s:(%s)" s (String.concat "*" (List.map to_string t))) ts))
  | `Option t -> Printf.sprintf "option.%s" (to_string t)
  | `Rec -> "rec"

(* Is [t1] a subtype of [t2] ?                                                *)
(* Our subtype relation is the following:                                     *)
(*  if value of type t2 are stored in the database, then a value of a type t1 *)
(*  can be naturally build from that stored value.                            *)
let is_subtype_of t1 t2 =
  let table = Hashtbl.create 128 in
  let rec (<:) t s =
    if Hashtbl.mem table (t,s) then
      Hashtbl.find table (t,s)
    else begin
      let result = match (t,s) with
      | `Rec, `Rec -> true
      | `Rec, s -> t1 <: s
      | t, `Rec -> t <: t2
      | `Collection t, `Collection s -> t <: s
      | `Option t, `Option s -> t <: s
  (*  | `Option t, _ -> t <: s NOT VALID YET *)
      | `Product ts, `Product ss -> List.for_all2 (<:) ts ss
      | `Named_product ts, `Named_product ss -> List.for_all (fun (x1,y1) -> List.exists (fun (x2,y2) -> x1=x2 && y1 <: y2) ss) ts 
      | `Named_sum ts, `Named_sum ss -> List.for_all (fun (x2,y2) -> List.exists (fun (x1,y1) -> x1=x2 && List.for_all2 (<:) y1 y2) ts) ss
      | `Unit, `Unit -> true
      | `Int, `Int -> true
      | `Int32, `Int32 | `Int32, `Int -> true
      | `Int64, `Int64 | `Int64, `Int32 | `Int64, `Int -> true
      | `Bool, `Bool -> true
      | `Float, `Float -> true
      | `Char, `Char -> true
      | `String, `Char | `String, `String -> true
      | _ -> false in
      Hashtbl.replace table (t,s) result;
      if not result then Printf.printf "%s <: %s --> FALSE\n" (to_string t) (to_string s);
      result
    end in
  t1 <: t2

let (<:) = is_subtype_of

exception Subtype_error of string * string

let index_par c s =
  let res = ref None in
  let par = ref 0 in
  let i = ref 0 in
  let n = String.length s in
  while !res = None && !i < n do
    if s.[!i] = '(' || s.[!i] = '[' || s.[!i] = '{' || s.[!i] = '<' then incr par;
    if s.[!i] = ')' || s.[!i] = ']' || s.[!i] = '}' || s.[!i] = '>' then decr par;
    if !par = 0 && s.[!i] = c then res := Some !i;
    incr i
  done;
  match !res with
  | None -> raise Not_found
  | Some i -> i

let split_par ?limit c s =
  let rec aux n s =
    match limit with
    | Some i when n>=i -> [s]
    | _ ->
      try 
        let i = index_par c s in
        let h = String.sub s 0 i in
        let t =
           try aux (n-1) (String.sub s (i+1) (String.length s - i - 1))
           with _ -> []
        in
        h :: t
      with _ ->
        [s] in
  aux 1 s

let rec of_string s : t  = match s with
  | "unit" -> `Unit
  | "int" -> `Int
  | "int32" -> `Int32
  | "int64" -> `Int64
  | "bool" -> `Bool
  | "float" -> `Float
  | "char" -> `Char
  | "string" -> `String
  | "rec" -> `Rec
  | s -> match s.[0] with
      | '(' ->
          let s = String.sub s 1 (String.length s - 2) in
          let ss = split_par '*' s in
         `Product (List.map of_string ss)
      | '[' ->
          let s = String.sub s 1 (String.length s - 2) in
          `Collection (of_string s)
      | '{' ->
          let s = String.sub s 1 (String.length s - 2) in
          let ss = split_par '*' s in
          let ss = List.map (split_par ~limit:2 ':') ss in
          `Named_product (List.map (fun x -> match x with [s;t] -> (s, of_string t) | _ -> assert false) ss)
      | '<' ->
          let s = String.sub s 1 (String.length s - 2) in
          let ss = split_par '*' s in
          let ss = List.map (split_par ~limit:2 ':') ss in
          let ss = List.map (fun x -> match x with
            | [s;"()"] -> (s, [])
            | [s;t] ->
                let t = String.sub t 1 (String.length t - 2) in
                (s, List.map of_string (split_par '*' t))
            | _ -> assert false) ss in
          `Named_sum ss
      | 'o' when s.[1] = 'p' && s.[2] = 't' && s.[3] = 'i' && s.[4] = 'o' && s.[5] = 'n' && s.[6] = '.' ->
          let s = String.sub s 7 (String.length s - 7) in
          `Option (of_string s)
      | _ ->
          failwith (Printf.sprintf "Unable to parse type '%s'" s)

(*
let x = of_string "option.int"
let y = of_string "int"
let b = x <: y

let x = of_string "{foo:option.int}"
let y = of_string "{bar:string*foo:int}"
let b = x <: y

let ts = [ ("foo", `Option `Int) ]
let b = `Named_product ts = x 
let ss = [ ("bar", `String); ("foo", `Int) ]
let b = `Named_product ss = y
let b = x <: y
let b = List.for_all (fun (s1,t1) -> List.exists (fun (s2,t2) -> s1=s2 && t1 <: t2) ss) ts 
*)


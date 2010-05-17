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

open Printf
open Lexing

open Camlp4
open PreCast
open Ast

open Pa_type_conv

module Key = struct

  (* Extend grammar with options for SQL tables *)
  type mode = [
    | `Sqlite
    | `Appengine
  ]

  type t = [
    | `Debug of string list
    | `Dot of string
    | `Index of (string * string list) list
    | `Unique of (string * string list) list 
    | `Mode of mode
  ]

  let string_of_key (k:t) = match k with
    | `Unique sl -> "unique: " ^ ( String.concat "," (List.map (fun (x,y) -> sprintf "(%s:%s)" x (String.concat "," y)) sl))
    | `Index sl  -> "index: " ^ ( String.concat "," (List.map (fun (x,y) -> sprintf "(%s:%s)" x (String.concat "," y)) sl))
    | `Debug d   -> "debug: " ^ (String.concat "," d)
    | `Dot f     -> "dot: " ^ f
    | `Mode `Sqlite    -> "mode: sqlite"
    | `Mode `Appengine -> "mode: appengine"


  let orm_parms = Gram.Entry.mk "orm_parms"
  EXTEND Gram

  GLOBAL: orm_parms;

  orm_svars: [[ l = LIST1 [ `LIDENT(x) -> x ] SEP "," -> l ]];

  orm_table: [[ x = LIDENT; "<"; y = orm_svars; ">" -> (x, y) ]];

  orm_tables: [[ l = LIST1 [ orm_table ] SEP "," -> l ]];

  orm_param: [[ 
       "unique"; ":" ; x = orm_tables  -> `Unique x
     | "index";  ":" ; x = orm_tables  -> `Index x
     | "debug";  ":" ; x = orm_svars   -> `Debug x 
     | "dot";    ":" ; x = STRING      -> `Dot x 
     | "mode";   ":" ; "sql"           -> `Mode `Sqlite
     | "mode";   ":" ; "appengine"     -> `Mode `Appengine
  ]];

  orm_parms: [
    [ l = LIST0 [ orm_param ] SEP ";" -> (l : t list) ]
  ];

  END
end

let _ = 
  add_generator_with_arg "orm" Key.orm_parms
    (fun tds args ->
      let _loc = loc_of_ctyp tds in
      let args = match args with None -> [] |Some x -> x in
      let mode, keys =  List.partition (function `Mode _ -> true |_ -> false) args in
      match mode with
      | [] | [`Mode `Sqlite] -> P4_orm_sqlite.gen keys tds
      | [`Mode `Appengine] ->   P4_orm_appengine.gen keys tds
      | _ -> failwith "unknown orm:mode argument"
    )

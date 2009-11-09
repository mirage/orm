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
open P4_orm

module Syntax = struct
  (* Extend grammar with options for SQL tables *)
  type p_keys =
    | Unique of (bool * string * string list) list (* unique, table, fields *)
    | Debug of string list
    | Dot of string

  let string_of_p_keys = function
    | Unique sl ->  "unique: " ^ ( String.concat "," 
       (List.map (fun (u,x,y) -> sprintf "%s(%s:%b)" x (String.concat "," y) u) sl ))
    | Debug d -> "debug: " ^ (String.concat "," d)
    | Dot f -> "dot: " ^ f

  let orm_parms = Gram.Entry.mk "orm_parms"
  EXTEND Gram

  GLOBAL: orm_parms;

  orm_svars: [[ l = LIST1 [ `LIDENT(x) -> x ] SEP "," -> l ]];

  orm_table: [[ x = LIDENT; "<"; y = orm_svars; ">" -> (x, y) ]];

  orm_tables: [[ l = LIST1 [ orm_table ] SEP "," -> l ]];

  orm_param: [[ 
       "unique"; ":" ; x = orm_tables -> Unique (List.map (fun (x,y) -> (true,x,y)) x)
     | "index"; ":" ; x = orm_tables -> Unique (List.map (fun (x,y) -> (false,x,y)) x)
     | "debug";  ":" ; x = orm_svars -> Debug x 
     | "dot";    ":" ; x = STRING -> Dot x 
  ]];

  orm_parms: [
    [ l = LIST0 [ orm_param ] SEP ";" -> l ]
  ];

  END

  let parse_keys ctyp =
    List.fold_left (fun env -> function
      | Unique fl -> { env with Env.indices = fl @ env.Env.indices }
      | Debug modes -> List.fold_left (fun env -> function
        | "sql" -> { env with Env.debug_sql=true } 
        | "binds" -> { env with Env.debug_binds=true }
        | "cache" -> { env with Env.debug_cache=true }
        | "all" -> { env with Env.debug_cache=true; Env.debug_binds=true; Env.debug_sql=true }
        | "none" -> { env with Env.debug_cache=false; Env.debug_binds=false; Env.debug_sql=false }
        | _ -> failwith "unknown debug mode"
        ) env modes
      | Dot file -> { env with Env.debug_dot=(Some file) }
    ) Env.empty

  (* let debug_dot env =
    match env.debug_dot with
    |None -> ()
    |Some fl ->
      let fout = open_out fl in
      Printf.fprintf fout "%s" (Dot_of.env env);
      close_out fout *)

  let _ = 
    add_generator_with_arg "orm"
      orm_parms
      (fun tds args ->
        let _loc = loc_of_ctyp tds in
        match tds, args with
        |_, None ->
          Loc.raise _loc (Stream.Error "pa_orm: arg required")
        |_, Some pkeys ->
          (* debug_dot env (Type.create ctyp); *)
          <:str_item<
            $P4_orm.gen (parse_keys tds) tds$
          >>
        )
end

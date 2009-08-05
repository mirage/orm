(*pp camlp4orf *)

open Printf
open Lexing

open Camlp4
open PreCast
open Ast
open Syntax
open Pa_type_conv

(* Extend grammar with options for SQL tables *)
let sql_parms = Gram.Entry.mk "sql_parms"
EXTEND Gram

GLOBAL: sql_parms;

svars: [
  [ l = LIST0 [ `LIDENT(x) -> x ] SEP "," -> l ]
];

param: [
  [ "unique"; ":"; x = svars -> x ]
];

sql_parms: [
  [ l = LIST0 [ param ] SEP ";" -> l ]
];

END

(* Begin implementation *)

open Sql_orm
open Types
open Parse
open Error

(* Inspect the OCaml type and map it to a SQLite data type *)
let sql_type_of_ocaml nm = function
| Int _ | Int32 _ | Int64 _ -> Schema.integer nm
| Float _ -> Schema.real nm
| Bool _ -> Schema.integer nm
| Char _ -> Schema.integer nm
| String _ -> Schema.text nm

(* Inspect the OCaml type declaration and come up with a table
   name *)
let sql_table_name_of_ocaml t =
   (* work out the top level table name *)
   let tname = match t.td_typ with
   |Record _  | Object _ -> ""
   in
   ()

(* *)
let mk_pp ty =
   let x = { Sql_orm.Schema.unique = [] } in
   let _loc = Loc.ghost in
   let t = parse_typedef _loc ty in
   (* Work out the top-level names for the record *)
   
   let s = String.concat " || " (List.map (fun t -> string_of_typ t.td_typ) t) in
     <:str_item<
       $str:s$
   >>

(* Register the keyword with type-conv *)
let () =
  add_generator_with_arg
    "persist"
    sql_parms
    (fun typ arg ->
      prerr_endline "in add_generator_with_arg: persist";
      match typ, arg with
      |_, None ->
        Loc.raise (Ast.loc_of_ctyp typ) (Stream.Error "pa_sql_orm: arg required")
      |_, Some name ->
        let _loc = Loc.ghost in
        <:str_item<$mk_pp typ$>>
      )


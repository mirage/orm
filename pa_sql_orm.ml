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

(* helper to pretty print AST fragments while debugging *)
module PP = Camlp4.Printers.OCaml.Make(Syntax)
let pp = new PP.printer ()
let debug_ctyp ty = Format.eprintf "DEBUG CTYP: %a@." pp#ctyp ty

(* Given a field in a record, figure out the schema type *)
let schema_field_of_ocaml_field f =
  let nm = f.f_id in
  match f.f_typ with
  | Int _ | Int32 _ | Int64 _ -> Schema.integer nm
  | Float _ -> Schema.real nm
  | Bool _ -> Schema.integer nm
  | Char _ -> Schema.integer nm
  | String _ -> Schema.text nm
  | _ -> Schema.blob nm
   
(* Make a Sql_orm.schema from an OCaml record type definition *)
let schema_of_ocaml_record_types t =
  match t.td_typ with
  |Record (_,fl) |Object (_,fl) -> 
     let table_name = t.td_id in
     let contents = List.map schema_field_of_ocaml_field fl in
     (table_name, contents, [], Schema.default_opts)
  |_ -> failwith "unsupported"

(* Return the fields of a record/object type *)
let fields_of_record t =
  match t.td_typ with
  |Record (_,fl) |Object (_,fl) ->  fl
  |_ -> failwith "fields_of_record: unexpected type"

exception Unsupported_type of string
let unsupported ty = raise (Unsupported_type (Types.string_of_typ ty))

let ctyp_of_typedef _loc f =
  let id = f.f_id in
  let ty = match f.f_typ with
  |Int _ -> <:ctyp< int >> 
  |Int32 _ -> <:ctyp< int32 >>
  |Int64 _ -> <:ctyp< int64 >>
  |Float _ -> <:ctyp< float >>
  |Bool _ -> <:ctyp< bool >>
  |Char _ -> <:ctyp< char >>
  |String _ -> <:ctyp< string >>
  |Apply (_loc, _, id, _) -> <:ctyp< $lid:id$ >>
  |x -> unsupported x
  in <:ctyp< $lid:id$ : $ty$ >>

let and_fold_ctypes _loc = function
  |hd::tl ->
    List.fold_left (fun a b -> <:ctyp< $a$ and $b$ >>) hd tl
  |[] ->
    failwith "and_fold_ctypes: empty list"

let mk_pp ty =
   let _ = { Sql_orm.Schema.unique = [] } in
   let _loc = Loc.ghost in
   let ts = parse_typedef _loc ty in
   let schema = List.map schema_of_ocaml_record_types ts in
   let collection = Schema.make schema in
   prerr_endline (Schema.collection_to_string collection);
   let fields = Ast.tyAnd_of_list (List.map (fun td ->
     let ts_name = td.td_id ^ "_persist" in
     let ts_fields = fields_of_record td in
     let fs = List.map (ctyp_of_typedef _loc) ts_fields in
     Ast.TyDcl (_loc, ts_name, [], <:ctyp< < $list:fs$ > >>, [])
   ) ts) in
   <:str_item< type $fields$ >> 

(* Register the keyword with type-conv *)
let () =
  add_generator_with_arg
    "persist"
    sql_parms
    (fun tds args ->
      prerr_endline "in add_generator_with_arg: persist";
      match tds, args with
      |_, None ->
        Loc.raise (Ast.loc_of_ctyp tds) (Stream.Error "pa_sql_orm: arg required")
      |_, Some name ->
        let _loc = Loc.ghost in
        <:str_item<
          $mk_pp tds$
        >>
      )

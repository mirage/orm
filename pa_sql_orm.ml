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

(* convenience function to wrap the TyDcl constructor since I cant
   find an appropriate quotation to use for this *)
let declare_type _loc name ty =
  Ast.TyDcl (_loc, name, [], ty, [])

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

(* Convert an OCaml type into the value exposed in the persist object.
   Not just directly passing through the original type so we can throw
   an error explicitly if its not supported *)
exception Unsupported_type of string
let unsupported ty = raise (Unsupported_type (Types.string_of_typ ty))

let ctyp_of_typedef _loc f =
  match f.f_typ with
  |Int _ -> <:ctyp< int >> 
  |Int32 _ -> <:ctyp< int32 >>
  |Int64 _ -> <:ctyp< int64 >>
  |Float _ -> <:ctyp< float >>
  |Bool _ -> <:ctyp< bool >>
  |Char _ -> <:ctyp< char >>
  |String _ -> <:ctyp< string >>
  |Apply (_loc, _, id, _) -> <:ctyp< $lid:id$ >>
  |x -> unsupported x

(* convert a simple ocaml type into a sql type *)
let sql_type_of_ocaml_type f = 
  match f.f_typ with
  |Int _ | Int32 _ | Int64 _ -> "INTEGER"
  |Float _ -> "REAL"
  |Bool _ -> "INTEGER"
  |Char _ -> "INTEGER"
  |String _ -> "TEXT"
  |x -> unsupported x

(* Return the accessor typedefs (the method/set_method) for a 
   particular OCaml type *)
let accessor_funcs_of_typedef _loc f =
  let ty = ctyp_of_typedef _loc f in
  let id = f.f_id in
  let set_id = sprintf "set_%s" id in
  let acc = <:ctyp< $lid:id$ : $ty$ >> in
  let set_acc = <:ctyp< $lid:set_id$ : $ty$ -> unit >> in
  [ acc; set_acc ]

(* declare the types of the _persist objects used to pass SQL
   objects back and forth*)
let construct_typedefs tds =
  let _loc = Loc.ghost in
  let ts = parse_typedef _loc tds in
  let object_decls = Ast.tyAnd_of_list (List.map (fun td ->
    let ts_name = td.td_id ^ "_persist" in
    let ts_fields = fields_of_record td in
    let fields = List.flatten (List.map (accessor_funcs_of_typedef _loc) ts_fields) in
    let other_fields = List.map (fun i -> <:ctyp< $lid:i$ : unit >>) ["save"; "delete"] in
    let all_fields = fields @ other_fields in
    declare_type _loc ts_name <:ctyp< < $list:all_fields$ > >>;
  ) ts) in
  <:str_item< type $object_decls$ >> 

let construct_funs tds =
  let _loc = Loc.ghost in
  let ts = parse_typedef _loc tds in
  (* assemble the SQL schema from the OCaml typedefs *)
  let schema = List.map schema_of_ocaml_record_types ts in
  let collection = Schema.make schema in
  prerr_endline (Schema.collection_to_string collection);
  (* output the function bindings *)
  let fn_bindings = Ast.biAnd_of_list (List.map (fun td ->
    let fun_name = sprintf "%s_init_db" td.td_id in
    let pid = "id INTEGER PRIMARY KEY AUTOINCREMENT" in
    let fields = fields_of_record td in
    let sqls = String.concat ", " (pid :: List.map (fun f ->
        sprintf "%s %s" f.f_id (sql_type_of_ocaml_type f)
      ) fields) in
    let create_sql = sprintf "CREATE TABLE IF NOT EXISTS %s (%s);" td.td_id sqls in
    let ex = <:expr<
        let sql = $str:create_sql$ in
        Sql_access.db_must_ok db (fun () -> Sqlite3.exec db.Sql_access.db sql)
     >> in
    <:binding< $lid:fun_name$ db = $ex$ >>
  ) ts) in
  <:str_item< value $fn_bindings$ >>

(* Register the persist keyword with type-conv *)
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
          $construct_typedefs tds$ ;
          $construct_funs tds$
        >>
      )

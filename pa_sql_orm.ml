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

(* defines the Ast.binding for a function of form:
let fun_name ?(opt_arg1) ?(opt_arg2) final_ident = function_body ...
*)
let function_with_label_args _loc ~fun_name ~final_ident ~function_body ~return_type opt_args =
   let opt_args = opt_args @ [ <:patt< $lid:final_ident$ >> ] in
   <:binding< $lid:fun_name$ = 
      $List.fold_right (fun b a ->
        <:expr<fun $b$ -> $a$ >>
       ) opt_args <:expr< ( $function_body$ : $return_type$ ) >>
      $ >>
   
(* convert an exposed ocaml type into an AST ctyp fragment *)
let rec ast_of_caml_type _loc = function
  |Types.Unit _    -> <:ctyp< unit >>
  |Types.Int _     -> <:ctyp< int >>
  |Types.Int32 _   -> <:ctyp< int32 >>
  |Types.Int64 _   -> <:ctyp< int64 >>
  |Types.Float _   -> <:ctyp< float >>
  |Types.Char _    -> <:ctyp< char >>
  |Types.String _  -> <:ctyp< string >>
  |Types.Bool _    -> <:ctyp< bool >>
  |Types.List (_,ty) -> <:ctyp< list $ast_of_caml_type _loc ty$ >>
  |Types.Array (_,ty) -> <:ctyp< array $ast_of_caml_type _loc ty$ >>
  |Types.Option (_,ty) -> <:ctyp< option $ast_of_caml_type _loc ty$ >>
  |x -> failwith ("ast_of_caml_type: " ^ (Types.string_of_typ x))

open Sql_types

(* declare the types of the _persist objects used to pass SQL
   objects back and forth *)
let construct_typedefs env =
  let _loc = Loc.ghost in
  let tables = exposed_tables env in
  let object_decls = Ast.tyAnd_of_list (List.fold_left (fun decls t ->
    (* define the external type name as <name>_persist *)
    let ts_name = t.t_name ^ "_persist" in
    (* define the accessor and set_accessor functions *)
    let fields = exposed_fields env t.t_name in
    let accessor_fields = List.flatten (List.map (fun (ty,f) ->
      let ctyp = ast_of_caml_type _loc ty in
      [ <:ctyp< $lid:f.f_name$ : $ctyp$ >> ;
       <:ctyp< $lid:"set_" ^ f.f_name$ : $ctyp$ -> unit >> ]
    ) fields) in
    let other_fields = List.map (fun i ->
      <:ctyp< $lid:i$ : unit >>) ["save"; "delete"] in
    let all_fields = accessor_fields  @ other_fields in
    declare_type _loc ts_name <:ctyp< < $list:all_fields$ > >> :: decls;
  ) [] tables) in
  <:str_item< type $object_decls$ >> 

(* construct the functions to init the db and create objects *)
let construct_funs env =
  let _loc = Loc.ghost in
  let tables = exposed_tables env in
  (* output the function bindings *)
  let fn_bindings = Ast.biAnd_of_list (List.map (fun t ->
    (* init function to initalize sqlite database *)
    let type_name = sprintf "%s_persist" t.t_name in
    let fun_name = sprintf "%s_init_db" t.t_name in

    let sql_decls = List.fold_left (fun a t ->
      let table = find_table env t in
      let fields = sql_fields env t in
      let sql_fields = List.map (fun f ->
        sprintf "%s %s" f.f_name (string_of_sql_type f.f_typ)
      ) fields in
      sprintf "CREATE TABLE IF NOT EXISTS %s (%s)" t (String.concat ", " sql_fields) :: a
    ) [] (t.t_name :: t.t_child) in

    let create_table sql = <:expr<
        let sql = $str:sql$ in
        Sql_access.db_must_ok db (fun () -> Sqlite3.exec db.Sql_access.db sql)
     >> in
    let create_statements = Ast.exSem_of_list (List.map create_table sql_decls) in
    let init_db_binding = <:binding< $lid:fun_name$ db = $create_statements$ >> in
   
    (* return single binding of all the functions for this type *)
    Ast.biAnd_of_list [init_db_binding]
  ) tables) in
  <:str_item< value $fn_bindings$ >>

(* Register the persist keyword with type-conv *)
let () =
  add_generator_with_arg
    "persist"
    sql_parms
    (fun tds args ->
      prerr_endline "in add_generator_with_arg: persist";
      let _loc = Loc.ghost in
      let ts = parse_typedef _loc tds in
      let env = List.fold_left (fun env t ->
        let f = { Types.f_id = t.Types.td_id; f_mut = false; f_typ = t.Types.td_typ } in
        Sql_types.process env "__top" f;
      ) Sql_types.empty_env ts in
      prerr_endline (Sql_types.string_of_env env);
      match tds, args with
      |_, None ->
        Loc.raise (Ast.loc_of_ctyp tds) (Stream.Error "pa_sql_orm: arg required")
      |_, Some name ->
        let _loc = Loc.ghost in
        <:str_item<
          $construct_typedefs env$;
          $construct_funs env$
        >>
      )

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
   XXX: figure out the quotation magic for this, if such exists
*)
let function_with_label_args _loc ~fun_name ~final_ident ~function_body ~return_type opt_args =
   let opt_args = opt_args @ [ <:patt< $lid:final_ident$ >> ] in
   let rec fn _loc = function  
   |hd::tl ->
     Ast.ExFun(_loc,
       Ast.McArr(_loc, 
         hd, 
         (Ast.ExNil _loc),
         (fn _loc tl)
       )
     )
   |[] -> <:expr< ( $function_body$ : $return_type$ ) >>
   in
   Ast.BiEq (_loc, 
     <:patt< $lid:fun_name$ >>,
     (fn _loc opt_args)
   )
     
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
  (* output the function bindings *)
  let fn_bindings = Ast.biAnd_of_list (List.map (fun td ->
    (* init function to initalize sqlite database *)
    let type_name = sprintf "%s_persist" td.td_id in
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
    let init_db_binding = <:binding< $lid:fun_name$ db = $ex$ >> in

    (* new function to create object of the type *)
    let fun_name = sprintf "%s_new" td.td_id in
    let opt_args = <:patt< ?(id=None) >> :: (List.map (fun f ->
        <:patt< ~ $lid:f.f_id$ >>
      ) fields) in
    let new_get_set_functions = List.flatten (List.map (fun f ->
       let internal_var_name = sprintf "_%s" f.f_id in
       let external_var_name = f.f_id in
       [ 
         <:class_str_item< value mutable $lid:internal_var_name$ = $lid:external_var_name$ >>;
         <:class_str_item< method $lid:external_var_name$ = $lid:internal_var_name$ >>;
         <:class_str_item< method $lid:"set_"^external_var_name$ v = ( $lid:internal_var_name$ := v ) >>;
       ]
    ) fields) in
    let new_admin_functions =
       [
         <:class_str_item< method delete = prerr_endline "delete" >>;
         <:class_str_item< method save   = prerr_endline "save" >>;
       ] in
    let new_functions = new_get_set_functions @ new_admin_functions in
    let new_body = <:expr<
        object
         $Ast.crSem_of_list new_functions$;
        end
      >> in
    let new_binding = function_with_label_args _loc 
      ~fun_name 
      ~final_ident:"db"
      ~function_body:new_body
      ~return_type:<:ctyp< $lid:type_name$ >>
      opt_args in

    (* return single binding of all the functions for this type *)
    Ast.biAnd_of_list [init_db_binding; new_binding]
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

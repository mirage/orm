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

(* convert a list of bindings into an expr fragment:
   let x = 1 in y = 2 in z = 3 in ()
*)
let biList_to_expr _loc bindings final =
  List.fold_right (fun b a -> 
    <:expr< let $b$ in $a$ >>
  ) bindings final
   
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
  |Types.Apply (_,[],id,[]) -> <:ctyp< $lid:id^"_persist"$ >>
  |x -> failwith ("ast_of_caml_type: " ^ (Types.string_of_typ x))

open Sql_types

(* declare the types of the _persist objects used to pass SQL
   objects back and forth *)
let construct_typedefs env =
  let _loc = Loc.ghost in
  let tables = exposed_tables env in
  let object_decls = Ast.tyAnd_of_list (List.fold_right (fun t decls ->
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
  ) tables []) in
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

    let sql_decls = List.fold_right (fun t a ->
      let fields = sql_fields env t in
      let sql_fields = List.map (fun f ->
        sprintf "%s %s" f.f_name (string_of_sql_type f.f_typ)
      ) fields in
      sprintf "CREATE TABLE IF NOT EXISTS %s (%s)" t (String.concat ", " sql_fields) :: a
    ) (t.t_name :: t.t_child) [] in

    let create_table sql = <:expr<
        let sql = $str:sql$ in
        Sql_access.db_must_ok db (fun () -> Sqlite3.exec db.Sql_access.db sql)
     >> in
    let create_statements = Ast.exSem_of_list (List.map create_table sql_decls) in
    (* the final init_db binding for the SQL creation function *)
    let init_db_binding = <:binding< $lid:fun_name$ db = do { $create_statements$ } >> in
  
    (* the _new creation function to spawn objects of the SQL type *)
    let fun_name = sprintf "%s_new" t.t_name in
    let fields = exposed_fields env t.t_name in
    let fun_args = List.map (fun (_,f) ->
      match f.f_opt with
      |true -> <:patt< ?($lid:f.f_name$ = None) >>
      |false -> <:patt< ~ $lid:f.f_name$ >>
    ) fields in 
    let new_get_set_functions = List.flatten (List.map (fun (_,f) ->
      let internal_var_name = sprintf "_%s" f.f_name in
      let external_var_name = f.f_name in
      [ 
        <:class_str_item< value mutable $lid:internal_var_name$ = $lid:external_var_name$ >>;
        <:class_str_item< method $lid:external_var_name$ = $lid:internal_var_name$ >>;
        <:class_str_item< method $lid:"set_"^external_var_name$ v = ( $lid:internal_var_name$ := v ) >>;
      ]
    ) fields) in

    (* implement the save function *)
    let foreign_single_ids = List.map (fun f ->
        let id = f.f_name in
        <:binding< $lid:"_"^id^"_id"$ = self#$lid:id$#save >>
      ) (foreign_single_fields env t.t_name) in 
  
    let save_new_record = <:expr< prerr_endline "new" >> in 
    let save_main = <:binding<
        _curobj_id = match _id with [
         None ->  $save_new_record$
        |Some _ -> ()
        ]
    >> in
    let save_bindings =  foreign_single_ids @ [ save_main ] in
    let save_fun = biList_to_expr _loc save_bindings <:expr< () >> in
    let new_admin_functions =
       [
         <:class_str_item< method delete = failwith "delete not implemented" >>;
         <:class_str_item< method save   = $save_fun$ >>;
       ] in
    let new_functions = new_get_set_functions @ new_admin_functions in
    let new_body = <:expr<
        object(self)
         $Ast.crSem_of_list new_functions$;
        end
      >> in
    let new_binding = function_with_label_args _loc 
      ~fun_name 
      ~final_ident:"db"
      ~function_body:new_body
      ~return_type:<:ctyp< $lid:type_name$ >>
      fun_args in

    (* return single binding of all the functions for this type *)
    Ast.biAnd_of_list [init_db_binding; new_binding]
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
      let env = List.fold_right (fun t env ->
        let f = { Types.f_id = t.Types.td_id; f_mut = false; f_typ = t.Types.td_typ } in
        Sql_types.process env "__top" f;
      ) ts Sql_types.empty_env in
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

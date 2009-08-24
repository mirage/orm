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
  |Types.Tuple (_, tyl) -> <:ctyp< $tup:tySta_of_list (List.map (ast_of_caml_type _loc) tyl)$ >>
  |x -> failwith ("ast_of_caml_type: " ^ (Types.string_of_typ x))

open Sql_types

let id_expr_of_field _loc f =
  <:expr< $lid:"_"^f.f_name$ >>

(* convert a field to a Sqlite3.Data fragment *)
let field_to_sql_data _loc f id =
  let ctyp = match f.f_ctyp with |None -> assert false |Some c -> c in
  let rec simple_type = function
    |Types.Unit _ -> <:expr< Sqlite3.Data.INT 1L >>
    |Types.Int _ -> <:expr< Sqlite3.Data.INT (Int64.of_int $id$) >>
    |Types.Int32 _ -> <:expr< Sqlite3.Data.INT (Int64.of_int32 $id$) >>
    |Types.Int64 _ -> <:expr< Sqlite3.Data.INT $id$ >>
    |Types.Float _ -> <:expr< Sqlite3.Data.REAL $id$ >>
    |Types.Char _ -> <:expr< Sqlite3.Data.INT (Int64.of_int (Char.code $id$)) >>
    |Types.String _ -> <:expr< Sqlite3.Data.TEXT $id$ >>
    |Types.Bool _ -> <:expr< Sqlite3.Data.INT (if $id$ then 1L else 0L) >>
    |x -> failwith ("field_to_sql_data: " ^ (Types.string_of_typ x))
  in
  match ctyp with
  |Types.Option (_,ty) ->
    <:expr< match $id$ with [ 
       None -> Sqlite3.Data.NULL
      |Some $lid:"_"^f.f_name$ -> $simple_type ty$ 
    ] >>
  |Types.Apply (_,[],id,[]) -> 
    <:expr< Sqlite3.Data.INT $lid:"_"^f.f_name^"_id"$ >>
  |x -> simple_type x

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
    let accessor_fields = List.flatten (List.map (fun f ->
      let ctyp = ast_of_caml_type _loc (ctyp_of_field f) in
      [ <:ctyp< $lid:f.f_name$ : $ctyp$ >> ;
       <:ctyp< $lid:"set_" ^ f.f_name$ : $ctyp$ -> unit >> ]
    ) fields) in
    let other_fields = [
      <:ctyp< save: int64 >>;
      <:ctyp< delete: unit >>;
    ] in
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
    (* open function to first access a sqlite3 db *)
    let init_fun_name = sprintf "%s_init_db" t.t_name in
    (* init function to initalize sqlite database *)
    let type_name = sprintf "%s_persist" t.t_name in

    let sql_decls = List.fold_right (fun t a ->
      let fields = sql_fields env t in
      let sql_fields = List.map (fun f ->
        sprintf "%s %s" f.f_name (string_of_sql_type f)
      ) fields in
      sprintf "CREATE TABLE IF NOT EXISTS %s (%s)" t (String.concat ", " sql_fields) :: a
    ) (t.t_name :: t.t_child) [] in

    let create_table sql = <:expr<
        let sql = $str:sql$ in
        Sql_access.db_must_ok db (fun () -> Sqlite3.exec db.Sql_access.db sql)
     >> in
    let create_statements = Ast.exSem_of_list (List.map create_table sql_decls) in
    (* the final init_db binding for the SQL creation function *)
    let init_db_binding = <:binding< $lid:init_fun_name$ db = do { $create_statements$ } >> in
  
    (* the _new creation function to spawn objects of the SQL type *)
    let new_fun_name = sprintf "%s_new" t.t_name in
    let fields = exposed_fields env t.t_name in
    let fun_args = List.map (fun f ->
      match f.f_opt with
      |true -> <:patt< ?($lid:f.f_name$ = None) >>
      |false -> <:patt< ~ $lid:f.f_name$ >>
    ) fields in 
    let new_get_set_functions = List.flatten (List.map (fun f ->
      let cty = ast_of_caml_type _loc (ctyp_of_field f) in
      let internal_var_name = sprintf "_%s" f.f_name in
      let external_var_name = f.f_name in [
        <:class_str_item<
          value mutable $lid:internal_var_name$ : $cty$ = $lid:external_var_name$
        >>;
        <:class_str_item< 
          method $lid:external_var_name$ = $lid:internal_var_name$ 
        >>;
        <:class_str_item< 
          method $lid:"set_"^external_var_name$ v = ( $lid:internal_var_name$ := v ) 
        >>;
      ]
    ) fields) in

    (* -- implement the save function *)

    (* bindings for the ids of all the foreign-single fields *)
    let foreign_single_ids = List.map (fun f ->
        let id = f.f_name in
        <:binding< $lid:"_"^id^"_id"$ = self#$lid:id$#save >>
      ) (foreign_single_fields env t.t_name) in 
    (* the INSERT statement for this object *)
    let insert_sql = sprintf "INSERT INTO %s VALUES(%s);" t.t_name
      (String.concat "," (List.map (fun f -> 
        if f.f_info = Internal_autoid then "NULL" else "?") (sql_fields env t.t_name))) in
    (* Bind any tuples to individual variables first *)
    let tuple_bind_binding = Hashtbl.fold (fun n' fl' a ->
       let fl = List.map (fun f -> <:patt< $lid:"_"^f.f_name$ >> ) fl' in
       <:binding< $tup:paCom_of_list fl$ = $lid:"_"^n'$ >> :: a
    ) (tuple_fields env t.t_name) [] in
    (* the Sqlite3.bind for each simple statement *)
    let sql_bind_pos = ref 0 in
    let sql_bind_binding = List.map (fun f ->
       incr sql_bind_pos;
       let id = id_expr_of_field _loc f in
       let v = field_to_sql_data _loc f id in
       <:binding< 
        () = Sql_access.db_must_ok db (fun () -> 
               Sqlite3.bind stmt $`int:!sql_bind_pos$ $v$
          )
       >>
    ) (sql_fields_no_autoid env t.t_name) in
    let save_main = <:binding<
       _curobj_id = match _id with [
         None -> $biList_to_expr _loc (
          <:binding< stmt = Sqlite3.prepare db.Sql_access.db $str:insert_sql$ >>
            :: tuple_bind_binding @ sql_bind_binding) 
            <:expr<
              let __id = Sqlite3.last_insert_rowid db.Sql_access.db in
              do { _id := Some __id; __id }
            >>
          $
        |Some _ -> 0L
      ]
    >> in
    let save_bindings =  foreign_single_ids @ [ save_main ] in
    let save_fun = biList_to_expr _loc save_bindings <:expr< _curobj_id >> in

    (* -- hook in the admin functions (save/delete) *)
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
      ~fun_name:new_fun_name
      ~final_ident:"db"
      ~function_body:new_body
      ~return_type:<:ctyp< $lid:type_name$ >>
      fun_args in

    (* return single binding of all the functions for this type *)
    Ast.biAnd_of_list [init_db_binding; new_binding]
  ) tables) in
  <:str_item< value $fn_bindings$ >>

(* An initialization function for the whole group of statements *)
let construct_init env =
  let _loc = Loc.ghost in
  (* XXX default name until its parsed into environment *)
  let name = "orm" in
  <:str_item<
    value $lid:name^"_init_db"$ db_name = 
      let db = Sql_access.new_state db_name in
      do {
        $exSem_of_list (List.map (fun t ->
            <:expr< $lid:t.t_name ^ "_init_db"$ db >>
          ) (exposed_tables env))$;
        db
      };
  >>

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
          $construct_funs env$;
          $construct_init env$
        >>
      )

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
   
(* Return the fields of a record/object type *)
let fields_of_record t =
  match t.td_typ with
  |Record (_,fl) |Object (_,fl) ->  fl
  |_ -> failwith "fields_of_record: unexpected type"

(* Convert an OCaml type into the value exposed in the persist object.
   Not just directly passing through the original type so we can throw
   an error explicitly if its not supported *)
exception Unsupported_type of string
let unsupported ty = raise (Unsupported_type ty)

type sql_type = {
  s_opt: bool;     (* is this type optional *)
  s_foreign: bool; (* is this type a foreign key *)
  s_list: bool;    (* is this type a list *)
}

let classify_type _loc f =
  let simple_classify _loc sty = function
    |Int _ ->    f, sty, <:ctyp< int >>
    |Int32 _ ->  f, sty, <:ctyp< int32 >>
    |Int64 _ ->  f, sty, <:ctyp< int64 >>
    |Float _ ->  f, sty, <:ctyp< float >>
    |Bool _ ->   f, sty, <:ctyp< bool >>
    |Char _ ->   f, sty, <:ctyp< char >>
    |String _ -> f, sty, <:ctyp< string >>
    |Apply (_loc, _, id, _) -> 
      let sty = { sty with s_foreign=true } in
      f, sty, <:ctyp< $lid:id$ >>
    |x -> unsupported (Types.string_of_typ x)
  in
  (* decompose a list or option type into the basic type and set options *)
  let base = { s_opt=false; s_foreign=false; s_list=false } in
  match f.f_typ with
  |Option (_, ty) -> simple_classify _loc { base with s_opt=true } ty
  |List (_, ty) ->   simple_classify _loc { base with s_list=true } ty
  |ty ->             simple_classify _loc base ty
  
(* convert a simple ocaml type into a sql type *)
let sql_type_of_ocaml_type = function
  | <:ctyp< int >> 
  | <:ctyp< int32 >> 
  | <:ctyp< int64 >>  -> "INTEGER"
  | <:ctyp< float >>  -> "REAL"
  | <:ctyp< bool >> 
  | <:ctyp< char >>   -> "INTEGER"
  | <:ctyp< string >> -> "TEXT"
  | x -> unsupported  "sql_type_of_ocaml_type:"

(* get the basic fields (i.e. not lists, options, foreign) from a list *)
let get_basic_fields _loc fl =
  let cltys = List.map (classify_type _loc) fl in
  List.filter (fun (_,s,_) -> s.s_foreign = false && s.s_list = false) cltys

(* Return the accessor typedefs (the method/set_method) for a 
   particular OCaml type *)
let accessor_funcs_of_typedef _loc f =
  prerr_endline (Types.string_of_typ f.f_typ);
  let f, sty, ty = classify_type _loc f in
  prerr_endline (sprintf "%s: opt=%b list=%b for=%b" f.f_id sty.s_opt sty.s_list sty.s_foreign);
  let ty = match sty with
    |{ s_opt = true } -> <:ctyp< option $ty$ >>
    |{ s_list = true } -> <:ctyp< list $ty$ >>
  |_ -> ty in
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
    let basic_fields = get_basic_fields _loc fields in
    let sqls = String.concat ", " (pid :: List.map (fun (f,_,oty) ->
        sprintf "%s %s" f.f_id (sql_type_of_ocaml_type oty)
      ) basic_fields) in
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

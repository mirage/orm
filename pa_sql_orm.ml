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

(* build something like 'f x1 x2 x3 ... xn' *)
let apply _loc f label_args =
  let make x = Ast.ExId (_loc, Ast.IdLid (_loc, x)) in
  let make_label x = Ast.ExLab (_loc, x, Ast.ExId (_loc, Ast.IdLid (_loc, x))) in
  let rec aux = function
  | []   -> Ast.ExApp (_loc, make f, make "()")
  | [h]  -> Ast.ExApp (_loc, make f, make_label h)
  | h::t -> Ast.ExApp (_loc, aux t , make_label h) in
  let aux0 = function
  | [] -> aux []
  | h::t -> Ast.ExApp (_loc, aux t , make h) in
  aux0 (List.rev label_args)

let access_array _loc a i =
  let make x = Ast.ExId (_loc, Ast.IdLid (_loc, x)) in
  Ast.ExAre (_loc, make a, Ast.ExInt (_loc, string_of_int i))
	
(* List.map with the integer position passed to the function *)
let mapi fn =
  let pos = ref 0 in
  List.map (fun x ->
    incr pos;
    fn !pos x
  ) 
    
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
    let accessor_fields = List.flatten (List.map (fun f ->
    let ctyp = if is_foreign f then <:ctyp< $f.f_ctyp$ >> else <:ctyp< $f.f_ctyp$ >> in 
      [ <:ctyp< $lid:f.f_name$ : $ctyp$ >> ;
       <:ctyp< $lid:"set_" ^ f.f_name$ : $f.f_ctyp$ -> unit >> ]
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
let construct_object_funs env =
  let _loc = Loc.ghost in
  let tables = exposed_tables env in
  (* output the function bindings *)
  List.flatten (List.map (fun t ->
    (* init function to initalize sqlite database *)
    let type_name = sprintf "%s_persist" t.t_name in
 
    (* the _new creation function to spawn objects of the SQL type *)
    let new_fun_name ~_lazy = sprintf "%s_new%s" t.t_name (if _lazy then "_lazy" else "") in
    let fields = exposed_fields env t.t_name in
    let fun_args = List.map (fun f ->
      if is_optional_field f then
        <:patt< ?($lid:f.f_name$ = None) >>
      else
        <:patt< ~ $lid:f.f_name$ >>
    ) fields in

    let new_set_get_functions ~_lazy = List.flatten (List.map (fun f ->
      let _lazy = is_foreign f && _lazy in
      let internal_var_name = sprintf "_%s" f.f_name in
      let make_lazy x = if _lazy then <:ctyp< unit -> ($x$)>> else <:ctyp< $x$ >> in
      let ctyp = match f.f_ctyp with 
        | <:ctyp< option $x$>> -> <:ctyp< option ($make_lazy x$)>>
	    | x -> make_lazy x in
      let lazy_force x = if _lazy then <:expr< $lid:x$ () >> else <:expr< $lid:x$ >> in 
      let lazy_assign x = if _lazy then <:expr< $lid:x$ := (fun () -> v) >> else <:expr< $lid:x$ := v >> in
      let external_var_name = f.f_name in [
        <:class_str_item<
          value mutable $lid:internal_var_name$ : $ctyp$ = $lid:external_var_name$
        >>;
        <:class_str_item< 
          method $lid:external_var_name$ = $lazy_force internal_var_name$
        >>;
        <:class_str_item< 
          method $lid:"set_"^external_var_name$ v = ( $lazy_assign internal_var_name$ ) 
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
        if is_autoid f then "NULL" else "?") (sql_fields env t.t_name))) in

    (* the UPDATE statement for this object *)
    let update_sql = sprintf "UPDATE %s SET %s WHERE id=?;" t.t_name
      (String.concat "," (List.map (fun f ->
          sprintf "%s=?" f.f_name
        ) (sql_fields_no_autoid env t.t_name))) in

    (* the Sqlite3.bind for each simple statement *)
    let sql_bind_pos = ref 0 in
    let sql_bind_expr = List.map (fun f ->
       incr sql_bind_pos;
       let v = field_to_sql_data _loc f in
       <:expr< 
        Sql_access.db_must_ok db (fun () -> 
               Sqlite3.bind stmt $`int:!sql_bind_pos$ $v$)
       >>
    ) (sql_fields_no_autoid env t.t_name) in

    (* last binding for update statement which also needs an id at the end *)
    incr sql_bind_pos;

    (* the main save function *)
    let save_main = <:binding<
       _curobj_id =
       let stmt = Sqlite3.prepare db.Sql_access.db 
         (match _id with [
            None -> $str:insert_sql$
           |Some _ -> $str:update_sql$
          ]
         ) in
       do {
         $exSem_of_list sql_bind_expr$;
         match _id with [
           None -> ()
          |Some _id -> 
            Sql_access.db_must_bind db stmt $`int:!sql_bind_pos$ (Sqlite3.Data.INT _id)
         ];
         Sql_access.db_must_step db stmt;
         match _id with [
           None -> 
            let __id = Sqlite3.last_insert_rowid db.Sql_access.db in
            do { _id := Some __id; __id }
          |Some _id ->
            _id
         ]
       }
    >> in
   
    let save_bindings = foreign_single_ids @ [ save_main ] in
    let save_fun = biList_to_expr _loc save_bindings <:expr< _curobj_id >> in

    (* -- hook in the admin functions (save/delete) *)
    let new_admin_functions =
       [
         <:class_str_item< method delete = failwith "delete not implemented" >>;
         <:class_str_item< method save   = $save_fun$ >>;
       ] in
    let new_functions ~_lazy = new_set_get_functions ~_lazy @ new_admin_functions in

    let new_body ~_lazy = <:expr<
        object(self)
         $Ast.crSem_of_list (new_functions ~_lazy)$;
        end
      >> in

    let new_binding ~_lazy = function_with_label_args _loc 
      ~fun_name:(new_fun_name ~_lazy)
      ~final_ident:"db"
      ~function_body:(new_body ~_lazy)
      ~return_type:<:ctyp< $lid:type_name$ >>
      fun_args in

    (* return single binding of all the functions for this type *)
    [ new_binding ~_lazy:false; new_binding ~_lazy:true ]
  ) tables)

(* force functions *)
let construct_force_functions env =
  let _loc = Loc.ghost in
  let tables = exposed_tables env in
  let foreign_tables, not_foreign_tables  = List.partition (fun t -> List.exists is_foreign t.t_fields) tables in 
  List.map (fun t ->
    let foreign_fields = List.filter is_foreign t.t_fields in

    let force_body =
      let force_exprs =
        List.map (fun f -> <:expr<
            let key = ($str:get_foreign f$, $lid:t.t_name$ # $lid:f.f_name$#id) in
            if Hashtbl.mem cache key
            then $lid:t.t_name$ # $lid:"set_"^f.f_name$ (Hashtbl.find cache key)
            else (do {
              Hashtbl.replace cache key $lid:t.t_name$ # $lid:f.f_name$;
              $lid:get_foreign f^"_force"$ ~cache $lid:t.t_name$ # $lid:f.f_name$
            })
          >>)
          foreign_fields in
      <:expr< do { $exSem_of_list force_exprs$ } >>
    in

    let force_binding = function_with_label_args _loc
      ~fun_name:(t.t_name^"_force")
      ~final_ident:t.t_name
      ~function_body:force_body
      ~return_type:<:ctyp< unit >>
      [ <:patt< ~cache >> ]
    in

    force_binding) 
    foreign_tables 
  @ List.map (fun t -> <:binding< $lid:t.t_name^"_force"$ ~cache $lid:t.t_name$ = () >>) not_foreign_tables
    

(* get functions *)
let construct_get_functions env =
  let _loc = Loc.ghost in
  let tables = exposed_tables env in
  List.flatten (List.map (fun t ->
    let type_name = sprintf "%s_persist" t.t_name in
    let fields = exposed_fields env t.t_name in
    let str_fields = List.map (fun f -> f.f_name) fields in
    let foreign_fields, not_foreign_fields = List.partition is_foreign fields in
    let new_lazy_object = <:expr< $apply _loc (sprintf "%s_new_lazy" t.t_name) (str_fields @ ["db"]) $ >> in

    let get_body arg =
      let select_clause = String.concat ", " str_fields in
      let where_clause = match arg with
      | None -> sprintf "custom_fn(%s)" select_clause
      | Some f -> sprintf "%s=" f.f_name in
      let sql = sprintf "SELECT %s FROM %s WHERE %s" select_clause t.t_name where_clause in
      let get_bindings =		
        mapi (fun i f ->
          let x = <:expr<
            let $lid:"_" ^ f.f_name$ = Sqlite3.column stmt $`int:i-1$ in
            $sql_data_to_field _loc f$
          >> in
          if is_foreign f then
            <:binding< $lid:f.f_name$ = (fun () -> $lid:get_foreign f^"_get_by_id"$ ~_lazy:True ~id:$x$ db) >>
          else
            <:binding< $lid:f.f_name$ = $x$ >>
        ) fields in 

      let sql_expr = match arg with
        | None -> <:expr< $str:sql$ >> 
        | Some f -> <:expr< $str:sql$ ^ $to_string _loc f$ >> in
      let final_expr = match arg with
        | Some f when is_autoid f -> 
               <:expr< match (Sql_access.step_fold db stmt of_stmt) with
                 [ [] -> raise Not_found | [h] -> h | _ -> failwith "TODO" ]
               >> 
        | Some f -> <:expr< Sql_access.step_fold db stmt of_stmt >>
        | None ->
            let access i f = 
              <:expr< let $lid:"_"^f.f_name$ = $access_array _loc "__sql_array__" (i-1)$ in 
                $sql_data_to_field _loc f$ >> in
            let not_foreign_bindings =
               mapi (fun i f -> <:binding< $lid:f.f_name$ = $access i f$ >>) not_foreign_fields in
            let foreign_binding =
               mapi (fun i f -> 
                   <:binding< $lid:f.f_name$ () = $lid:get_foreign f^"_get_by_id"$ ~_lazy ~id: $access i f$ db>>
                 ) foreign_fields in
            <:expr<
              let custom_fn __sql_array__ =
                let x =
                  $biList_to_expr _loc (not_foreign_bindings @ foreign_binding) new_lazy_object$ in
                fn x in
	      do {
                Sqlite3.create_funN db.Sql_access.db "custom_fn" custom_fn;
                Sql_access.step_fold db stmt of_stmt } >> in

      <:expr< 
        let sql = $sql_expr$ in
        let stmt = Sqlite3.prepare db.Sql_access.db sql in
        let of_stmt stmt =
          $
          if foreign_fields = [] then
            biList_to_expr _loc get_bindings new_lazy_object
          else
            <:expr< 
              let $lid:"__"^t.t_name$ = $biList_to_expr _loc get_bindings new_lazy_object$ in
              do {
                if not _lazy then $lid:t.t_name^"_force"$ ~cache:(Hashtbl.create 64) $lid:"__"^t.t_name$ else ();
                $lid:"__"^t.t_name$ } 
            >>$ 
        in
        $final_expr$
      >> in

    let get_fun_name = function 
      | None -> sprintf "%s_get" t.t_name 
      | Some f ->  sprintf "%s_get_by_%s" t.t_name f.f_name in
    let return_type = function
      | Some f when is_autoid f -> <:ctyp< $lid:type_name$ >>
      | _ -> <:ctyp< list $lid:type_name$ >> in
    let get_argument = function | None -> <:patt< ~fn >> | Some f -> <:patt< ~ $lid:f.f_name$ >> in
    let get_binding arg = function_with_label_args _loc
      ~fun_name:(get_fun_name arg)
      ~final_ident:"db"
      ~function_body:(get_body arg)
      ~return_type:(return_type arg)
      [ <:patt< ~_lazy >>; get_argument arg ] in
     (List.map (fun x -> get_binding (Some x)) not_foreign_fields) @ [ get_binding None ]
  ) tables)

let construct_funs env =
  let _loc = Loc.ghost in
  let bs = List.fold_left (fun a f -> f env @ a) []
    [ construct_object_funs ] in
  <:str_item< 
     value rec $biAnd_of_list bs$ >>

(* --- Initialization functions to create tables and open the db handle *)

let init_db_funs env =
  let _loc = Loc.ghost in
  Ast.exSem_of_list (List.map (fun t ->
    (* open function to first access a sqlite3 db *)
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
    <:expr< do { $create_statements$ } >>
  ) (sql_tables env))
 
let construct_init env =
  let _loc = Loc.ghost in
  (* XXX default name until its parsed into environment *)
  let name = "orm" in
  <:str_item<
    value $lid:name^"_init_db"$ db_name = 
      let db = Sql_access.new_state db_name in
      do {
        $init_db_funs env$; db
      };
  >>

let construct_sexp env =
  let _loc = Loc.ghost in
  let sexp_of_bindings = List.map (fun (n,t) ->
      Pa_sexp_conv.Generate_sexp_of.sexp_of_td _loc n [] t
    ) (sexp_tables env) in
  let bindings_of_sexp = List.map (fun (n,t) ->
      let internal_binding,external_binding = 
         Pa_sexp_conv.Generate_of_sexp.td_of_sexp _loc n [] t in
      <:binding< $internal_binding$ and $external_binding$ >>
    ) (sexp_tables env) in
  <:str_item< value rec $biAnd_of_list (sexp_of_bindings @ bindings_of_sexp)$ >>
 
(* Register the persist keyword with type-conv *)
let () =
  add_generator_with_arg
    "persist"
    sql_parms
    (fun tds args ->
      prerr_endline "in add_generator_with_arg: persist";
      let _loc = Loc.ghost in
      let env = process tds in
      prerr_endline (Sql_types.string_of_env env);
      match tds, args with
      |_, None ->
        Loc.raise (Ast.loc_of_ctyp tds) (Stream.Error "pa_sql_orm: arg required")
      |_, Some name ->
        let _loc = Loc.ghost in
        <:str_item<
          $construct_sexp env$;
          $construct_typedefs env$;
          $construct_funs env$;
          $construct_init env$;
          value rec $biAnd_of_list (construct_force_functions env)$;
          value rec $biAnd_of_list (construct_get_functions env)$;
        >>
      )

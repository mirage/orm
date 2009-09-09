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

(* build something like 'f ~x1 ~x2 ~x3 ... xn' *)
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

(* declare the types used to pass SQL objects back and forth *)
let construct_typedefs env =
  let _loc = Loc.ghost in
  let tables = exposed_tables env in

  let cache_decls =
    let sum_type = List.map (fun t ->
      let ctyp = match t.t_type with
        |Exposed -> <:ctyp< $lid:t.t_name$  >>
        | _ -> t.t_ctyp 
      in
      (_loc, (fcachefn t), [ ctyp ])
    ) env.e_tables 
    in
    let sum_type =
      if List.length sum_type = 1 then
        <:ctyp< $lid:(List.hd tables).t_name$ >>
      else
        <:ctyp< [ $sum_type_of_list sum_type$ ] >> in
      declare_type _loc "cache_elt" sum_type in
 
  let unique_table_child_iter fn t = 
    let h = Hashtbl.create 1 in
    let rec child = function
     |[] -> []
     |hd::tl ->
       with_table (fun env table ->
        if Hashtbl.mem h table.t_name then
           child table.t_child
         else (
            Hashtbl.add h table.t_name ();
            fn t @ (child table.t_child)
         )
       ) env hd @ (child tl)
    in child (t.t_name :: t.t_child)
  in

  let id_decls = Ast.tyAnd_of_list (
     List.map (fun t ->
       let r = unique_table_child_iter (fun t' ->
         List.map (fun f ->
           let cty = if is_foreign_exposed env f then 
              <:ctyp< $lid:tidfn (get_foreign_table env f)$ >>
           else
             <:ctyp< int64 >>
           in
           <:ctyp< $lid:fidfn f$ : mutable option $cty$ >>
         ) (id_fields env t'.t_name)
       ) t in
       declare_type _loc (tidfn t) <:ctyp< { $list:r$ } >> 
     ) tables
  ) in

  let new_id_decls = biAnd_of_list (
    List.map (fun t ->
      let r = unique_table_child_iter (fun t' ->
        List.map (fun f ->
            let x = match f.f_info with
              |Internal_autoid -> <:expr< __x >>
              |_ -> <:expr< None >> 
            in
            <:rec_binding< $lid:fidfn f$ = $x$ >>
          ) (id_fields env t'.t_name)
      ) t in
      <:binding< $lid:tnewfn t$ __x = { $rbSem_of_list r$ } >>
    ) tables
  ) in

  stSem_of_list [ 
    <:str_item< type $id_decls$ >>;
    <:str_item< type $cache_decls$  >>;
    <:str_item< type cache = Hashtbl.t (string * int64) cache_elt >>;
    <:str_item< value rec $new_id_decls$ >>
  ]

let save_expr env t =
    let _loc = Loc.ghost in
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

    let field_accessor f =
      <:expr< $lid:f.f_table$ . $lid:f.f_name$  >> in

    (* bindings for the ids of all the foreign-single fields *)
    let foreign_single_ids = List.map (fun f ->
        let id = f.f_name in
        let ftable = get_foreign_table env f in
        let ex = match ftable.t_type with
         |Variant
         |Tuple
         |List ->
            <:expr< failwith "not complete" >>
         |Exposed ->
            <:expr< $lid:savefn ftable$ 
              ~_id:(_id.$lid:fidfn f$) ~_cache:(Some _cache) db $field_accessor f$ >>
        in
        <:binding< $lid:"_"^id^"_id"$ = $ex$ >>
      ) (foreign_single_fields env t.t_name)
    in 

   let field_var_binds = List.map (fun f ->
       <:binding< $lid:"_"^f.f_name$ = $field_accessor f$ >>
     ) (sql_fields_no_autoid env t.t_name) in

   (* the main save function *)
   biList_to_expr _loc (foreign_single_ids @ field_var_binds)
    <:expr<
       let stmt = Sqlite3.prepare db.Sql_access.db 
         (match _id.$lid:fautofn env t$ with [
            None -> $str:insert_sql$
           |Some _ -> $str:update_sql$
          ]
         ) 
       in
       do {
         $exSem_of_list sql_bind_expr$;
         match _id.$lid:fautofn env t$ with [
           None -> ()
          |Some _id -> 
            Sql_access.db_must_bind db stmt $`int:!sql_bind_pos$ (Sqlite3.Data.INT _id)
         ];
         Sql_access.db_must_step db stmt;
         match _id.$lid:fautofn env t$ with [
           None ->
              let __id = Sqlite3.last_insert_rowid db.Sql_access.db in
              do {
                _id.$lid:fautofn env t$ := Some __id;
                __id
              }
          |Some _id ->
            _id
         ]
       } 
    >>  

let construct_save_funs env = 
  let _loc = Loc.ghost in
  let binds = biAnd_of_list (List.map (fun t ->
      <:binding< $lid:savefn t$ ?(_id=None) ?(_cache=None) db $lid:t.t_name$ = 
        let _id = match _id with 
          [  None -> $lid:tnewfn t$ None
           | Some _id -> _id
          ] in
        let _cache = match _cache with
          [  None -> Hashtbl.create 1
           | Some c -> c 
          ] in
        let save () = $save_expr env t$ in
        match _id.$lid:fautofn env t$ with 
        [ None ->
            save ()
         |Some _curobj_id ->
            if Hashtbl.mem _cache ($str:t.t_name$,_curobj_id) then
              ( _curobj_id )
            else
              (
              let _newobj_id = save () in 
              do {
                assert (_curobj_id = _newobj_id);
                Hashtbl.replace _cache ($str:t.t_name$,_newobj_id)
                  ($uid:fcachefn t$ $lid:t.t_name$);
                _newobj_id
              }
              )
        ]
        >>
    ) env.e_tables) 
  in
  <:str_item< value rec $binds$ >>

(* --- Initialization functions to create tables and open the db handle *)

let init_db_funs env =
  let _loc = Loc.ghost in
  Ast.exSem_of_list (List.map (fun table ->
    (* open function to first access a sqlite3 db *)
    let sql_decls = List.fold_right (fun t a ->
      let fields = sql_fields env t in
      let sql_fields = List.map (fun f ->
        sprintf "%s %s" f.f_name (string_of_sql_type f)
      ) fields in
      let prim_key = match table.t_type with 
        |List -> ", PRIMARY KEY(id, _idx)"
        |_ -> "" in
      sprintf "CREATE TABLE IF NOT EXISTS %s (%s%s)" 
        t (String.concat ", " sql_fields) prim_key :: a
    ) (table.t_name :: table.t_child) [] in

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
  <:str_item<
    value init db_name = 
      let db = Sql_access.new_state db_name in
      do {
        $init_db_funs env$; db
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
      let env = process tds in
      prerr_endline (Sql_types.string_of_env env);
      match tds, args with
      |_, None ->
        Loc.raise (Ast.loc_of_ctyp tds) (Stream.Error "pa_sql_orm: arg required")
      |_, Some name ->
        let _loc = Loc.ghost in
       (* XXX default name is Orm until its parsed into environment *)
        <:str_item<
        module Orm = struct
          $construct_typedefs env$;
          $construct_save_funs env$;
          $construct_init env$;
        end
        >>
      )

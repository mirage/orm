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
let fun_name ?(opt_arg1) ?(opt_arg2) ident1 ident2 = function_body ...
*)
let function_with_label_args _loc ~fun_name ~idents ~function_body ~return_type opt_args =
   let opt_args = opt_args @ (List.map (fun x -> <:patt< $lid:x$ >>) idents) in
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

(* build something like 'f ?x1 ?x2 ?x3 ... xn' *)
let apply _loc f label_args =
  let make x = Ast.ExId (_loc, Ast.IdLid (_loc, x)) in
  let make_label x = Ast.ExOlb (_loc, x, Ast.ExNil _loc) in
  let rec aux = function
  | []   -> make f
  | h::t -> Ast.ExApp (_loc, aux t , make_label h) in
  aux (List.rev label_args)

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
  let nlit = tables_no_list_item env in

  let id_decl =
    let ids = List.flatten (List.map (fun t -> [
      <:ctyp< $lid:tidfn t$ : $uid:whashfn t$.t int64 >> ;
      <:ctyp< $lid:tridfn t$ : $uid:rhashfn t$.t $ctyp_of_table t$ >> 
    ]) nlit) in
   declare_type _loc "_cache" <:ctyp< { $tySem_of_list ids$ } >>
  in

  let id_new = 
    let ids = List.flatten (List.map (fun t -> [
      <:rec_binding< $lid:tidfn t$ = $uid:whashfn t$.create 1 >> ;
      <:rec_binding< $lid:tridfn t$ = $uid:rhashfn t$.create 1 >>;
     ]) nlit) in
    <:binding< _cache_new () = { $rbSem_of_list ids$ } >>
  in

  let wh_decls = List.map (fun t ->
    <:str_item< 
      module $uid:whashfn t$ = Weaktbl.Make (
          struct 
            type t = $ctyp_of_table t$;
            value equal = ( == );
            value compare = ( == );
            value hash = Hashtbl.hash;
          end );
      module $uid:rhashfn t$ = Weaktbl.Make (
          struct
            type t = int64;
            value equal = (=);
            value compare = Int64.compare;
            value hash = Hashtbl.hash;
          end );
    >>
  ) nlit in

  let cache_decls =
    let sum_type = List.flatten (List.map (fun t ->
      [ (_loc, (fcachefn t), [ ]) ;  (* Full cache entry C_t *)
        (_loc, (fpcachefn t), [ ]) ; (* Partial cache entry P_t *)
      ]
    ) nlit)
    in
    let sum_type =
      if List.length sum_type = 1 then
        <:ctyp< $lid:(List.hd tables).t_name$ >>
      else
        <:ctyp< [ $sum_type_of_list sum_type$ ] >> in
      declare_type _loc "cache_elt" sum_type in

  stSem_of_list [ 
    <:str_item< $list:wh_decls$ >>;
    <:str_item< type $id_decl$ >>;
    <:str_item< value $id_new$ >>;
    <:str_item< type $cache_decls$ >>;
    <:str_item< type cache = Hashtbl.t (string * int64) cache_elt >>;
  ]

(* bind a variable to the contents of the current thing being saved *)
let field_var_binds env t =
  let _loc = Loc.ghost in
  let snif = sql_fields_no_autoid env t.t_name in
  let bs = match t.t_type with
    | Exposed -> 
      List.map (fun f -> <:binding< $lid:"_"^f.f_name$ = 
          $lid:f.f_table$.$lid:f.f_name$ >>) snif
    | Tuple -> 
      let fs = List.rev_map (fun f -> <:patt< $lid:"_"^f.f_name$ >>) snif in
      [ <:binding< ( $tup:paCom_of_list fs$ ) = $lid:t.t_name$ >> ]
    | Optional ->
      List.map (fun f -> match f.f_info with 
         |Internal_field -> (* this is the isset *)
            <:binding< $lid:"_"^f.f_name$ = match $lid:t.t_name$ with [
               None -> False | Some _ -> True ] >> 
         |_ -> <:binding< $lid:"_"^f.f_name$ = $lid:t.t_name$ >>
      ) snif
    | Variant vi ->
      List.map (fun f ->  match f.f_info with
         |Internal_field -> (* this is the index *)
          <:binding< $lid:"_"^f.f_name$ = match $lid:t.t_name$ with [
            $mcOr_of_list (Hashtbl.fold (fun id (n,args) a -> 
               (if args then (* <:patt< $uid:id$ _ >> else <:patt< $uid:id$ >> in *)
                 <:match_case< $uid:id$ _ -> $`int64:n$ >> 
               else
                 <:match_case< $uid:id$ -> $`int64:n$ >> )
              :: a) vi.v_indices [])
            $
           ]  >>
         |_ -> <:binding< >>
        ) snif
    | _ -> [ ]
  in biAnd_of_list bs

(* get an expression which can resolve an id and lookup cache correctly *)
let id_for_field_expr ~null_foreigns env f = 
  let _loc = Loc.ghost in
  let ft = get_foreign_table env f in
  match ft.t_type with
    |List_items  -> failwith "list item encountered"
    |List| Exposed |Tuple |Optional |Variant _ -> 
  if null_foreigns then
    <:expr< 0L >>
  else
    <:expr< 
      try 
        let __i = $uid:whashfn ft$.find db.Sql_access.cache.$lid:tidfn ft$ 
          $field_accessor f$ in
        match Hashtbl.mem _cache ( $str:ft.t_name$ , __i ) with [
          True -> __i
          | False ->  $lid:savefn ft$ ~_cache db $field_accessor f$ ]
      with 
      [ Not_found -> 
          $lid:savefn ft$ ~_cache db $field_accessor f$
      ]
    >>   

let sql_binding ~null_foreigns pos env t f =
  let _loc = Loc.ghost in
  let idex = if is_foreign f then  
    <:binding< $lid:"_"^f.f_name^"_id"$ = $id_for_field_expr ~null_foreigns env f$ >>
  else
    <:binding<  >>
  in
  let v = match t.t_type with 
  |Variant _ -> begin
    match f.f_info with
    |Internal_field -> (* this is the index *)
      field_to_sql_data _loc f
    |_ -> <:expr<
       match $lid:t.t_name$ with [
         $uid:String.capitalize f.f_name$ $lid:"_"^f.f_name$ -> 
           let $idex$ in
           $field_to_sql_data _loc f$
         | _ -> Sqlite3.Data.NULL
       ] >>
  end
  |Optional -> begin
    match f.f_info with
    |Internal_field -> (* this is the isset *)
      <:expr< $field_to_sql_data _loc f$ >>
    |_ -> <:expr<
      match $lid:"_"^f.f_name$ with [
        None -> Sqlite3.Data.NULL
       |Some $lid:"_"^f.f_name$ ->
          let $idex$ in 
           $field_to_sql_data _loc f$
      ] 
    >>
  end
  |_ -> 
    <:expr< let $idex$ in $field_to_sql_data _loc f$ >>
  in
  <:expr< Sql_access.db_must_bind db stmt $`int:pos$ $v$ >>
 

let save_expr ?(null_foreigns=false) env t =
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

    (* if there are no fields to update, then that would generate invalid 
       SQL above, so set no_update true so the code gen can skip update in this case *)
    let no_update = List.length (sql_fields_no_autoid env t.t_name) = 0 in

    (* the Sqlite3.bind for each simple statement *)
    let sql_bind_pos = ref 0 in
    let sql_bind_expr = List.map (fun f ->
      incr sql_bind_pos;
      sql_binding ~null_foreigns !sql_bind_pos env t f;
    ) (sql_fields_no_autoid env t.t_name) in

    (* last binding for update statement which also needs an id at the end *)
    incr sql_bind_pos;
    let sql_stmts = <:expr<
      do {
         $exSem_of_list sql_bind_expr$;
         match $lid:tidfn t$ with [
           None -> ()
          |Some _id -> 
            Sql_access.db_must_bind db stmt $`int:!sql_bind_pos$ (Sqlite3.Data.INT _id)
         ];
         Sql_access.db_must_step db stmt;
         match $lid:tidfn t$ with [
           None ->
              let __id = Sqlite3.last_insert_rowid db.Sql_access.db in
              do {
                $uid:whashfn t$.add db.Sql_access.cache.$lid:tidfn t$ $lid:t.t_name$ __id;
                $uid:rhashfn t$.add db.Sql_access.cache.$lid:tridfn t$ __id $lid:t.t_name$;
                __id
              }
          |Some _id ->
            _id
         ]
       } 
    >>  
    in

    (* the main save function *)
    if no_update then
      <:expr<
       match $lid:tidfn t$ with [
        None -> 
          let stmt = Sqlite3.prepare db.Sql_access.db $str:insert_sql$ in
          $sql_stmts$
       |Some _id -> _id
       ] 
      >>
    else <:expr<
       let stmt = Sqlite3.prepare db.Sql_access.db 
         (match $lid:tidfn t$ with [
            None -> $str:insert_sql$
           |Some _ -> $str:update_sql$
          ]) 
       in $sql_stmts$
    >>

let construct_save_funs env = 
  let _loc = Loc.ghost in
  let binds = biAnd_of_list (List.flatten (List.map (fun t ->
    let fsf = foreign_single_fields env t.t_name in
    let save_after_cache_check =
      if is_recursive_table env t then
        <:expr<
          try
            match Hashtbl.find _cache ($str:t.t_name$ , _curobj_id) with [
              $uid:fpcachefn t$ -> (* partial entry found, save children and update ids *)
                       let sql = $str:sprintf "UPDATE %s SET %s WHERE id=?;"
                         t.t_name (String.concat "," (List.map (fun f -> 
                           f.f_name ^ "=?") fsf))$ 
                       in 
                       let stmt = Sqlite3.prepare db.Sql_access.db sql in
                       do {
                         $exSem_of_list (
                           mapi (fun pos f -> 
                              sql_binding ~null_foreigns:false pos env t f
                           ) fsf
                         )$;
                        Sql_access.db_must_bind db stmt $`int:List.length fsf+1$
                          (Sqlite3.Data.INT _curobj_id);
                        Sql_access.db_must_step db stmt;
                        Hashtbl.replace _cache ($str:t.t_name$ , _curobj_id) 
                          $uid:fcachefn t$;
                        _curobj_id
                      }
              | $uid:fcachefn t$ -> (* full entry found, just return its id *)
                _curobj_id
              | _ -> assert False
            ]
          with [ Not_found -> save () ]
        >>
    else <:expr< save () >>
    in

  let list_saves = match t.t_type with
    |List -> begin
      (* get the child list_item table *) 
      with_table (fun env listitem ->

       (* XXX factor out binds with previous use *)
        let sql_bind_pos = ref 0 in
        let sql_bind_exprs = exSem_of_list (List.map (fun f ->
           let idex = if is_foreign f then  
             <:binding< $lid:"_"^f.f_name^"_id"$ = $id_for_field_expr ~null_foreigns:false env f$ >>
           else
             <:binding<  >>
           in
           incr sql_bind_pos;
           let v = field_to_sql_data _loc f in
           <:expr< 
             let $idex$ in
             Sql_access.db_must_bind db stmt $`int:!sql_bind_pos$ $v$ 
           >>
        ) (sql_fields_no_autoid env listitem.t_name)) in

        (* decide which iterator to use depending on the list type *)
        let access_fn, length_fn = match t.t_ctyp with
          | <:ctyp< array $c$ >> -> <:expr< Array.iteri >> , <:expr< Array.length >>
          | <:ctyp< list $c$ >> ->  <:expr< Sql_access.list_iteri >> , <:expr< List.length >>
          | _ -> failwith "table not array or list" in

        <:expr<  $access_fn$ (fun pos __item ->
            let __idx = Int64.of_int pos in
            let _id = _newobj_id in
            let stmt = Sqlite3.prepare db.Sql_access.db 
              $`str:sprintf "INSERT OR REPLACE INTO %s VALUES(%s);" listitem.t_name 
               (String.concat "," (List.map (fun _ -> "?") (sql_fields env listitem.t_name))) $
            in
            do { 
              $sql_bind_exprs$;  
              Sql_access.db_must_step db stmt;
              let stmt = Sqlite3.prepare db.Sql_access.db
                $`str:sprintf "DELETE FROM %s WHERE (id=?) AND (_idx > ?);" listitem.t_name$ in
              do {
                Sql_access.db_must_bind db stmt 1 (Sqlite3.Data.INT _id);
                (* XXX check for off-by-one here *)
                Sql_access.db_must_bind db stmt 2
                 (Sqlite3.Data.INT (Int64.of_int ($length_fn$ $lid:t.t_name$)));
                Sql_access.db_must_step db stmt
              }
            }
          ) $lid:t.t_name$ 
        >>
      ) env (match t.t_child with [x] -> x |_ -> failwith "listitem<>1") 
    end
    |_ -> <:expr< >>
  in
  let int_binding =
      <:binding< $lid:savefn t$ ~_cache db $lid:t.t_name$ = 
        let $lid:tidfn t$ = try Some ( 
            $uid:whashfn t$.find 
               db.Sql_access.cache.$lid:tidfn t$ $lid:t.t_name$ )
           with [ Not_found -> None ] in

        let $field_var_binds env t$ in
        let save () =
          let _newobj_id = $save_expr env t$ in
          do {
            $uid:whashfn t$.add db.Sql_access.cache.$lid:tidfn t$ $lid:t.t_name$ _newobj_id;
            $uid:rhashfn t$.add db.Sql_access.cache.$lid:tridfn t$ _newobj_id $lid:t.t_name$;
            Hashtbl.replace _cache ($str:t.t_name$,_newobj_id)
              $uid:fcachefn t$; 
            $list_saves$;
            _newobj_id
          } 
        in
        do { 
          let _curobj_id = match $lid:tidfn t$ with 
            [ None -> save ()
             |Some _curobj_id -> $save_after_cache_check$
            ] in
          _curobj_id
        }
        >>
    in
    let ext_binding = 
      <:binding< $lid:extsavefn t$ db $lid:t.t_name$ =
        let _cache = Hashtbl.create 1 in
        let $lid:tidfn t$ = try Some ( 
            $uid:whashfn t$.find 
               db.Sql_access.cache.$lid:tidfn t$ $lid:t.t_name$ )
           with [ Not_found -> None ] in

        (* first break the chain by inserting a partial save *)
        let $field_var_binds env t $ in
        let _curobj_id = $save_expr ~null_foreigns:true env t$ in
        (* add in a partial cache entry *)
        do {
          Hashtbl.add _cache ( $str:t.t_name$, _curobj_id ) $uid:fpcachefn t$;
          $uid:whashfn t$.replace db.Sql_access.cache.$lid:tidfn t$ $lid:t.t_name$ _curobj_id;
          $lid:savefn t$ ~_cache db $lid:t.t_name$
        }
      >>
    in 
    int_binding ::  (if t.t_type = Exposed then [ ext_binding ] else [])

    ) (tables_no_list_item env)))
  in
  <:str_item< value rec $binds$ >>

(* --- Get functions *)

(* construct the concrete value to return *)
let of_stmt env t =
  let _loc = Loc.ghost in
  let ef = exposed_fields_no_autoid env t.t_name in
  match t.t_type with
  |List ->
    let lit = match t.t_child with [lit] -> lit |_ -> assert false in
    let lif = list_item_field env lit in
    let sql = sprintf "SELECT %s.id,%s._idx,%s._item from %s LEFT JOIN %s ON %s.id = %s.id"
      lit lit lit lit t.t_name lit t.t_name in
    let id_sql = <:expr<
      match id with [ 
        Some (`Id _) -> $str:sprintf " WHERE %s.id=?" t.t_name$
      | None -> "" ] >> in
    <:expr<
      let sql = $str:sql$ ^ $id_sql$ ^ " ORDER BY _idx DESC" in
      let $debug t.t_name <:expr< sql >>$ in
      let stmt = Sqlite3.prepare db.Sql_access.db sql in
      do {
        match id with [
          None -> ()
        | Some (`Id i) -> Sql_access.db_must_bind db stmt 1 (Sqlite3.Data.INT i)
        ];
        let _id = ref (Sqlite3.Data.NULL) in
        let l = Sql_access.step_fold db stmt (fun stmt ->
          let $lid:"__"^lif.f_name$ = Sqlite3.column stmt 2 in
          do {
            _id.val := Sqlite3.column stmt 0;
            $sql_data_to_field ~null_foreigns:false _loc env lif$
          }
        ) in
        match l with [
          [] -> l (* empty list, no need for an id *)
        | _  -> let _id = match _id.val with [
          Sqlite3.Data.INT x -> x |x -> failwith (Sqlite3.Data.to_string x) ] in
          do {
            $whashex "add" t$ l _id;
            $rhashex "add" t$ _id l;
            l
          }
        ]
      }
    >>
  |Exposed ->
    let rb = mapi (fun pos f ->
      <:rec_binding< 
        $lid:f.f_name$ = let $lid:"__"^f.f_name$ = Sqlite3.column stmt $`int:pos-1$ in
            $sql_data_to_field ~null_foreigns:true _loc env f$
      >>
    ) ef in
    <:expr< { $rbSem_of_list rb$ } >>
  |Optional ->
    let isset = listi (fun f -> f.f_info = Internal_field) t.t_fields in
    let f = List.hd ef in
    let pos = listi (fun x -> f.f_name = x.f_name) t.t_fields in
    <:expr<
      match Sqlite3.column stmt $`int:isset$ with [
          Sqlite3.Data.INT 0L -> None
        | Sqlite3.Data.INT 1L -> Some (
          let $lid:"__"^f.f_name$ = Sqlite3.column stmt $`int:pos$ in
          $sql_data_to_field ~null_foreigns:false _loc env f$)
        | _ -> assert False ]
    >>
  |Tuple ->
    let tp = mapi (fun pos f ->
        <:expr<
          let $lid:"__"^f.f_name$ = Sqlite3.column stmt $`int:pos-1$ in
          $sql_data_to_field ~null_foreigns:false _loc env f$ >>
        ) ef in
      <:expr< ( $tup:exCom_of_list (List.rev tp)$ ) >>
  |Variant vi ->
      (* this assumes idx is the only Internal_field in table Variants *)
      let idx = listi (fun f -> f.f_info = Internal_field) t.t_fields in
      let mcs = Hashtbl.fold (fun vuid (id,args) a ->
        let ex = if args then begin
          let idx = listi (fun f -> f.f_name = String.lowercase vuid) t.t_fields in
          let f = List.nth t.t_fields idx in
          <:expr< $uid:vuid$ 
            (let $lid:"__"^f.f_name$ = Sqlite3.column stmt $`int:idx$ in
                  $sql_data_to_field ~null_foreigns:false _loc env f$)
          >>
        end else
          <:expr< $uid:vuid$ >> in
        <:match_case< Sqlite3.Data.INT $`int64:id$ -> $ex$ >>  :: a
      ) vi.v_indices [] in
      <:expr< 
        match Sqlite3.column stmt $`int:idx$ with [
          $mcOr_of_list mcs$
        | x -> failwith ("unexpected db return: " ^ (Sqlite3.Data.to_string x))
        ] >>
  |List_items -> assert false

let of_stmt_final env t =
  let _loc = Loc.ghost in
  with_table (fun env t -> 
    match list_or_option_fields env t.t_name, t.t_type with
    |[],_ -> <:expr< __v >> (* no list/option fields so no rewriting necessary *)
    |lof,Exposed ->
      let rbs = rbSem_of_list (List.map (fun f ->
        let pos = listi (fun f' -> f'.f_name = f.f_name) t.t_fields  in
        <:rec_binding< 
          $lid:f.f_name$ = let $lid:"__"^f.f_name$ = Sqlite3.column stmt $`int:pos$ in
              $sql_data_to_field ~null_foreigns:false _loc env f$
        >>
       ) lof) in
      Printf.eprintf "X=%d %d\n" (List.length lof) (List.length (exposed_fields env t.t_name));
      let v' = if List.length lof = (List.length (exposed_fields_no_autoid env t.t_name)) then
          <:expr< { $rbs$ } >>
        else
          <:expr< { (__v) with $rbs$ } >> in
      <:expr<
        let __v' = $v'$ in
        do {
          $whashex "replace" t$ __v' __id;
          $rhashex "replace" t$ __id __v';
          __v'
        }
      >>
    |_ -> <:expr< __v >> (* not an record, so no rewriting needed *)
  ) env t
 
let construct_get_funs env =
  let _loc = Loc.ghost in
  let tables = tables_no_list_item env in
  let bs = biAnd_of_list (List.flatten (List.map (fun t ->
    let fields = exposed_fields env t.t_name in
    let get_fun_name = getfn t in
    let ext_get_fun_name = extgetfn t in

    let get_body =
      (* build-up the SQL query *)
      let sql =
        let select_clause = String.concat ", " 
          (List.map (fun f -> f.f_name) (sql_fields env t.t_name)) in
        let where_0 = <:binding< __accu = [] >> in
        let where_of_field f =
          <:binding< __accu = match $lid:f.f_name$ with [
              None                -> __accu
            | Some $lid:f.f_name$ -> [ $ocaml_variant_to_sql_request _loc f$ :: __accu ] ]
          >> in
        let where_of_custom =
          <:binding< __accu = match fn with [
              None    -> List.rev __accu
            | Some fn -> List.rev [ $str:sprintf "custom_fn(%s)" select_clause$ :: __accu ] ]
          >> in
        let bindings =
          where_0 ::
            (List.map (fun f -> where_of_field f) fields)
          @ [ where_of_custom ] in
        biList_to_expr _loc bindings
          <:expr<
            let where_clause = String.concat " AND " __accu in
            $str:"SELECT "^select_clause^" FROM "^t.t_name$ ^
              (if where_clause = "" then "" else (" WHERE "^where_clause) ) 
          >> in

      (* build up SQL binds *)
      let binds = 
        let bind_of_field f =
          <:expr< match $lid:f.f_name$ with [
             None -> ()
           | Some $lid:f.f_name$ -> 
              $ocaml_variant_to_sql_binds _loc env f$ ]
          >> in
        exSem_of_list (List.map bind_of_field fields)
      in
    
      (* main body of the get call *)
      let body = <:expr<
        let lookup () =
          let of_stmt stmt = $of_stmt env t$ in
          let of_stmt_final stmt __id __v = $of_stmt_final env t.t_name$ in
          let sql = $sql$ in
          let stmt = Sqlite3.prepare db.Sql_access.db sql in
          let sql_bind_pos = ref 0 in
          let $debug (getfn t) <:expr< sql >>$ in
          do {
            $binds$;
            Sql_access.step_fold db stmt 
             (fun stmt ->
               let __v = of_stmt stmt in
               let __id = match Sqlite3.column stmt
                  $`int:listi (fun f -> f.f_info = Internal_autoid) t.t_fields$ with [
                   Sqlite3.Data.INT x -> x
                  |_ -> failwith "id not found" ] in
               do { 
                 $whashex "add" t$ __v __id;
                 $rhashex "add" t$ __id __v;
                 of_stmt_final stmt __id __v
               }
             )
          } in
        (* check the id cache for the object first *)
        match id with [
          Some (`Id i) -> 
            try [ $uid:rhashfn t$.find db.Sql_access.cache.$lid:tridfn t$ i ]
            with [ Not_found -> lookup () ]
        | None -> lookup ()
        ]
        >> in
      body
    in

    let str_fields = "fn" :: (List.map (fun f -> f.f_name) (exposed_fields env t.t_name)) in
    let ext_get_body =
      (* just assume recursive for the moment, so pass in a cache *)
      <:expr< 
       let _cache = Hashtbl.create 1 in
       $apply _loc (getfn t) str_fields$ _cache db
      >> 
    in

    let get_argument = <:patt< ? fn >> :: List.map (fun f ->
        <:patt< ? $lid:f.f_name$ >>
      ) (exposed_fields env t.t_name) in
    let return_type = <:ctyp< list $ctyp_of_table t$ >> in
    let int_get_binding = function_with_label_args _loc
      ~fun_name:get_fun_name
      ~idents:["_cache"; "db"]
      ~function_body:get_body
      ~return_type
      get_argument in
    let ext_get_binding = function_with_label_args _loc
     ~fun_name:ext_get_fun_name
     ~idents:["db"]
     ~function_body:ext_get_body
     ~return_type
      get_argument in
    [ int_get_binding; ext_get_binding ]
  ) tables)) in
  <:str_item< value rec $bs$ >>


(* --- Initialization functions to create tables and open the db handle *)

let init_db_funs env =
  let _loc = Loc.ghost in
  let exs = Ast.exSem_of_list (
    List.map (fun table ->
      (* open function to first access a sqlite3 db *)
      let sql_create =
        let fields = sql_fields env table.t_name in
        let sql_fields = List.map (fun f ->
          sprintf "%s %s%s" f.f_name (string_of_sql_type f) 
            (match table.t_type,f.f_info with
           |List_items,Internal_autoid -> ""
           |_,Internal_autoid -> " PRIMARY KEY AUTOINCREMENT"
           |_ -> "" )
        ) fields in
        let prim_key = match table.t_type with 
          |List_items -> ", PRIMARY KEY(id, _idx)"
          |_ -> "" in
        sprintf "CREATE TABLE IF NOT EXISTS %s (%s%s)" 
          table.t_name (String.concat ", " sql_fields) prim_key
      in

      let sql_cascade_delete =
        let fields = foreign_single_fields env table.t_name in
        let sqls = List.map (fun f ->
          sprintf
            ("CREATE TRIGGER IF NOT EXISTS %s_cascade_delete AFTER DELETE ON %s FOR EACH ROW BEGIN DELETE FROM %s WHERE id = OLD.%s; END;")
            table.t_name table.t_name (get_foreign f) f.f_name)
            fields in
        String.concat " " sqls in

      let sql_prevent_delete =
        let fields = foreign_single_fields env table.t_name in
        let sqls = List.map (fun f ->
          let foreign_table = get_foreign f in
          sprintf
            ("CREATE TRIGGER IF NOT EXISTS %s_%s_prevent_delete BEFORE DELETE ON %s FOR EACH ROW BEGIN SELECT RAISE(IGNORE) WHERE (SELECT id FROM %s WHERE %s = OLD.id) IS NOT NULL; END;")
            table.t_name f.f_name foreign_table table.t_name f.f_name)
            fields in
        String.concat " " sqls in
     
      <:expr< 
      do{
        Sql_access.db_must_ok db (fun () -> Sqlite3.exec db.Sql_access.db $str:sql_create$);
(*
        Sql_access.db_must_ok db (fun () -> Sqlite3.exec db.Sql_access.db $str:sql_cascade_delete$);
        Sql_access.db_must_ok db (fun () -> Sqlite3.exec db.Sql_access.db $str:sql_prevent_delete$);
*)
      }
      >>
  ) (sql_tables env)) in
  <:expr< do { $exs$ } >>
 
let construct_init env =
  let _loc = Loc.ghost in
  <:str_item<
    value init db_name = 
      let db = Sql_access.new_state (_cache_new ()) db_name in
      do {
        $init_db_funs env$; db
      };
  >>

(* TODO: take objects instead of id *)
let construct_delete env =
  let _loc = Loc.ghost in
  let tables = tables_no_list_item env in
  let fn table =
    let sql = sprintf "DELETE FROM %s WHERE id=" table.t_name in 
    let body =
      <:expr<
        let sql = $str:sql$ ^ Int64.to_string id in
        Sql_access.db_must_ok db (fun () -> Sqlite3.exec db.Sql_access.db sql) >>
    in
    function_with_label_args _loc
      ~fun_name:(table.t_name^"_delete")
      ~idents:["db"]
      ~function_body:body
      ~return_type:<:ctyp< unit >>
      [ <:patt< ~id >> ] in
   <:str_item< value $biAnd_of_list (List.map fn tables)$ >>

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
      let fout = open_out "debug.dot" in
      Printf.fprintf fout "%s" (Sql_types.dot_of_env env);
      close_out fout;
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
          $construct_get_funs env$;
          $construct_init env$;
  (*        $construct_delete env$; *)
        end
        >>
      )

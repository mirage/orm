(*
 * Copyright (c) 2009 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Printf

module Schema = struct
    type t =
    |Text
    |Blob
    |Int
    |Foreign of string
    |ForeignMany of string
    |Date

    type options = [
      |`Optional
      |`Unique
      |`Index
    ]

    type s = {
        name: string;
        ty: t;
        opt: bool;
        uniq: bool;
        idx: bool;
    }

    (* custom get generator requests *)
    type g = {
        want: s list;
        by: s list;
        want_id: bool; (* was the ID specifically requested*)
        want_all: bool; (* just get the entire object *)
    }

    let base ~flags ty n = {name=n; ty=ty; opt=(List.mem `Optional flags); uniq=(List.mem `Unique flags); idx=(List.mem `Index flags)}
    let text ?(flags=[]) = base ~flags Text
    let blob ?(flags=[]) = base ~flags Blob
    let date ?(flags=[]) = base ~flags Date
    let integer ?(flags=[]) = base ~flags Int
    let foreign ?(flags=[]) f = base ~flags (Foreign f)
    let foreign_many ?(flags=[]) f = base ~flags (ForeignMany f)
    let id () = integer ~flags:[`Optional] "id"

    type collection = (string * s list * g list) list

    (* add in an id field *)
    let make f : collection = 
       List.map (fun (n,v,gets) ->
         (* add an id field to every one of the gets *)
         let v = id () :: v in
         let gets = List.map (fun (ws,bs) -> 
           let find_f x = try List.find (fun f -> f.name = x) v with Not_found -> failwith (sprintf "not found: %s in %s" x n) in
           let wsf = List.map find_f ws in
           let bsf = List.map find_f bs in
           let wall = match ws with |[] -> true |_ -> false in
           if List.mem "id" ws then 
             {want=wsf; want_id=true; by=bsf; want_all=wall}
           else 
             {want=id () :: wsf; want_id=false; by=bsf; want_all=wall}
         ) gets in
         (n, v, gets)
       ) f

    let to_ocaml_type ?(always_opt=false) f =
      let opt = if always_opt then true else f.opt in
      match f.name,f.ty,opt with
      |_,Text,false -> "string"
      |_,Text,true -> "string option"
      |_,Blob,false -> "string" (* watch out for 16MB limit *)
      |_,Blob,true -> "string option"
      |_,Int,false -> "int64"
      |_,Int,true -> "int64 option"
      |_,Date,false -> "float"
      |_,Date,true -> "float option"
      |_,Foreign x,false -> sprintf "%s.t" (String.capitalize x)
      |_,Foreign x,true -> sprintf "%s.t option" (String.capitalize x)
      |_,ForeignMany x,_ -> sprintf "%s.t list" (String.capitalize x)

    let to_sql_type = function
    |Text -> "text"
    |Blob -> "blob"
    |Int -> "integer"
    |Foreign _ -> "integer"
    |ForeignMany _ -> assert false
    |Date -> "integer"

    let to_sql_type_wrapper = function
    |Text -> "Sqlite3.Data.TEXT v"
    |Blob -> "Sqlite3.Data.BLOB v"
    |Int -> "Sqlite3.Data.INT v"
    |Foreign _ -> "Sqlite3.Data.INT v"
    |ForeignMany _ -> assert false
    |Date -> "Sqlite3.Data.INT (Int64.of_float v)"

    let convert_from_sql alias f = match f.ty with
    |Text -> "Sqlite3.Data.to_string x"
    |Blob -> "Sqlite3.Data.to_string x"
    |Int -> sprintf "match x with |Sqlite3.Data.INT i -> i |x -> (try Int64.of_string (Sqlite3.Data.to_string x) with _ -> failwith \"error: %s %s\")" alias f.name
    |Foreign x -> "false (* XX *)"
    |ForeignMany _ -> assert false
    |Date -> "match x with |Sqlite3.Data.INT i -> Int64.to_float i|_ -> float_of_string (Sqlite3.Data.to_string x)"

    let map_table t f = match f.ty with
    |ForeignMany ft -> sprintf "map_%s_%s_%s" f.name t ft
    |_ -> assert false
    
    let ocaml_var_name f = f.name ^ match f.ty with
    |Foreign _ -> "_id"
    |_ -> ""

    let to_sql_table_alias f table = match f.ty with
    |Foreign ftable -> sprintf "%s_%s" ftable f.name
    |_ -> table

    let sql_var_name = ocaml_var_name 

    let get_table_fields (c:collection) table = 
       let _,fs,_ = List.find (fun (n,_,_) -> n = table) c in
       fs

    let partition_table_fields c table =
       List.partition (fun f -> match f.ty with ForeignMany _|Foreign _ -> true |_ -> false)
          (get_table_fields c table)

    let filter_out_id fields =
       List.filter (fun f -> f.name <> "id") fields

    let filter_singular_fields f =
       List.filter (fun f -> match f.ty with ForeignMany _ -> false |_ -> true) f

    let foreign_table_fields (all:collection) table =
      let rec fn table acc =
         let fs = get_table_fields all table in
         List.fold_left (fun a f ->
            match f.ty with
            |Foreign ftable ->
               let alias = sprintf "%s_%s" acc f.name in
               (fn ftable alias) @ ((ftable, alias, acc, f) :: a)
            |_ -> a
         ) [] fs
      in fn table table
end

open Printer_utils.Printer
open Schema


(* all has been filtered to remove top-level fields we dont want *)
let output_get_fn_sqlbind e (all:collection) module_name =
  let _, native_fields = partition_table_fields all module_name in
  (* select_tables keeps track of foreign tables we have joined on, for SELECTing later *)
  let select_tables = Hashtbl.create 1 in
  (* list of transitive foreign tables from this table which we need to LEFT JOIN on *)
  let foreign_table_fields = foreign_table_fields all module_name in
  (* calculate all the joins needed by foreign keys *)
  let joins = String.concat "" (List.rev_map (fun (table, table_alias, prefix, f) ->
      Hashtbl.add select_tables table table_alias;
      sprintf "LEFT JOIN %s AS %s ON (%s.id = %s.%s_id) " table table_alias table_alias prefix f.name
    ) foreign_table_fields) in
  Hashtbl.add select_tables module_name module_name;
  (* get all the field names to select, combination of the transitive foreign keys as well *)
  let col_positions = Hashtbl.create 1 in
  let sql_field_names_list = List.flatten (
    Hashtbl.fold (fun table table_alias a ->
      let fields = filter_singular_fields (get_table_fields all table) in
      List.map (fun f ->
        sprintf "%s.%s" table_alias (sql_var_name f)
      ) fields :: a;
    ) select_tables [];
  ) in
  let pos = ref 0 in
  List.iter (fun s -> Hashtbl.add col_positions s !pos; incr pos) sql_field_names_list;
  let sql_field_names = String.concat ", " sql_field_names_list in
  e += "let sql=\"SELECT %s FROM %s %s\" ^ q in" $ sql_field_names $ module_name $ joins;
  e -= "\"%s.get: \" ^ q" $ module_name;
  e += "let stmt=Sqlite3.prepare db.db sql in";
  native_fields, col_positions

let render_native_field_get e col_positions table_alias f =
  let col_pos = Hashtbl.find col_positions (table_alias ^ "." ^ f.name) in
  let from_sql = convert_from_sql table_alias f in
  (match f.opt with
   |false ->
     e += "(match Sqlite3.column stmt %d with" $ col_pos;
     e --> (fun e ->
       e += "|Sqlite3.Data.NULL -> failwith \"null of_stmt\"";
       e += "|x -> %s)" $ from_sql;
     );
   |true ->
     e += "(match Sqlite3.column stmt %d with" $ col_pos;
     e --> (fun e ->
       e += "|Sqlite3.Data.NULL -> None";
       e += "|x -> Some (%s))" $ from_sql;
     );
  )
 
let output_get_fn_of_stmt e all module_name col_positions =
  let rec of_stmt e table table_alias =
    let foreign_fields, fields = partition_table_fields all table in
    (if table = module_name then 
       e += "t" 
     else
       e += "%s.t" $ (String.capitalize table));
    e --> (fun e ->
      e --* "native fields";
      List.iter (fun f ->
        e += "~%s:(" $ f.name;
        render_native_field_get e col_positions table_alias f;
        e += ")"
      ) fields;
      e --* "foreign fields";
      List.iter (fun f ->
          e += "~%s:(" $ f.name;
          e --> (fun e ->
            match f.ty,f.opt with
              |Foreign ftable,false ->
                of_stmt e ftable (sprintf "%s_%s" table_alias f.name);
                e += ")"
              |Foreign ftable,true ->
                e += "(try";
                e += "Some (";
                of_stmt e ftable (sprintf "%s_%s" table_alias f.name);
                e += ") with _ -> None))"
              |ForeignMany ftable,_ ->
                e --* "foreign many-many mapping field";
                e += "let sql' = \"select %s_id from %s where %s_id=?\" in" $ ftable $ (map_table table f) $ table;
                e -= "\"%s.of_stmt (%s): \" ^ sql'" $ table $ ftable;
                e += "let stmt' = Sqlite3.prepare db.db sql' in";
                e += "let %s__id = Sqlite3.column stmt %d in" $ table $ (Hashtbl.find col_positions (table_alias ^ ".id"));
                e += "db_must_ok db (fun () -> Sqlite3.bind stmt' 1 %s__id);" $ table; 
                e += "List.flatten (step_fold db stmt' (fun s ->";
                e --> (fun e ->
                  e += "let i = match Sqlite3.column s 0 with |Sqlite3.Data.INT i -> i |_ -> assert false in";
                  e += "%s.get ~id:(Some i) db)" $ (String.capitalize ftable);
                );
                e += "))"
              |_ -> assert false
            );
      ) foreign_fields;
    );
    e += "db" in
  e --* "convert statement into an ocaml object";
  e += "let of_stmt stmt =";
  of_stmt e module_name module_name;
  e += "in ";
  e --* "execute the SQL query";
  e += "step_fold db stmt of_stmt"

let get_func_name g =
  let by_name = match g.by with
  |[] -> ""
  |by -> "_by_" ^ (String.concat "_" (List.map (fun f -> f.name) by))
  in
  let want = if g.want_id then g.want else filter_out_id g.want in
  let want_name = if g.want_all then "" else 
    String.concat "" (List.map (fun f -> "_" ^ f.name) want) in
  sprintf "get%s%s" want_name by_name 

let output_get_fn_partial e all module_name g =
  let all = match g.want_all with
  |true ->
    (* all the fields have been requested, so return a full object *)
    all
  |false ->
    (* filter the full all structure to remove fields in the first level to just those we want *)
    List.rev (List.fold_left (fun a (mname, mfields, o) ->
      if mname = module_name then begin
        let fs = List.filter (fun f -> f.name = "id" || (List.mem f g.want)) mfields in
        (mname, fs, o) :: a
      end else (mname, mfields, o) :: a
    ) [] all) in
  let func_name = get_func_name g in
  let func_fields = String.concat " " (List.map (fun f -> sprintf "~%s" f.name) g.by) in
  e += "let %s %s db =" $ func_name $ func_fields;
  e --> (fun e ->
    let first = ref true in
    e += "let q = \"%s\" in" $ String.concat "" (List.map (fun f ->
      let prefix = match !first with
      |true -> first := false; "WHERE "
      |false -> " AND " in
      sprintf "%s%s.%s=?" prefix (to_sql_table_alias f module_name) f.name
    ) g.by);
    let _, col_positions = output_get_fn_sqlbind e all module_name in
    let pos = ref 1 in
    List.iter (fun f ->
      (match f.opt with
      |false -> e += "db_must_ok db (fun () -> let v = %s in Sqlite3.bind stmt %d (%s));" $
        f.name $ !pos $ (to_sql_type_wrapper f.ty);
      |true -> e += "db_must_ok db (fun () -> Sqlite3.bind stmt %d (match %s with |None -> Sqlite3.Data.NONE |Some v -> %s));" $
        !pos $ f.name $ (to_sql_type_wrapper f.ty);
      );
      incr pos;
    ) g.by;
    let ret_fields = String.concat " " (List.map (fun f -> sprintf "~%s" f.name) g.want) in
    let want_fields = if g.want_id then g.want else List.filter (fun f -> f.name <> "id") g.want in
    let ret_fields' = String.concat "," (List.map (fun f -> f.name) want_fields) in
    if not g.want_all then (e += "let t %s db = (%s) in" $ ret_fields $ ret_fields');
    output_get_fn_of_stmt e all module_name col_positions; 
  )

let output_get_fn e all module_name =
  let foreign_fields, native_fields = partition_table_fields all module_name in
  e --* "General get function for any of the columns";
  e += "let get %s db =" $ 
    (String.concat " " (List.map (fun f -> sprintf "?(%s=None)" f.name) native_fields));
  e --> (fun e ->
    e --* "assemble the SQL query string";
    e += "let q = \"\" in";
    e += "let _first = ref true in";
    e += "let f () = match !_first with |true -> _first := false; \" WHERE \" |false -> \" AND \" in";
    List.iter (fun f ->
      e += "let q = match %s with |None -> q |Some b -> q ^ (f()) ^ \"%s.%s=?\" in" $
        f.name $ (to_sql_table_alias f module_name) $ f.name;
    ) native_fields;
    let bind_fields, col_positions = output_get_fn_sqlbind e all module_name in
    e --* "bind the position variables to the statement";
    e += "let bindpos = ref 1 in";
    List.iter (fun f ->
      e += "ignore(match %s with |None -> () |Some v ->" $ f.name;
      e --> (fun e ->
        e += "db_must_ok db (fun () -> Sqlite3.bind stmt !bindpos (%s));" $ (to_sql_type_wrapper f.ty);
        e += "incr bindpos";
      );
      e += ");";
    ) bind_fields;
    output_get_fn_of_stmt e all module_name col_positions;
  )

let output_module e all module_name fields gets =
  let foreign_fields, native_fields = partition_table_fields all module_name in
  print_module e module_name (fun e ->
    print_object e "t" (fun e ->
      List.iter (fun f ->
          e += "%s : %s;" $ f.name $ (to_ocaml_type f);
          e += "set_%s : %s -> unit;" $ f.name $ (to_ocaml_type f);
      ) fields;
      e += "save: int64; delete: unit";
    );
    e += "let init db =";
    e --> (fun e ->
      let fs,fsmany = List.partition (fun x -> match x.ty with |ForeignMany _ -> false |_ -> true) fields in
      let pid = "id integer primary key autoincrement" in
      let sqls = String.concat "," (pid :: List.map (fun f ->
        sprintf "%s %s" (ocaml_var_name f) (to_sql_type f.ty)
      ) (filter_out_id fs)) in
      let create_table table sql =
        e += "let sql = \"create table if not exists %s (%s);\" in" $ table $ sql;
        e += "db_must_ok db (fun () -> Sqlite3.exec db.db sql);" in
      create_table module_name sqls;
      (* create foreign many-many tables now *)
      List.iter (fun fm -> match fm.ty with
        |ForeignMany ftable ->
          let table_name = map_table module_name fm in
          let sqls = sprintf "%s_id integer, %s_id integer, primary key(%s_id, %s_id)" module_name ftable module_name ftable in
          create_table table_name sqls;
        |_ -> assert false
      ) fsmany;
      (* create indices *)
      List.iter (fun f -> 
        if f.idx then (
          let uniq = if f.uniq then "UNIQUE " else "" in
          let idx = sprintf "%s_%s_idx" module_name f.name in
          e += "let sql = \"CREATE %sINDEX IF NOT EXISTS %s ON %s (%s) \" in" $ uniq $ idx $ module_name $ (sql_var_name f);
          e += "db_must_ok db (fun () -> Sqlite3.exec db.db sql);";
        )
      ) fields;
      e += "()";
       
    );
    e.nl(); 
    e --* "object definition";
    let label_names = String.concat " " (List.map (fun f ->
       match f.opt with
       |true -> sprintf "?(%s=None)" f.name
       |false -> sprintf "~%s" f.name) fields) in
    e += "let t %s db : t = object" $ label_names;
    e --> (fun e -> 
      e --* "get functions";
      List.iter (fun f ->
          e += "val mutable _%s = %s" $ f.name $ f.name;
          e += "method %s : %s = _%s" $ f.name $ (to_ocaml_type f) $ f.name;
      ) fields;
      e.nl ();
      e --* "set functions";
      List.iter (fun f ->
          e += "method set_%s v =" $ f.name;
          e --> (fun e ->
            e += "_%s <- v" $ f.name;
          );
      ) fields;
      e.nl ();
      e --* "admin functions";
      e += "method delete =";
      e --> (fun e ->
         e += "match _id with";
         e += "|None -> ()";
         e += "|Some id ->";
         e --> (fun e ->
            e -= "\"%s.delete: id found, deleting\"" $ module_name;
            e += "let sql = \"DELETE FROM %s WHERE id=?\" in" $ module_name;
            e -= "\"%s.delete: \" ^ sql" $ module_name;
            e += "let stmt = Sqlite3.prepare db.db sql in";
            e += "db_must_ok db (fun () -> Sqlite3.bind stmt 1 (Sqlite3.Data.INT id));";
            e += "ignore(step_fold db stmt (fun _ -> ()));";
            e += "_id <- None"
         );
      );
      e.nl ();
      e += "method save = transaction db (fun () ->";
      e --> (fun e ->
        e --* "insert any foreign-one fields into their table and get id";
        List.iter (fun f -> match f.ty,f.opt with
        |Foreign _,false ->
          e += "let _%s = %s#save in" $ (ocaml_var_name f) $ f.name;
        |Foreign _,true ->
          e += "let _%s = match %s with None -> None | Some x -> Some x#save in" $ (ocaml_var_name f) $ f.name;
        |ForeignMany _,_-> () (* this gets inserted later on after we have the current obj id *)
        |_ -> assert false
        ) foreign_fields;
        (* helper function to output the bind statements for a set of fields *)
        let output_bind_fields e fields =
          let pos = ref 1 in
          List.iter (fun f ->
             let var = match f.opt with
             |true -> sprintf "match _%s with |None -> Sqlite3.Data.NULL |Some v -> %s" 
               (ocaml_var_name f) (to_sql_type_wrapper f.ty)
             |false -> sprintf "let v = _%s in %s" 
               (ocaml_var_name f) (to_sql_type_wrapper f.ty) 
             in
             e += "db_must_ok db (fun () -> Sqlite3.bind stmt %d (%s));" $ !pos $ var;
             incr pos;
          ) fields;
          !pos
        in
        e += "let _curobj_id = match _id with";
        e += "|None -> (* insert new record *)";
        e --> (fun e ->
          e -= "\"%s.save: inserting new record\"" $ module_name;
          let singular_fields = filter_out_id (filter_singular_fields fields) in
          let values = String.concat "," (List.map (fun f -> "?") singular_fields) in
          e += "let sql = \"INSERT INTO %s VALUES(NULL,%s)\" in" $ module_name $ values;
          e -= "\"%s.save: \" ^ sql" $ module_name;
          e += "let stmt = Sqlite3.prepare db.db sql in";
          ignore(output_bind_fields e singular_fields);
          e += "ignore(db_busy_retry db (fun () -> Sqlite3.step stmt)); (* XXX add error check *)";
          e += "let __id = Sqlite3.last_insert_rowid db.db in";
          e += "_id <- Some __id;";
          e += "__id"
        );
        e += "|Some id -> (* update *)";
        e --> (fun e ->
          e -= "sprintf \"%s.save: id %%Lu exists, updating\" id" $ module_name;
          let up_fields = filter_out_id ( filter_singular_fields fields) in
          let set_vars = String.concat "," (List.map (fun f ->
            sprintf "%s%s=?" f.name (match f.ty with |Foreign _ -> "_id" |_ -> "")
          ) up_fields) in
          e += "let sql = \"UPDATE %s SET %s WHERE id=?\" in" $ module_name $ set_vars;
          e -= "\"%s.save: \" ^ sql" $ module_name;
          e += "let stmt = Sqlite3.prepare db.db sql in";
          let pos = output_bind_fields e up_fields in
          e += "db_must_ok db (fun () -> Sqlite3.bind stmt %d (Sqlite3.Data.INT id));" $ pos;
          e += "ignore(db_busy_retry db (fun () -> Sqlite3.step stmt)); (* XXX add error check *)";
          e += "id";
        );
        e += "in";
        List.iter (fun f -> match f.ty with
        |Foreign _ -> () (* done earlier *)
        |ForeignMany ftable -> 
          e += "List.iter (fun f ->";
          e --> (fun e ->
            e += "let _refobj_id = f#save in";
            e += "let sql = \"INSERT OR IGNORE INTO %s VALUES(?,?)\" in" $ (map_table module_name f);
            e -= "\"%s.save: foreign insert: \" ^ sql" $ module_name;
            e += "let stmt = Sqlite3.prepare db.db sql in";
            e += "db_must_ok db (fun () -> Sqlite3.bind stmt 1 (Sqlite3.Data.INT _curobj_id));";
            e += "db_must_ok db (fun () -> Sqlite3.bind stmt 2 (Sqlite3.Data.INT _refobj_id));";
            e += "ignore(step_fold db stmt (fun _ -> ()));";
          );
          e += ") _%s;" $ f.name;
          e += "let ids = String.concat \",\" (List.map (fun x -> match x#id with |None -> assert false |Some x -> Int64.to_string x) _%s) in" $ f.name;
          e += "let sql = \"DELETE FROM %s WHERE %s_id=? AND (%s_id NOT IN (\" ^ ids ^ \"))\" in" $ (map_table module_name f) $ module_name $ ftable;
          e -= "\"%s.save: foreign drop gc: \" ^ sql" $ module_name;
          e += "let stmt = Sqlite3.prepare db.db sql in";
          e += "db_must_ok db (fun () -> Sqlite3.bind stmt 1 (Sqlite3.Data.INT _curobj_id));";
          e += "ignore(step_fold db stmt (fun _ -> ()));";
        |_ -> assert false
        ) foreign_fields;
        e += "_curobj_id";
      );
      e.p ")"
    );
    e += "end";
    e.nl ();
    output_get_fn e all module_name;
    e.nl ();
    List.iter (fun g ->
      output_get_fn_partial e all module_name g;
      e.nl ();
    ) gets
  )

let output_init_module e all =
   e += "exception Sql_error of (Sqlite3.Rc.t * string)";
   print_module e "Init" (fun e ->
     e += "type t = state";
     e += "type transaction_mode = [`Exclusive |`Deferred |`Immediate ]";
     e += "let t ?(busyfn=default_busyfn) ?(mode=`Immediate) db_name =";
     e --> (fun e ->
       e += "let db = {db=Sqlite3.db_open db_name; in_transaction=0; mode=mode; busyfn=busyfn } in";
       List.iter (fun (m,_,_) ->
         e += "%s.init db;" $ (String.capitalize m);
       ) all;
       e += "db";
     );
     e.nl ();
     e += "let db handle = handle.db"
   )

let output_module_interface e all module_name fields gets =
  let raises = "Sql_error if a database error is encountered" in
  let foreign_fields, native_fields = partition_table_fields all module_name in
  print_module_sig e module_name (fun e ->
    print_object e "t" (fun e ->
      List.iter (fun f ->
          e += "%s : %s;" $ f.name $ (to_ocaml_type f);
          e += "set_%s : %s -> unit;" $ f.name $ (to_ocaml_type f);
      ) fields;
      e += "save: int64; delete: unit";
    );
    print_ocamldoc e "An object which can be stored in the database with the [save] method call, or removed by calling [delete].  Fields can be accessed via the approriate named method and set via the [set_] methods.  Changes are not committed to the database until [save] is invoked.";
    e.nl ();

    e += "val t :";
    e --> (fun e ->
      List.iter (fun f ->
        e += "%s%s:%s ->" $ (if f.opt then "?" else "") $ f.name $ (to_ocaml_type f)
      ) fields;
      e += "Init.t -> t";
    );
    print_ocamldoc e ~raises "Can be used to construct a new object.  If [id] is not specified, it will be automatically assigned the first time [save] is called on the object.  The object is not committed to the database until [save] is invoked.  The [save] method will also return the [id] assigned to the object.";
    e.nl ();
    e += "val get :";
    e --> (fun e ->
      List.iter (fun f ->
        e += "?%s:%s ->" $ f.name $ (to_ocaml_type ~always_opt:true f)
      ) native_fields;
      e += "Init.t -> t list";
    );
   print_ocamldoc e ~raises "Used to retrieve objects from the database.  If an argument is specified, it is included in the search criteria (all fields are ANDed together).";
   e.nl ();
   List.iter (fun g ->
     e += "val %s :" $ (get_func_name g);
     e --> (fun e ->
        List.iter (fun f ->
          e += "%s:%s -> " $ f.name $ (to_ocaml_type f)
        ) g.by;
        e += "Init.t -> ";
        match g.want_all with
        |true ->  e += "t list"
        |false -> begin 
          let want = if g.want_id then g.want else filter_out_id g.want in
          let otype = List.map (fun f ->
            to_ocaml_type f
          ) want in
          if List.length otype > 1 then
            e += "(%s) list" $ (String.concat " * " otype)
          else
            e += "%s list" $ (List.hd otype)
        end
     );
     e.nl ()
   ) gets;
  )

let output_init_module_interface e =
  print_module_sig e "Init" (fun e ->
     e += "type t";
     e += "type transaction_mode = [`Exclusive |`Deferred |`Immediate ]";
     print_ocamldoc e "Database handle which can be used to create and retrieve objects";
     e += "val t :";
     e --> (fun e ->
       e += "?busyfn:(Sqlite3.db -> unit) -> ?mode:transaction_mode ->";
       e += "string -> t";
     );
     print_ocamldoc e ~args:"t db_name" ~raises:"Sql_error if a database error is encountered" "open a Sqlite3 database with filename [db_name] and create any tables if they are missing. @return a database handle which can be used to create and retrieve objects in the database.";
     e += "val db: t -> Sqlite3.db";
     print_ocamldoc e ~args:"db handle" "@return the underlying Sqlite3 database handle for the connection, for advanced queries.";
  )

let output_sqlaccess_module e =
  e += "exception Sql_error of (Sqlite3.Rc.t * string)";
  print_module e "Sql_access" (fun e ->
     Sql_orm_header.print_header e;
  );
  e.nl ()

let generate ?(debug=false) (all:collection) output_basename =
  let mlout = open_out (output_basename ^ ".ml") in
  let e = init_printer ~msg:(Some "(* autogenerated by sql_orm *)") ~debug mlout in
  if e.dbg then e += "open Printf";
  output_sqlaccess_module e;
  e += "open Sql_access";
  List.iter (fun (name, fields, gets) -> output_module e all name fields gets) all;
  output_init_module e all;
  close_out mlout;
  let mliout = open_out (output_basename ^ ".mli") in
  let e = init_printer ~msg:(Some "(* autogenerated by sql_orm *)") ~debug mliout in
  print_ocamldoc e "Use the [[Init]] module to open a new database handle.  Each object type has its own module with functions to create, modify, save and destroy objects of that type into the SQLite database";
  e += "exception Sql_error of (Sqlite3.Rc.t * string)";
  output_init_module_interface e;
  List.iter (fun (name, fields, gets) -> output_module_interface e all name fields gets) all;
  close_out mliout
   

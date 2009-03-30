(* ORM layer for Sqlite *)
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
      `Optional
    ]

    type s = {
        name: string;
        ty: t;
        flags: options list;
    }

    let text ?(flags=[]) n = {name=n; ty=Text; flags=flags}
    let blob ?(flags=[]) n = {name=n; ty=Blob; flags=flags}
    let date ?(flags=[]) n = {name=n; ty=Date; flags=flags}
    let foreign ?(flags=[]) n f = {name=n; ty=(Foreign f); flags=flags}
    let foreign_many ?(flags=[]) n f = {name=n; ty=(ForeignMany f); flags=flags}
    let id () = {name="id"; ty=Int; flags=[]}

    type collection = (string * s list) list

    (* add in an id field *)
    let make (f:collection) = 
       List.map (fun (n,v) ->
         (n, id () :: v)
       ) f

    let to_ocaml_type f =
      match f.name,f.ty with
      |"id",_ -> "int64 option"
      |_,Text -> "string"
      |_,Blob -> "string" (* watch out for 16MB limit *)
      |_,Int -> "int64"
      |_,Foreign x -> sprintf "%s.t" (String.capitalize x)
      |_,ForeignMany x -> sprintf "%s.t list" (String.capitalize x)
      |_,Date -> "float"

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

    let convert_from_sql = function
    |Text -> "Sqlite3.Data.to_string x"
    |Blob -> "Sqlite3.Data.to_string x"
    |Int -> "match x with |Sqlite3.Data.INT i -> i |x -> Int64.of_string (Sqlite3.Data.to_string x)"
    |Foreign x -> "false (* XX *)"
    |ForeignMany _ -> assert false
    |Date -> "match x with |Sqlite3.Data.INT i -> Int64.to_float i|_ -> float_of_string (Sqlite3.Data.to_string x)"

    let map_table t f = match f.ty with
    |ForeignMany ft -> sprintf "map_%s_%s_%s" f.name t ft
    |_ -> assert false
    
    let ocaml_var_name f = f.name ^ match f.ty with
    |Foreign _ -> "_id"
    |_ -> ""

    let get_table_fields (c:collection) table = 
       List.assoc table c

    let partition_table_fields c table =
       List.partition (fun f -> match f.ty with ForeignMany _|Foreign _ -> true |_ -> false)
          (get_table_fields c table)

    let filter_out_id fields =
       List.filter (fun f -> f.name <> "id") fields

    let filter_singular_fields f =
       List.filter (fun f -> match f.ty with ForeignMany _ -> false |_ -> true) f

    let rec foreign_table_names all table =
      let fs = get_table_fields all table in
      List.fold_left (fun a b ->
        match b.ty with 
        |Foreign x ->
           (foreign_table_names all x) @ (x::a)
        |_ -> a
      ) [] fs

end

open Printer_utils.Printer

let output_module e all (module_name, fields) =
  let foreign_fields, native_fields = Schema.partition_table_fields all module_name in
  print_module e module_name (fun e ->
    print_object e "t" (fun e ->
      List.iter (fun f ->
          pfn e "%s : %s;" f.Schema.name (Schema.to_ocaml_type f);
          pfn e "set_%s : %s -> unit;" f.Schema.name (Schema.to_ocaml_type f);
      ) fields;
      pfn e "save: int64; delete: unit";
    );
    pfn e "let init db =";
    e --> (fun e ->
      let fs,fsmany = List.partition (fun x -> match x.Schema.ty with |Schema.ForeignMany _ -> false |_ -> true) fields in
      let pid = "id integer primary key autoincrement" in
      let sqls = String.concat "," (pid :: List.map (fun f ->
        sprintf "%s %s" (Schema.ocaml_var_name f) (Schema.to_sql_type f.Schema.ty)
      ) (Schema.filter_out_id fs)) in
      let create_table table sql =
        pfn e "let sql = \"create table if not exists %s (%s);\" in" table sql;
        pfn e "Sql_access.db_must_ok (fun () -> Sqlite3.exec db sql);" in
      create_table module_name sqls;
      (* create foreign many-many tables now *)
      List.iter (fun fm -> match fm.Schema.ty with
        |Schema.ForeignMany ftable ->
          let table_name = Schema.map_table module_name fm in
          let sqls = sprintf "%s_id integer, %s_id integer, primary key(%s_id, %s_id)" module_name ftable module_name ftable in
          create_table table_name sqls;
        |_ -> assert false
      ) fsmany;
      pfn e "()";
       
    );
    e.nl(); 
    e --* "object definition";
    let label_names = String.concat " " (List.map (fun f ->
       match f.Schema.name with
       |"id" -> "?(id=None)"
       |name -> sprintf "~%s" name) fields) in
    pfn e "let t %s (db:Sqlite3.db) : t = object" label_names;
    e --> (fun e -> 
      e --* "get functions";
      List.iter (fun f ->
          pfn e "val mutable _%s = %s" f.Schema.name f.Schema.name;
          pfn e "method %s : %s = _%s" f.Schema.name (Schema.to_ocaml_type f) f.Schema.name;
      ) fields;
      e.nl ();
      e --* "set functions";
      List.iter (fun f ->
          pfn e "method set_%s v =" f.Schema.name;
          e --> (fun e -> 
            pfn e "_%s <- v" f.Schema.name;
          );
      ) fields;
      e.nl ();
      e --* "admin functions";
      pfn e "method delete =";
      e --> (fun e ->
         pfn e "match _id with";
         pfn e "|None -> ()";
         pfn e "|Some id ->";
         e --> (fun e ->
            dbg e "\"%s.delete: id found, deleting\"" module_name;
            pfn e "let sql = \"DELETE FROM %s WHERE id=?\" in" module_name;
            dbg e "\"%s.delete: \" ^ sql" module_name;
            pfn e "let stmt = Sqlite3.prepare db sql in";
            pfn e "Sql_access.db_must_ok (fun () -> Sqlite3.bind stmt 1 (Sqlite3.Data.INT id));";
            pfn e "ignore(Sql_access.step_fold stmt (fun _ -> ()));";
            pfn e "_id <- None"
         );
      );
      e.nl ();
      pfn e "method save =";
      e --> (fun e ->
        e --* "XXX wrap this in transaction";
        e --* "insert any foreign-one fields into their table and get id";
        List.iter (fun f -> match f.Schema.ty with
        |Schema.Foreign _ ->
          pfn e "let _%s = %s#save in" (Schema.ocaml_var_name f) f.Schema.name;
        |Schema.ForeignMany ftable -> () (* this gets inserted later on after we have the current obj id *)
        |_ -> assert false
        ) foreign_fields;
        pfn e "let _curobj_id = match _id with";
        pfn e "|None -> (* insert new record *)";
        e --> (fun e ->
          dbg e "\"%s.save: inserting new record\"" module_name;
          let singular_fields = Schema.filter_out_id (Schema.filter_singular_fields fields) in
          let values = String.concat "," (List.map (fun f -> "?") singular_fields) in
          pfn e "let sql = \"INSERT INTO %s VALUES(NULL,%s)\" in" module_name values;
          dbg e "\"%s.save: \" ^ sql" module_name;
          pfn e "let stmt = Sqlite3.prepare db sql in";
          let pos = ref 1 in
          List.iter (fun f ->
             let var = sprintf "let v = _%s in %s" (Schema.ocaml_var_name f) (Schema.to_sql_type_wrapper f.Schema.ty) in
             pfn e "Sql_access.db_must_ok (fun () -> Sqlite3.bind stmt %d (%s));" !pos var;
             incr pos;
          ) singular_fields;
          pfn e "ignore(Sql_access.db_busy_retry (fun () -> Sqlite3.step stmt)); (* XXX add error check *)";
          pfn e "let __id = Sqlite3.last_insert_rowid db in";
          pfn e "_id <- Some __id;";
          pfn e "__id"
        );
        pfn e "|Some id -> (* update *)";
        e --> (fun e ->
          dbg e "sprintf \"%s.save: id %%Lu exists, updating\" id" module_name;
          let up_fields = Schema.filter_out_id ( Schema.filter_singular_fields fields) in
          let set_vars = String.concat "," (List.map (fun f ->
            sprintf "%s%s=?" f.Schema.name (match f.Schema.ty with |Schema.Foreign _ -> "_id" |_ -> "")
          ) up_fields) in
          pfn e "let sql = \"UPDATE %s SET %s WHERE id=?\" in" module_name set_vars;
          dbg e "\"%s.save: \" ^ sql" module_name;
          pfn e "let stmt = Sqlite3.prepare db sql in";
          let pos = ref 1 in
          List.iter (fun f ->
             let var = sprintf "let v = _%s in %s" (Schema.ocaml_var_name f) (Schema.to_sql_type_wrapper f.Schema.ty) in
             pfn e "Sql_access.db_must_ok (fun () -> Sqlite3.bind stmt %d (%s));" !pos var;
             incr pos;
          ) up_fields;
          pfn e "Sql_access.db_must_ok (fun () -> Sqlite3.bind stmt %d (Sqlite3.Data.INT id));" !pos;
          pfn e "ignore(Sql_access.db_busy_retry (fun () -> Sqlite3.step stmt)); (* XXX add error check *)";
          pfn e "id";
        );
        pfn e "in";
        List.iter (fun f -> match f.Schema.ty with
        |Schema.Foreign _ -> () (* done earlier *)
        |Schema.ForeignMany ftable -> 
          pfn e "List.iter (fun f ->";
          e --> (fun e ->
            pfn e "let _refobj_id = f#save in";
            pfn e "let sql = \"insert into %s values(?,?)\" in" (Schema.map_table module_name f);
            dbg e "\"%s.save: foreign insert: \" ^ sql" module_name;
            pfn e "let stmt = Sqlite3.prepare db sql in";
            pfn e "Sql_access.db_must_ok (fun () -> Sqlite3.bind stmt 1 (Sqlite3.Data.INT _curobj_id));";
            pfn e "Sql_access.db_must_ok (fun () -> Sqlite3.bind stmt 2 (Sqlite3.Data.INT _refobj_id));";
            pfn e "ignore(Sql_access.step_fold stmt (fun _ -> ()));";
          );
          pfn e ") %s;" f.Schema.name;
        |_ -> assert false
        ) foreign_fields;
        pfn e "_curobj_id";
      );
    );

    pfn e "end";
    e.nl ();
    e --* "General get function for any of the columns";
    pfn e "let get %s (db:Sqlite3.db) =" (String.concat " " (List.map (fun f -> sprintf "?(%s=None)" f.Schema.name) native_fields));
    e --> (fun e ->
      e --* "assemble the SQL query string";
      pfn e "let q = \"\" in";
      let first = ref true in
      List.iter (fun f ->
        pfn e "let q = match %s with |None -> q |Some b -> q ^ \"%s%s=?\" in" f.Schema.name (match !first with true -> "" |false ->" &&") f.Schema.name;
        first := false;
      ) native_fields;
      (* get all the field names to select, combination of the foreign keys as well *)
      let col_positions = Hashtbl.create 1 in
      let pos = ref 0 in
      let sql_field_names = String.concat ", " ( 
        List.concat (
          List.map (fun table -> 
             List.map (fun f ->
               Hashtbl.add col_positions (table,f.Schema.name) !pos;
               incr pos;
               sprintf "%s.%s" table f.Schema.name;
             ) (Schema.filter_singular_fields (Schema.get_table_fields all table))
          ) (module_name :: (Schema.foreign_table_names all module_name))
        )
      ) in
      let joins = String.concat "" (List.map (fun f ->
           match f.Schema.ty with 
           |Schema.Foreign ftable -> sprintf "LEFT JOIN %s ON (%s.id = %s.%s_id) " ftable ftable module_name ftable
           |Schema.ForeignMany ftable -> ""
           |_ -> assert false
         ) foreign_fields) in
      pfn e "let q=\"SELECT %s FROM %s %sWHERE \" ^ q in" sql_field_names module_name joins;
      dbg e "\"%s.get: \" ^ q" module_name;
      pfn e "let stmt=Sqlite3.prepare db q in";
      e --* "bind the position variables to the statement";
      pfn e "let bindpos = ref 1 in";
      List.iter (fun f ->
         pfn e "ignore(match %s with |None -> () |Some v ->" f.Schema.name;
         e --> (fun e ->
           pfn e "Sql_access.db_must_ok (fun () -> Sqlite3.bind stmt !bindpos (%s));" (Schema.to_sql_type_wrapper f.Schema.ty);
           pfn e "incr bindpos";
         );
         pfn e ");";
      ) native_fields;

      e --* "convert statement into an ocaml object";
      pfn e "let of_stmt stmt =";
      let rec of_stmt e table =
        let foreign_fields, fields = Schema.partition_table_fields all table in
        (if table = module_name then pfn e "t" else pfn e "%s.t" (String.capitalize table));
        e --> (fun e ->
          e --* "native fields";
          List.iter (fun f ->
             pfn e "%s" (match f.Schema.name with "id" -> "~id:(Some " |x -> sprintf "~%s:(" x);
             e --> (fun e ->
               pfn e "(let x = Sqlite3.column stmt %d in" (Hashtbl.find col_positions (table, f.Schema.name));
               pfn e "%s)" (Schema.convert_from_sql f.Schema.ty);
             );
             pfn e ")";
          ) fields;
          List.iter (fun f ->
            pfn e "~%s:(" f.Schema.name;
            match f.Schema.ty with
            |Schema.Foreign ftable ->
              e --* "foreign mapping field";
              of_stmt e ftable;
              pfn e ")"
            |Schema.ForeignMany ftable ->
              e --* "foreign many-many mapping field";
              pfn e "let stmt' = Sqlite3.prepare db \"select %s_id from %s where %s_id=?\" in" ftable (Schema.map_table table f) table;
              pfn e "let %s__id = Sqlite3.column stmt %d in" table (Hashtbl.find col_positions (table, "id"));
              pfn e "Sql_access.db_must_ok (fun () -> Sqlite3.bind stmt' 1 %s__id);" table; 
              pfn e "List.flatten (Sql_access.step_fold stmt' (fun s ->";
              e --> (fun e ->
                pfn e "let i = match Sqlite3.column s 0 with |Sqlite3.Data.INT i -> i |_ -> assert false in";
                pfn e "%s.get ~id:(Some i) db)" (String.capitalize ftable);
              );
              pfn e "))"
            |_ -> assert false
          ) foreign_fields;
        );
        pfn e "db"
      in
      of_stmt e module_name;
      pfn e "in ";
      e --* "execute the SQL query";
      pfn e "Sql_access.step_fold stmt of_stmt"
  
    );
  )

let output_init_module e all =
   print_module e "Init" (fun e ->
     pfn e "let t db_name =";
     e --> (fun e ->
       pfn e "let db = Sqlite3.db_open db_name in";
       List.iter (fun (m,_) ->
         pfn e "%s.init db;" (String.capitalize m);
       ) all;
       pfn e "db";
     );
   )

let generate ?(debug=false) all =
  let e = init_printer ~msg:(Some "(* autogenerated by sql_orm *)") ~debug stdout in
  pfn e "exception Sqlite_error of Sqlite3.Rc.t";
  if e.dbg then pfn e "open Printf";
  List.iter (output_module e all) all;
  output_init_module e all;
  ()
   

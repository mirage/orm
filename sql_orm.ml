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
 
    type s = {
        name: string;
        ty: t;
    }

    let text n = {name=n; ty=Text}
    let blob n = {name=n; ty=Blob}
    let date n = {name=n; ty=Date}
    let foreign n f = {name=n; ty=(Foreign f)}
    let foreign_many n f = {name=n; ty=(ForeignMany f)}
    let id () = {name="id"; ty=Int}

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

let all = Schema.make [
  "contacts" , [
    Schema.text "file_name";
    Schema.text "uid";
    Schema.text "abrecord";
    Schema.text "first_name";
    Schema.text "last_name";
    Schema.date "mtime";
  ];

  "people" , [
    Schema.text "service_name";
    Schema.text "service_id";
    Schema.foreign "contact" "contacts"
  ];

  "mtypes" , [
    Schema.text "mtype";
    Schema.text "label";
    Schema.text "icon";
    Schema.text "implements";
  ];

  "attachments" , [
    Schema.text "file_name";
    Schema.text "mime_type";
  ];

  "lifedb" , [
    Schema.text "file_name";
    Schema.date "ctime";
    Schema.foreign "mtype" "mtypes";
    Schema.foreign "people_from" "people";
    Schema.foreign_many "atts" "attachments";
    Schema.foreign_many "people_to" "people";
    Schema.text "summary";
  ];

]

open Printer_utils.Printer

let output_module e (module_name, fields) =
  let foreign_fields, native_fields = Schema.partition_table_fields all module_name in
  print_module e module_name (fun e ->
    print_object e "t" (fun e ->
      List.iter (fun f ->
          e.p (sprintf "%s : %s;" f.Schema.name (Schema.to_ocaml_type f));
          e.p (sprintf "set_%s : %s -> unit;" f.Schema.name (Schema.to_ocaml_type f));
      ) fields;
      e.p "save: int64";
    );
    e.p "let init db =";
    indent_fn e (fun e ->
      let fs,fsmany = List.partition (fun x -> match x.Schema.ty with |Schema.ForeignMany _ -> false |_ -> true) fields in
      let pid = "id integer primary key autoincrement" in
      let sqls = String.concat "," (pid :: List.map (fun f ->
        sprintf "%s %s" (Schema.ocaml_var_name f) (Schema.to_sql_type f.Schema.ty)
      ) (Schema.filter_out_id fs)) in
      let create_table table sql =
        e.p (sprintf "let sql = \"create table if not exists %s (%s);\" in" table sql);
        e.p "Sql_access.db_must_ok (fun () -> Sqlite3.exec db sql);" in
      create_table module_name sqls;
      (* create foreign many-many tables now *)
      List.iter (fun fm -> match fm.Schema.ty with
        |Schema.ForeignMany ftable ->
          let table_name = sprintf "map_%s_%s" module_name fm.Schema.name in
          let sqls = sprintf "%s_id integer, %s_id integer, primary key(%s_id, %s_id)" module_name ftable module_name ftable in
          create_table table_name sqls;
        |_ -> assert false
      ) fsmany;
      e.p "()";
       
    );
    e.nl(); 
    print_comment e "object definition";
    let label_names = String.concat " " (List.map (fun f ->
       match f.Schema.name with
       |"id" -> "?(id=None)"
       |name -> sprintf "~%s" name) fields) in
    e.p (sprintf "let t %s (db:Sqlite3.db)  = object" label_names);
    indent_fn e (fun e -> 
      print_comment e "get functions";
      List.iter (fun f ->
          e.p (sprintf "val mutable _%s = %s" f.Schema.name f.Schema.name);
          e.p (sprintf "method %s : %s = _%s" f.Schema.name (Schema.to_ocaml_type f) f.Schema.name);
      ) fields;
      e.nl ();
      print_comment e "set functions";
      List.iter (fun f ->
          e.p (sprintf "method set_%s v =" f.Schema.name);
          indent_fn e (fun e -> 
            e.p (sprintf "_%s <- v" f.Schema.name);
          );
      ) fields;
      e.nl ();
      print_comment e "admin functions";
      e.p "method save =";
      indent_fn e (fun e ->
        print_comment e "(* XXX wrap this in transaction *)";
        print_comment e "insert any foreign-one fields into their table and get id";
        List.iter (fun f -> match f.Schema.ty with
        |Schema.Foreign _ ->
          e.p (sprintf "let _%s = %s#save in" (Schema.ocaml_var_name f) f.Schema.name);
        |Schema.ForeignMany ftable ->
          e.p "List.iter (fun f ->";
          indent_fn e (fun e ->
            e.p "()";
          );
          e.p (sprintf ") %s;" f.Schema.name);
        |_ -> assert false
        ) foreign_fields;
        e.p "match id with";
        e.p "|None -> (* insert new record *)";
        indent_fn e (fun e ->
          let singular_fields = Schema.filter_out_id (Schema.filter_singular_fields fields) in
          let values = String.concat "," (List.map (fun f -> "?") singular_fields) in
          e.p (sprintf "let sql = \"insert into %s values(NULL,%s)\" in" module_name values);
          e.p "let stmt = Sqlite3.prepare db sql in";
          let pos = ref 1 in
          List.iter (fun f ->
             let var = sprintf "let v = _%s in %s" (Schema.ocaml_var_name f) (Schema.to_sql_type_wrapper f.Schema.ty) in
             e.p (sprintf "Sql_access.db_must_ok (fun () -> Sqlite3.bind stmt %d (%s));" !pos var);
             incr pos;
          ) singular_fields;
          e.p "ignore(Sql_access.db_busy_retry (fun () -> Sqlite3.step stmt)); (* XXX add error check *)";
          e.p "let __id = Sqlite3.last_insert_rowid db in";
          e.p "_id <- Some __id;";
          e.p "__id"
        );
        e.p "|Some id -> (* update *)";
        indent_fn e (fun e ->
          let up_fields = Schema.filter_out_id ( Schema.filter_singular_fields fields) in
          let set_vars = String.concat "," (List.map (fun f ->
            sprintf "%s%s=?" f.Schema.name (match f.Schema.ty with |Schema.Foreign _ -> "_id" |_ -> "")
          ) up_fields) in
          e.p (sprintf "let sql = \"update %s set %s where id=?\" in" module_name set_vars);
          e.p "let stmt = Sqlite3.prepare db sql in";
          let pos = ref 1 in
          List.iter (fun f ->
             let var = sprintf "let v = _%s in %s" (Schema.ocaml_var_name f) (Schema.to_sql_type_wrapper f.Schema.ty) in
             e.p (sprintf "Sql_access.db_must_ok (fun () -> Sqlite3.bind stmt %d (%s));" !pos var);
             incr pos;
          ) up_fields;
          e.p (sprintf "Sql_access.db_must_ok (fun () -> Sqlite3.bind stmt %d (Sqlite3.Data.INT id));" !pos);
          e.p "ignore(Sql_access.db_busy_retry (fun () -> Sqlite3.step stmt)); (* XXX add error check *)";
          e.p "id";
        );
      );
    );

    e.p "end";
    e.nl ();
    print_comment e "General get function for any of the columns";
    e.p (sprintf "let get %s (db:Sqlite3.db) =" (String.concat " " (List.map (fun f -> sprintf "?(%s=None)" f.Schema.name) native_fields)));
    indent_fn e (fun e ->
      print_comment e "assemble the SQL query string";
      let wheres = List.map (fun f -> sprintf "(match %s with |None -> \"\" |Some _ -> \"%s=?\");" f.Schema.name f.Schema.name) native_fields in
      e.p "let wheres = String.concat \" && \"  [";
      list_iter_indent e (fun e -> e.p) wheres;
      e.p "] in";
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
      let joins = String.concat " " (List.map (fun f ->
           match f.Schema.ty with 
           |Schema.Foreign ftable -> sprintf "LEFT JOIN %s ON (%s.id = %s.%s_id)" ftable ftable module_name ftable
           |Schema.ForeignMany ftable -> ""
           |_ -> assert false
         ) foreign_fields) in
      e.p (sprintf "let q=\"SELECT %s FROM %s %s WHERE \" ^ wheres in" sql_field_names module_name joins);
      e.p "let stmt=Sqlite3.prepare db q in";
      print_comment e "bind the position variables to the statement";
      e.p "let bindpos = ref 1 in";
      List.iter (fun f ->
         e.p (sprintf "ignore(match %s with |None -> () |Some v ->" f.Schema.name);
         indent_fn e (fun e ->
           e.p (sprintf "Sql_access.db_must_ok (fun () -> Sqlite3.bind stmt !bindpos (%s));" (Schema.to_sql_type_wrapper f.Schema.ty));
           e.p "incr bindpos";
         );
         e.p ");";
      ) native_fields;

      print_comment e "convert statement into an ocaml object";
      e.p "let of_stmt stmt =";
      let rec of_stmt e table =
        let foreign_fields, fields = Schema.partition_table_fields all table in
        (if table = module_name then e.p "t" else e.p (sprintf "%s.t" (String.capitalize table)));
        indent_fn e (fun e ->
          print_comment e "native fields";
          List.iter (fun f ->
             e.p (match f.Schema.name with "id" -> "~id:(Some " |x -> sprintf "~%s:(" x);
             indent_fn e (fun e ->
               e.p (sprintf "(let x = Sqlite3.column stmt %d in" (Hashtbl.find col_positions (table, f.Schema.name)));
               e.p (sprintf "%s)" (Schema.convert_from_sql f.Schema.ty));
             );
             e.p ")";
          ) fields;
          List.iter (fun f ->
            e.p (sprintf "~%s:(" f.Schema.name);
            match f.Schema.ty with
            |Schema.Foreign ftable ->
              print_comment e "foreign mapping field";
              of_stmt e ftable;
              e.p ")"
            |Schema.ForeignMany ftable ->
              print_comment e "foreign many-many mapping field";
              e.p (sprintf "let stmt' = Sqlite3.prepare db \"select %s_id from map_%s_%s where %s_id=?\" in" ftable table ftable table);
              e.p (sprintf "let %s__id = Sqlite3.column stmt %d in" table (Hashtbl.find col_positions (table, "id")));
              e.p (sprintf "Sql_access.db_must_ok (fun () -> Sqlite3.bind stmt' 1 %s__id);" table); 
              e.p (sprintf "List.flatten (Sql_access.step_fold stmt' (fun s ->");
              indent_fn e (fun e ->
                e.p "let i = match Sqlite3.column s 0 with |Sqlite3.Data.INT i -> i |_ -> assert false in";
                e.p (sprintf "%s.get ~id:(Some i) db)" (String.capitalize ftable));
              );
              e.p "))"
            |_ -> assert false
          ) foreign_fields;
        );
        e.p "db"
      in
      of_stmt e module_name;
      e.p "in ";
      print_comment e "execute the SQL query";
      e.p "Sql_access.step_fold stmt of_stmt"
  
    );
  )

let _ =
  let e = init_printer ~msg:(Some "(* autogenerated by sql_orm *)") stdout in
  e.p "exception Sqlite_error of Sqlite3.Rc.t";
  List.iter (output_module e) all;
  ()
   

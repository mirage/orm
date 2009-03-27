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
      |_,ForeignMany x -> sprintf "%s.t" (String.capitalize x)
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
    |Foreign _ -> assert false
    |ForeignMany _ -> assert false
    |Date -> "Sqlite3.Data.INT (Int64.of_float v)"

    let convert_from_sql = function
    |Text -> "Sqlite3.Data.to_string x"
    |Blob -> "Sqlite3.Data.to_string x"
    |Int -> "match x with |Sqlite3.Data.INT i -> i |x -> Int64.of_string (Sqlite3.Data.to_string x)"
    |Foreign x -> "false (* XX *)"
    |ForeignMany _ -> assert false
    |Date -> "match x with |Sqlite3.Data.INT i -> Int64.to_float i|_ -> float_of_string (Sqlite3.Data.to_string x)"

    let sql_decls (fs: s list) =
     let fs = List.filter (fun x -> match x.ty with |ForeignMany _ -> false |_ -> true) fs in
     let pid = "id integer primary key autoincrement" in
      let sqls = List.map (fun f ->
        sprintf "%s %s" f.name (to_sql_type f.ty) 
      ) fs in
      String.concat ", " (pid::sqls)

    let get_table_fields (c:collection) table = 
      List.assoc table c

    let partition_table_fields c table =
       List.partition (fun f -> match f.ty with ForeignMany _|Foreign _ -> true |_ -> false)
          (get_table_fields c table)

    let rec foreign_table_names all table =
      let fs = get_table_fields all table in
      let res = List.fold_left (fun a b ->
        match b.ty with 
        |Foreign x |ForeignMany x ->
           (foreign_table_names all x) @ (x::a)
        |_ -> a
      ) [] fs in
      prerr_endline (String.concat "," res); res
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
    Schema.foreign "contact_id" "contacts"
  ];

  "mtypes" , [
    Schema.text "mtype";
    Schema.text "label";
    Schema.text "icon";
    Schema.text "implements";
  ];

  "lifedb" , [
    Schema.text "file_name";
    Schema.date "ctime";
    Schema.foreign "mtype" "mtypes";
    Schema.foreign "people_from" "people";
    Schema.foreign_many "people_to" "people";
    Schema.text "summary";
  ];

  "attachments" , [
    Schema.foreign "lifedb_id" "lifedb";
    Schema.text "file_name";
  ];

]

open Printer_utils.Printer

let output_module e (module_name, fields) =
  let foreign_fields, native_fields = Schema.partition_table_fields all module_name in
  print_module e module_name (fun e ->
    print_object e "t" (fun e ->
      List.iter (fun f ->
          e.p (sprintf "%s : %s;" f.Schema.name (Schema.to_ocaml_type f));
      ) fields;
    );
    e.p "let init db =";
    indent_fn e (fun e ->
      e.p (sprintf "let sql = \"create table if not exists %s (%s);\" in" module_name (Schema.sql_decls native_fields));
      e.p "Sql_access.db_must_ok (fun () -> Sqlite3.exec db sql)";
    );
    e.nl(); 
    print_comment e "object definition";
    let label_names = String.concat " " (List.map (fun f ->
       match f.Schema.name with
       |"id" -> "?(id=None)"
       |name -> sprintf "~%s" name) fields) in
    e.p (sprintf "let t %s (db:Sqlite3.db)  = object" label_names);
    indent_fn e (fun e -> 
      List.iter (fun f ->
          e.p (sprintf "val mutable _%s = %s" f.Schema.name f.Schema.name);
          e.p (sprintf "method %s : %s = _%s" f.Schema.name (Schema.to_ocaml_type f) f.Schema.name);
      ) fields;
    );
    e.p "end";
    e.nl ();
    print_comment e "General get function for any of the columns";
    e.p (sprintf "let get %s (db:Sqlite3.db) (iterfn:t->unit) =" (String.concat " " (List.map (fun f -> sprintf "?(%s=None)" f.Schema.name) native_fields)));
    indent_fn e (fun e ->
      print_comment e "assemble the SQL query string";
      let wheres = List.map (fun f -> sprintf "(match %s with |None -> \"\" |Some _ -> \"%s=?\");" f.Schema.name f.Schema.name) native_fields in
      e.p "let wheres = String.concat \" && \"  [";
      list_iter_indent e (fun e -> e.p) wheres;
      e.p "] in";
      (* get all the field names to select, combination of the foreign keys as well *)
      let col_positions = Hashtbl.create 1 in
      let pos = ref 0 in
      let sql_field_names = String.concat "," ( 
        List.concat (
          List.map (fun table -> 
             List.map (fun f ->
               Hashtbl.add col_positions (table,f.Schema.name) !pos;
               incr pos;
               sprintf "%s.%s" table f.Schema.name;
             ) (Schema.get_table_fields all table)
          ) (module_name :: (Schema.foreign_table_names all module_name))
        )
      ) in
      let joins = String.concat " " (List.map (fun f ->
           match f.Schema.ty with 
           |Schema.Foreign ftable
           |Schema.ForeignMany ftable -> sprintf "LEFT JOIN %s ON (%s.id = %s.id)" ftable ftable module_name
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
          print_comment e "foreign mapping fields";
          List.iter (fun f ->
            e.p (sprintf "~%s:(" f.Schema.name);
            match f.Schema.ty with
            |Schema.ForeignMany ftable |Schema.Foreign ftable ->
              of_stmt e ftable;
              e.p ")"
            |_ -> assert false
          ) foreign_fields;
        );
        e.p "db"
      in
      of_stmt e module_name;
      e.p "in ";
      print_comment e "execute the SQL query";
      e.p "let iterfn = (fun () ->";
      indent_fn e (fun e ->
        e.p "match Sqlite3.step stmt with";
        e.p "|Sqlite3.Rc.ROW -> iterfn (of_stmt stmt); true";
        e.p "|Sqlite3.Rc.DONE -> false";
        e.p "|x -> raise (Sqlite_error x)";
      );
      e.p ") in while iterfn () do () done; ()"
  
    );
  )

let _ =
  let e = init_printer ~msg:(Some "(* autogenerated by sql_orm *)") stdout in
  e.p "exception Sqlite_error of Sqlite3.Rc.t";
  List.iter (output_module e) all;
  ()
   

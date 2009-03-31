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
      `Optional
    ]

    type s = {
        name: string;
        ty: t;
        opt: bool;
    }

    let base ~flags ty n = {name=n; ty=ty; opt=(List.mem `Optional flags)}
    let text ?(flags=[]) = base ~flags Text
    let blob ?(flags=[]) = base ~flags Blob
    let date ?(flags=[]) = base ~flags Date
    let integer ?(flags=[]) = base ~flags Int
    let foreign ?(flags=[]) f = base ~flags (Foreign f)
    let foreign_many ?(flags=[]) f = base ~flags (ForeignMany f)
    let id () = integer ~flags:[`Optional] "id"

    type collection = (string * s list) list

    (* add in an id field *)
    let make (f:collection) = 
       List.map (fun (n,v) ->
         (n, id () :: v)
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

    let sql_var_name = ocaml_var_name 

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
          e += "%s : %s;" $ f.Schema.name $ (Schema.to_ocaml_type f);
          e += "set_%s : %s -> unit;" $ f.Schema.name $ (Schema.to_ocaml_type f);
      ) fields;
      e += "save: int64; delete: unit";
    );
    e += "let init db =";
    e --> (fun e ->
      let fs,fsmany = List.partition (fun x -> match x.Schema.ty with |Schema.ForeignMany _ -> false |_ -> true) fields in
      let pid = "id integer primary key autoincrement" in
      let sqls = String.concat "," (pid :: List.map (fun f ->
        sprintf "%s %s" (Schema.ocaml_var_name f) (Schema.to_sql_type f.Schema.ty)
      ) (Schema.filter_out_id fs)) in
      let create_table table sql =
        e += "let sql = \"create table if not exists %s (%s);\" in" $ table $ sql;
        e += "db_must_ok (fun () -> Sqlite3.exec db.db sql);" in
      create_table module_name sqls;
      (* create foreign many-many tables now *)
      List.iter (fun fm -> match fm.Schema.ty with
        |Schema.ForeignMany ftable ->
          let table_name = Schema.map_table module_name fm in
          let sqls = sprintf "%s_id integer, %s_id integer, primary key(%s_id, %s_id)" module_name ftable module_name ftable in
          create_table table_name sqls;
        |_ -> assert false
      ) fsmany;
      e += "()";
       
    );
    e.nl(); 
    e --* "object definition";
    let label_names = String.concat " " (List.map (fun f ->
       match f.Schema.opt with
       |true -> sprintf "?(%s=None)" f.Schema.name
       |false -> sprintf "~%s" f.Schema.name) fields) in
    e += "let t %s db : t = object" $ label_names;
    e --> (fun e -> 
      e --* "get functions";
      List.iter (fun f ->
          e += "val mutable _%s = %s" $ f.Schema.name $ f.Schema.name;
          e += "method %s : %s = _%s" $ f.Schema.name $ (Schema.to_ocaml_type f) $ f.Schema.name;
      ) fields;
      e.nl ();
      e --* "set functions";
      List.iter (fun f ->
          e += "method set_%s v =" $ f.Schema.name;
          e --> (fun e ->
            e += "_%s <- v" $ f.Schema.name;
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
            e += "db_must_ok (fun () -> Sqlite3.bind stmt 1 (Sqlite3.Data.INT id));";
            e += "ignore(step_fold stmt (fun _ -> ()));";
            e += "_id <- None"
         );
      );
      e.nl ();
      e += "method save = transaction db (fun () ->";
      e --> (fun e ->
        e --* "insert any foreign-one fields into their table and get id";
        List.iter (fun f -> match f.Schema.ty,f.Schema.opt with
        |Schema.Foreign _,false ->
          e += "let _%s = %s#save in" $ (Schema.ocaml_var_name f) $ f.Schema.name;
        |Schema.Foreign _,true ->
          e += "let _%s = match %s with None -> None | Some x -> Some x#save in" $ (Schema.ocaml_var_name f) $ f.Schema.name;
        |Schema.ForeignMany _,_-> () (* this gets inserted later on after we have the current obj id *)
        |_ -> assert false
        ) foreign_fields;
        (* helper function to output the bind statements for a set of fields *)
        let output_bind_fields e fields =
          let pos = ref 1 in
          List.iter (fun f ->
             let var = match f.Schema.opt with
             |true -> sprintf "match _%s with |None -> Sqlite3.Data.NULL |Some v -> %s" 
               (Schema.ocaml_var_name f) (Schema.to_sql_type_wrapper f.Schema.ty)
             |false -> sprintf "let v = _%s in %s" 
               (Schema.ocaml_var_name f) (Schema.to_sql_type_wrapper f.Schema.ty) 
             in
             e += "db_must_ok (fun () -> Sqlite3.bind stmt %d (%s));" $ !pos $ var;
             incr pos;
          ) fields;
          !pos
        in
        e += "let _curobj_id = match _id with";
        e += "|None -> (* insert new record *)";
        e --> (fun e ->
          e -= "\"%s.save: inserting new record\"" $ module_name;
          let singular_fields = Schema.filter_out_id (Schema.filter_singular_fields fields) in
          let values = String.concat "," (List.map (fun f -> "?") singular_fields) in
          e += "let sql = \"INSERT INTO %s VALUES(NULL,%s)\" in" $ module_name $ values;
          e -= "\"%s.save: \" ^ sql" $ module_name;
          e += "let stmt = Sqlite3.prepare db.db sql in";
          ignore(output_bind_fields e singular_fields);
          e += "ignore(db_busy_retry (fun () -> Sqlite3.step stmt)); (* XXX add error check *)";
          e += "let __id = Sqlite3.last_insert_rowid db.db in";
          e += "_id <- Some __id;";
          e += "__id"
        );
        e += "|Some id -> (* update *)";
        e --> (fun e ->
          e -= "sprintf \"%s.save: id %%Lu exists, updating\" id" $ module_name;
          let up_fields = Schema.filter_out_id ( Schema.filter_singular_fields fields) in
          let set_vars = String.concat "," (List.map (fun f ->
            sprintf "%s%s=?" f.Schema.name (match f.Schema.ty with |Schema.Foreign _ -> "_id" |_ -> "")
          ) up_fields) in
          e += "let sql = \"UPDATE %s SET %s WHERE id=?\" in" $ module_name $ set_vars;
          e -= "\"%s.save: \" ^ sql" $ module_name;
          e += "let stmt = Sqlite3.prepare db.db sql in";
          let pos = output_bind_fields e up_fields in
          e += "db_must_ok (fun () -> Sqlite3.bind stmt %d (Sqlite3.Data.INT id));" $ pos;
          e += "ignore(db_busy_retry (fun () -> Sqlite3.step stmt)); (* XXX add error check *)";
          e += "id";
        );
        e += "in";
        List.iter (fun f -> match f.Schema.ty with
        |Schema.Foreign _ -> () (* done earlier *)
        |Schema.ForeignMany ftable -> 
          e += "List.iter (fun f ->";
          e --> (fun e ->
            e += "let _refobj_id = f#save in";
            e += "let sql = \"INSERT OR IGNORE INTO %s VALUES(?,?)\" in" $ (Schema.map_table module_name f);
            e -= "\"%s.save: foreign insert: \" ^ sql" $ module_name;
            e += "let stmt = Sqlite3.prepare db.db sql in";
            e += "db_must_ok (fun () -> Sqlite3.bind stmt 1 (Sqlite3.Data.INT _curobj_id));";
            e += "db_must_ok (fun () -> Sqlite3.bind stmt 2 (Sqlite3.Data.INT _refobj_id));";
            e += "ignore(step_fold stmt (fun _ -> ()));";
          );
          e += ") _%s;" $ f.Schema.name;
          e += "let ids = String.concat \",\" (List.map (fun x -> match x#id with |None -> assert false |Some x -> Int64.to_string x) _%s) in" $ f.Schema.name;
          e += "let sql = \"DELETE FROM %s WHERE %s_id=? AND (%s_id NOT IN (\" ^ ids ^ \"))\" in" $ (Schema.map_table module_name f) $ module_name $ ftable;
          e -= "\"%s.save: foreign drop gc: \" ^ sql" $ module_name;
          e += "let stmt = Sqlite3.prepare db.db sql in";
          e += "db_must_ok (fun () -> Sqlite3.bind stmt 1 (Sqlite3.Data.INT _curobj_id));";
          e += "ignore(step_fold stmt (fun _ -> ()));";
        |_ -> assert false
        ) foreign_fields;
        e += "_curobj_id";
      );
      e.p ")"
    );
    e += "end";
    e.nl ();
    e --* "General get function for any of the columns";
    e += "let get %s db =" $ 
      (String.concat " " (List.map (fun f -> sprintf "?(%s=None)" f.Schema.name) native_fields));
    e --> (fun e ->
      e --* "assemble the SQL query string";
      e += "let q = \"\" in";
      e += "let _first = ref true in";
      e += "let f () = match !_first with |true -> _first := false; \" WHERE \" |false -> \" AND \" in";
      List.iter (fun f ->
        e += "let q = match %s with |None -> q |Some b -> q ^ (f()) ^ \"%s.%s=?\" in" $
          f.Schema.name $ module_name $ f.Schema.name;
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
               sprintf "%s.%s" table (Schema.sql_var_name f)
             ) (Schema.filter_singular_fields (Schema.get_table_fields all table))
          ) (module_name :: (Schema.foreign_table_names all module_name))
        )
      ) in
      let joins = String.concat "" (List.map (fun f ->
           match f.Schema.ty with 
           |Schema.Foreign ftable -> sprintf "LEFT JOIN %s ON (%s.id = %s.%s_id) " ftable ftable module_name f.Schema.name
           |Schema.ForeignMany ftable -> ""
           |_ -> assert false
         ) foreign_fields) in
      e += "let q=\"SELECT %s FROM %s %s\" ^ q in" $ sql_field_names $ module_name $ joins;
      e -= "\"%s.get: \" ^ q" $ module_name;
      e += "let stmt=Sqlite3.prepare db.db q in";
      e --* "bind the position variables to the statement";
      e += "let bindpos = ref 1 in";
      List.iter (fun f ->
         e += "ignore(match %s with |None -> () |Some v ->" $ f.Schema.name;
         e --> (fun e ->
           e += "db_must_ok (fun () -> Sqlite3.bind stmt !bindpos (%s));" $ (Schema.to_sql_type_wrapper f.Schema.ty);
           e += "incr bindpos";
         );
         e += ");";
      ) native_fields;

      e --* "convert statement into an ocaml object";
      e += "let of_stmt stmt =";
      let rec of_stmt e table =
        let foreign_fields, fields = Schema.partition_table_fields all table in
        (if table = module_name then 
           e += "t" 
         else
           e += "%s.t" $ (String.capitalize table));
        e --> (fun e ->
          e --* "native fields";
          List.iter (fun f ->
             let col_pos = Hashtbl.find col_positions (table, f.Schema.name) in
             let from_sql = Schema.convert_from_sql f.Schema.ty in
             e += "~%s:(" $ f.Schema.name;
             (match f.Schema.opt with
             |false ->
               e --> (fun e -> 
                 e += "(let x = Sqlite3.column stmt %d in" $ col_pos;
                 e += "%s)" $ from_sql;
               );
             |true ->
               e += "(match Sqlite3.column stmt %d with" $ col_pos;
               e --> (fun e ->
                 e += "|Sqlite3.Data.NULL -> None";
                 e += "|x -> Some (%s))" $ from_sql;
               );
             );
             e += ")"
          ) fields;
          List.iter (fun f ->
            e += "~%s:(" $ f.Schema.name;
            e --> (fun e ->
              match f.Schema.ty,f.Schema.opt with
              |Schema.Foreign ftable,false ->
                of_stmt e ftable;
                e += ")"
              |Schema.Foreign ftable,true ->
                e += "Some (";
                of_stmt e ftable;
                e += "))"
              |Schema.ForeignMany ftable,_ ->
                e --* "foreign many-many mapping field";
                e += "let sql' = \"select %s_id from %s where %s_id=?\" in" $ ftable $ (Schema.map_table table f) $ table;
                e -= "\"%s.of_stmt (%s): \" ^ sql'" $ table $ ftable;
                e += "let stmt' = Sqlite3.prepare db.db sql' in";
                e += "let %s__id = Sqlite3.column stmt %d in" $ table $ (Hashtbl.find col_positions (table, "id"));
                e += "db_must_ok (fun () -> Sqlite3.bind stmt' 1 %s__id);" $ table; 
                e += "List.flatten (step_fold stmt' (fun s ->";
                e --> (fun e ->
                  e += "let i = match Sqlite3.column s 0 with |Sqlite3.Data.INT i -> i |_ -> assert false in";
                  e += "%s.get ~id:(Some i) db)" $ (String.capitalize ftable);
                );
                e += "))"
              |_ -> assert false
            );
          ) foreign_fields;
        );
        e += "db"
      in
      of_stmt e module_name;
      e += "in ";
      e --* "execute the SQL query";
      e += "step_fold stmt of_stmt"
  
    );
  )

let output_init_module e all =
   e += "exception Sql_error of (Sqlite3.Rc.t * string)";
   print_module e "Init" (fun e ->
     e += "type t = state";
     e += "let t db_name =";
     e --> (fun e ->
       e += "let db = {db=Sqlite3.db_open db_name; in_transaction=0} in";
       List.iter (fun (m,_) ->
         e += "%s.init db;" $ (String.capitalize m);
       ) all;
       e += "db";
     );
     e.nl ();
     e += "let db handle = handle.db"
   )

let output_module_interface e all (module_name, fields) =
  let raises = "Sql_error if a database error is encountered" in
  let foreign_fields, native_fields = Schema.partition_table_fields all module_name in
  print_module_sig e module_name (fun e ->
    print_object e "t" (fun e ->
      List.iter (fun f ->
          e += "%s : %s;" $ f.Schema.name $ (Schema.to_ocaml_type f);
          e += "set_%s : %s -> unit;" $ f.Schema.name $ (Schema.to_ocaml_type f);
      ) fields;
      e += "save: int64; delete: unit";
    );
    print_ocamldoc e "An object which can be stored in the database with the [save] method call, or removed by calling [delete].  Fields can be accessed via the approriate named method and set via the [set_] methods.  Changes are not committed to the database until [save] is invoked.";
    e.nl ();

    e += "val t :";
    e --> (fun e ->
      List.iter (fun f ->
        e += "%s%s:%s ->" $ (if f.Schema.opt then "?" else "") $ f.Schema.name $ (Schema.to_ocaml_type f)
      ) fields;
      e += "Init.t -> t";
    );
    print_ocamldoc e ~raises "Can be used to construct a new object.  If [id] is not specified, it will be automatically assigned the first time [save] is called on the object.  The object is not committed to the database until [save] is invoked.  The [save] method will also return the [id] assigned to the object.";
    e.nl ();
    e += "val get :";
    e --> (fun e ->
      List.iter (fun f ->
        e += "?%s:%s ->" $ f.Schema.name $ (Schema.to_ocaml_type ~always_opt:true f)
      ) native_fields
    );
    e += "Init.t -> t list";
    print_ocamldoc e ~raises "Used to retrieve objects from the database.  If an argument is specified, it is included in the search criteria (all fields are ANDed together)."
  )

let output_init_module_interface e =
  print_module_sig e "Init" (fun e ->
     e += "type t";
     print_ocamldoc e "Database handle which can be used to create and retrieve objects";
     e += "val t : string -> t";
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

let generate ?(debug=false) all output_basename =
  let mlout = open_out (output_basename ^ ".ml") in
  let e = init_printer ~msg:(Some "(* autogenerated by sql_orm *)") ~debug mlout in
  if e.dbg then e += "open Printf";
  output_sqlaccess_module e;
  e += "open Sql_access";
  List.iter (output_module e all) all;
  output_init_module e all;
  close_out mlout;
  let mliout = open_out (output_basename ^ ".mli") in
  let e = init_printer ~msg:(Some "(* autogenerated by sql_orm *)") ~debug mliout in
  print_ocamldoc e "Use the [[Init]] module to open a new database handle.  Each object type has its own module with functions to create, modify, save and destroy objects of that type into the SQLite database";
  e += "exception Sql_error of (Sqlite3.Rc.t * string)";
  output_init_module_interface e;
  List.iter (output_module_interface e all) all;
  close_out mliout
   

open OUnit
open Test_utils

type page = {
  parent : page option;  (* the optional parent, None -> root page *)
  title : string;
} with orm
 
let rec p1 = {
  parent = None;
  title = "root page";
}
and p2 = {
  parent = Some p1;
  title = "child page";
}
and p3 = {
  parent = Some p1;
  title = "child page 2";
};;
 
let db_name = "option_rec.db"
 
let save () =
 let db = open_db page_init db_name in
 page_save db p1;
 page_save db p2;
 page_save db p3

let get () =
 let db = page_init db_name in
  let pages = page_get db in ()

let suite = [
  "save" >:: save;
  "get" >:: get;
]

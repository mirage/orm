TYPE_CONV_PATH "Bib"

open Printf

type t =
  |Book
  |In_proceedings
  |In_book
  |In_collection
  |Article
  |Proceedings
  |Webpage
  |Tech_report
  |Phd_thesis
  |Masters_thesis
  |Unpublished
  |Misc

and ent = {
  ty: t;
  key: string;
  authors: string list;
  year: int option;
  title: string;
  misc: (string * string) list
} with orm

open OUnit
open Test_utils

let name = "bib.db"

let test_init () =
  ignore(open_db ent_init name);
  ignore(open_db ~rm:false ent_init name);
  ignore(open_db ~rm:false ent_init name)

let test_save () =
  let b = { ty=Book; key="akey"; authors=["Anil";"Thomas"]; year=(Some 2010); title="Do ORMs ever work?"; misc=["key","val"; "key2","val2"] } in
  let b2 = { ty=Book; key="23467"; authors=["Anil";"Thomas"]; year=(Some 2010); title="Do ORMs ever work?"; misc=["key","val"; "key2","val2"] } in
  let db = open_db ent_init name in
  ent_save db b;
  ent_save db b2

let test_get () =
  let db = open_db ent_init_read_only ~rm:false name in
  let _ = ent_get db in
  ()

let suite = [
  "bib_init" >:: test_init;
  "bib_save" >:: test_save;
  "bib_get" >:: test_get;
]

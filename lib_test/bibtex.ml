TYPE_CONV_PATH "Bibtex"

open Printf

type entry_type = string
and key = string
and atom =
  | Id of string
  | String of string
and command =
  | Comment of string
  | Preamble of atom list
  | Abbrev of string * atom list
  | Entry  of entry_type * key * (string * atom list) list
with orm

open OUnit
open Test_utils

let name = "bibtex.db"

let test_init () =
  ignore(open_db command_init name);
  ignore(open_db ~rm:false command_init name);
  ignore(open_db ~rm:false command_init name)

let test_save () =
  let b = Entry ("article", "foo123", [ "str", [Id "x"; String "y"] ]) in
  let db = open_db command_init name in
  command_save db b

let suite = [
  "bibtex_init" >:: test_init;
  "bibtex_save" >:: test_save;
]

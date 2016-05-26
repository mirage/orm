(*pp camlp4orf *)

open Printf

type exif_val =
    Exif_string of string
  | Exif_int of int64
  | Exif_float of float
and
  photo = {
  filename: string;
  metadata: (string * exif_val) list;
} with orm

open OUnit
open Test_utils

type image = string (* filename *)
(* XXX TODO: be able to wrap image in a module *)

let type_of_image = type_of_photo

(* marshalling to the database from a image *)
let value_of_image ~id_seed (img:image) : Dyntype.Value.t =
  (* printf "reading exif data from file: %s\n%!" img; *)
  let exif = [ "date", (Exif_string ("today " ^ img)) ] in
  let filename = img ^ ".jpg" in
  value_of_photo ~id_seed { filename=filename; metadata=exif }

(* marshalling from the database into an image type *)
let image_of_value (v:Dyntype.Value.t) : image =
  let p = photo_of_value v in
  (* printf "retrieving file from database: %s\n%!" p.filename; *)
  p.filename

let hash_of_image = Hashtbl.hash

type gallery = {
  date: float;
  contents: image list
} with orm

let name = "photo.db"

let test_init () =
  ignore(open_db gallery_init name);
  ignore(open_db ~rm:false gallery_init name)

let test_gallery () =
  let files = [ "p1"; "p2"; "p3" ] in
  let g = { date= 12345.0 ; contents = files } in
  let db = open_db gallery_init name in
  gallery_save db g;
  gallery_save db g;
  "list eq 3" @? (List.length (List.hd (gallery_get db)).contents = 3)

let suite = [
  "photo_init" >:: test_init;
  "photo_save" >:: test_gallery;
]

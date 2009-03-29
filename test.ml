(* test out the ORM library *)

open OUnit
open Ormtest
open Printf

let db_name = ref "test.db"

let must = function
   |None -> assert_failure "must"
   |Some x -> x

let never = function
   |Some x -> assert_failure "never"
   |None -> ()

let open_db ?(rm=false) () =
  if Sys.file_exists !db_name && rm then Sys.remove !db_name;
  Init.t !db_name

let test_init () =
   (* do two inits, should be idempotent *)
   let _ = open_db () in
   let _ = open_db () in
   let _ = open_db ~rm:true () in
   ()

let test_simple_insert_update_delete () =
   let db = open_db ~rm:true () in
   let now = Unix.gettimeofday () in
   let contact = Contact.t ~first_name:"John" ~last_name:"Smith"
     ~email:"john@example.com" ~mtime:now db in
   printf "inserting\n%!";
   let id = contact#save in
   "contact has id" @? (contact#id <> None);
   printf "saved\n%!";
   let contact' = Contact.get ~id:(Some id) db in
   List.iter (fun c ->
     printf "contact_name: %s %s\n%!" c#first_name c#last_name
   ) contact';
   assert_equal (List.length contact') 1; 
   printf "saving same object again\n%!";
   let id2 = contact#save in
   assert_equal id id2;
   let contact'' = Contact.get ~id:(Some id2) db in
   assert_equal (List.length contact'') 1;
   assert_equal (List.hd contact'')#id (List.hd contact')#id;
   assert_equal contact#id (List.hd contact')#id;
   printf "changing first name\n%!";
   contact#set_first_name "Foo";
   ignore(contact#save);
   assert_equal contact#first_name "Foo";
   let contact' = List.hd (Contact.get ~id:(Some id) db) in
   assert_equal contact'#first_name "Foo";
   ()

let suite = "SQL ORM test" >:::
    [  "test_init" >:: test_init ;
       "test_simple_insert" >:: test_simple_insert_update_delete; 
    ]

(* Returns true if the result list contains successes only *)
let rec was_successful results =
  match results with
      [] -> true
    | RSuccess _::t
    | RSkip _::t -> was_successful t
    | RFailure _::_
    | RError _::_
    | RTodo _::_ -> false

let _ =
  let verbose = ref false in
  let set_verbose _ = verbose := true in
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the tests in verbose mode.");]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");

  if not (was_successful (run_test_tt ~verbose:!verbose suite)) then
    exit 1

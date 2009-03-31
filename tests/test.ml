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

open OUnit
open Ormtest
open Printf

let db_name = ref "test.db"

let open_db ?(rm=false) () =
  if Sys.file_exists !db_name && rm then Sys.remove !db_name;
  Init.t !db_name

let test_init () =
   (* do two inits, should be idempotent *)
   let _ = open_db () in
   let _ = open_db () in
   let _ = open_db ~rm:true () in
   ()

let gen_contact fname lname db =
   let now = Unix.gettimeofday () in
   Contact.t ~first_name:fname ~last_name:lname
     ~email:(sprintf "%s.%s@example.com" fname lname) ~mtime:now ~vcards:[] ~notes:[]
     db 

let test_simple_insert_update_delete () =
   let db = open_db ~rm:true () in
   let contact = gen_contact "John" "Smith" db in
   let id = contact#save in
   "contact has id" @? (contact#id <> None);
   let contact' = Contact.get ~id:(Some id) db in
   assert_equal (List.length contact') 1; 
   let id2 = contact#save in
   assert_equal id id2;
   let contact'' = Contact.get ~id:(Some id2) db in
   assert_equal (List.length contact'') 1;
   assert_equal (List.hd contact'')#id (List.hd contact')#id;
   assert_equal contact#id (List.hd contact')#id;
   contact#set_first_name "Foo";
   ignore(contact#save);
   assert_equal contact#first_name "Foo";
   let contact' = List.hd (Contact.get ~id:(Some id) db) in
   assert_equal contact'#first_name "Foo";
   contact#delete;
   assert_equal contact#id None;
   let id' = contact#save in
   "contact has new id" @? (id' <> id)

let test_gets () =
   let db = open_db ~rm:true () in
   let c = gen_contact "Foo" "Bar" db in
   let check cl = 
      "one contact" @? (List.length cl = 1);
      let c = List.hd cl in
      "contact id" @? (c#id = Some 1L);
      "contact fname" @? (c#first_name = "Foo");
      "contact lname" @? (c#last_name = "Bar");
      "contact email" @? (c#email = "Foo.Bar@example.com")
   in
   let cid = c#save in
   check (Contact.get ~id:(Some cid) db);
   check (Contact.get ~first_name:(Some "Foo") db);
   check (Contact.get ~last_name:(Some "Bar") db);
   check (Contact.get ~email:(Some "Foo.Bar@example.com") db)

let test_new_foreign_map () =
   let db = open_db ~rm:true () in
   let now = Unix.gettimeofday () in
   let from = gen_contact "John" "Smith" db in
   let cto = List.map (fun (a,b) -> gen_contact a b db) [
      ("Alice","Aardvark"); ("Bob","Bear"); ("Charlie","Chaplin") ] in
   let atts = [] in
   let e = Entry.t ~body:"Test Body" ~received:now ~people_from:from
     ~atts:atts ~people_to:cto db in
   let eid = e#save in
   "entry has an id" @? (e#id <> None);
   assert_equal (Some eid) e#id;
   ()

let test_multiple_foreign_map () =
   let db = open_db ~rm:true () in
   let now = Unix.gettimeofday () in
   let vcard1 = Attachment.t ~file_name:"vcard1.vcs" ~mime_type:"vcard" db in
   let vcard2 = Attachment.t ~file_name:"vcard2.vcs" ~mime_type:"vcard" db in
   let vcard3 = Attachment.t ~file_name:"vcard3.vcs" ~mime_type:"vcard" db in
   let note1 =  Attachment.t ~file_name:"note1.txt"  ~mime_type:"note"  db in
   let note2 =  Attachment.t ~file_name:"note2.txt"  ~mime_type:"note"  db in
   (* contact without an image *)
   let contact = Contact.t ~first_name:"Foo" ~last_name:"Bar" ~email:"foobar@example.com"
     ~mtime:now ~vcards:[vcard1;vcard2] ~notes:[note1;note2] db in
   let cid = contact#save in
   let get_contact_with_id cid =
     let contact' = Contact.get ~id:(Some cid) db in
     assert_equal (List.length contact') 1;
     List.hd contact' in
   let contact' = get_contact_with_id cid in
   assert_equal contact#id contact'#id;
   let vcards = contact#vcards in
   assert_equal (List.length vcards) 2;
   let [vcard1';vcard2'] = vcards in
   assert_equal "vcard1.vcs" vcard1'#file_name;
   assert_equal "vcard2.vcs" vcard2'#file_name;
   contact#set_vcards [vcard1; vcard3];
   let cid = contact#save in
   let contact' = get_contact_with_id cid in
   let vcards' = contact'#vcards in
   "2 vcards back" @? (List.length vcards' = 2);
   let [vcard3'; vcard1'] = vcards' in
   "first vcard is same" @? ("vcard1.vcs" = vcard1'#file_name);
   "second vcard is same" @? ("vcard3.vcs" = vcard3'#file_name);
   contact'#set_vcards (note1 :: vcards);
   let cid = contact'#save in
   let contact = get_contact_with_id cid in
   let notes = contact#notes in
   let vcards = contact#vcards in
   "still 2 notes" @? (List.length notes = 2);
   "and 3 vcards" @? (List.length vcards = 3);
   "one vcard has mimetype note" @? (List.length (List.find_all (fun x -> x#mime_type = "note") vcards) = 1);
   "two vcards have mimetype vcard" @? (List.length (List.find_all (fun x -> x#mime_type = "vcard") vcards) = 2);
   let contact = get_contact_with_id cid in
   contact#set_notes [];
   let cid = contact#save in
   let contact = get_contact_with_id cid in
   let notes = contact#notes in
   let vcards = contact#vcards in
   "now 0 notes" @? (List.length notes = 0);
   "and 3 vcards" @? (List.length vcards = 3);
   "one vcard has mimetype note" @? (List.length (List.find_all (fun x -> x#mime_type = "note") vcards) = 1);
   "two vcards have mimetype vcard" @? (List.length (List.find_all (fun x -> x#mime_type = "vcard") vcards) = 2);
   ()

let suite = "SQL ORM test" >::: [
       "test_init" >:: test_init ;
       "test_simple_insert" >:: test_simple_insert_update_delete; 
       "test_gets" >:: test_gets;
       "test_new_foreign_map" >:: test_new_foreign_map;
       "test_multiple_foreign_map" >:: test_multiple_foreign_map;
    ]

let _ =
   run_test_tt_main suite

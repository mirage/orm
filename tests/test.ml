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
open Printf

module Basic = struct
   open Ormtest
    let open_db ?(rm=false) name =
       if Sys.file_exists name && rm then Sys.remove name;
       Init.t name

    let test_init () =
       (* do two inits, should be idempotent *)
       let _ = open_db "test.db" in
       let _ = open_db "test.db" in
       let _ = open_db ~rm:true "test.db" in
       ()

    let gen_contact fname lname db =
       let now = Unix.gettimeofday () in
       Contact.t ~first_name:fname ~last_name:lname
         ~email:(Some (sprintf "%s.%s@example.com" fname lname)) ~mtime:now ~vcards:[] ~notes:[]
         db 

    let test_simple_insert_update_delete () =
       let db = open_db ~rm:true "test_insert.db" in
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
       let db = open_db ~rm:true "test_gets.db" in
       let c = gen_contact "Foo" "Bar" db in
       let check cl = 
          "one contact" @? (List.length cl = 1);
          let c = List.hd cl in
          "contact id" @? (c#id = Some 1L);
          "contact fname" @? (c#first_name = "Foo");
          "contact lname" @? (c#last_name = "Bar");
          "contact email" @? (match c#email with Some x -> x = "Foo.Bar@example.com" |None -> false)
       in
       let cid = c#save in
       check (Contact.get ~id:(Some cid) db);
       check (Contact.get ~first_name:(Some "Foo") db);
       check (Contact.get ~last_name:(Some "Bar") db);
       check (Contact.get ~email:(Some "Foo.Bar@example.com") db);
       let c = gen_contact "Alice" "Bob" db in
       ignore(c#save);
       let all = Contact.get db in
       "2 entries" @? (List.length all = 2);
       "entries valid" @? (List.sort compare (List.map (fun x -> match x#email with Some x -> x|None -> assert_failure "bad email") all) =
         ["Alice.Bob@example.com"; "Foo.Bar@example.com"]);
       let c = Contact.get ~first_name:(Some "Foo") ~last_name:(Some "Bar") db in
       "multiple field get 1 result" @? (List.length c = 1);
       "correct id" @? ((List.hd c)#id = Some cid)

    let test_new_foreign_map () =
       let db = open_db ~rm:true "test_new_foreign.db" in
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
       let db = open_db ~rm:true "test_multiple_foreign.db"  in
       let now = Unix.gettimeofday () in
       let vcard1 = Attachment.t ~file_name:"vcard1.vcs" ~mime_type:"vcard" db in
       let vcard2 = Attachment.t ~file_name:"vcard2.vcs" ~mime_type:"vcard" db in
       let vcard3 = Attachment.t ~file_name:"vcard3.vcs" ~mime_type:"vcard" db in
       let note1 =  Attachment.t ~file_name:"note1.txt"  ~mime_type:"note"  db in
       let note2 =  Attachment.t ~file_name:"note2.txt"  ~mime_type:"note"  db in
       (* contact without an image *)
       let contact = Contact.t ~first_name:"Foo" ~last_name:"Bar" ~email:(Some "foobar@example.com")
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
       let vcard1',vcard2' = match vcards with [a;b] -> a,b |_ -> assert false in
       assert_equal "vcard1.vcs" vcard1'#file_name;
       assert_equal "vcard2.vcs" vcard2'#file_name;
       contact#set_vcards [vcard1; vcard3];
       let cid = contact#save in
       let contact' = get_contact_with_id cid in
       let vcards' = contact'#vcards in
       "2 vcards back" @? (List.length vcards' = 2);
       let vcard3',vcard1' = match vcards' with |[a;b] -> a,b |_ -> assert false in
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
       let image = contact#image in
       "image should be None" @? (image = None)

   let test_special_gets () =
       let db = open_db ~rm:true "test_special_gets.db"  in
       let now = Unix.gettimeofday () in
       let vcard1 = Attachment.t ~file_name:"vcard1.vcs" ~mime_type:"vcard" db in
       let vcard2 = Attachment.t ~file_name:"vcard2.vcs" ~mime_type:"vcard" db in
       let vcard3 = Attachment.t ~file_name:"vcard3.vcs" ~mime_type:"vcard" db in
       let note1 =  Attachment.t ~file_name:"note1.txt"  ~mime_type:"note"  db in
       let note2 =  Attachment.t ~file_name:"note2.txt"  ~mime_type:"note"  db in
       (* contact without an image *)
       let contact = Contact.t ~first_name:"Foo" ~last_name:"Bar" ~email:(Some "foobar@example.com")
         ~mtime:now ~vcards:[vcard1;vcard2] ~notes:[note1;note2] db in
       let cid = contact#save in
       let r = Contact.get_by_id ~id:(Some cid) db in
       "one result" @? (List.length r = 1);
       let r = List.hd r in
       let r' = Contact.get_by_id ~id:(Some cid) db in
       "one result" @? (List.length r' = 1);
       let r' = List.hd r' in
       "get_by_id fname same as get" @? (r'#first_name = r#first_name);
       "get_by_id lname same as get" @? (r'#last_name = r#last_name);
       "get_by_id email same as get" @? (r'#email = r#email);
       "get_by_id vcards same as get" @? (List.map (fun x -> x#file_name) r'#vcards = (List.map (fun x -> x#file_name) r#vcards));
       let r = Contact.get_first_name_image_vcards_by_email ~email:(Some "foobar@example.com") db in
       "one result" @? (List.length r = 1);
       let fname, image, vcards = List.hd r in
       "get_complex_by_email fname" @? (fname = "Foo");
       "get_complex_by_email image" @? (image = None);
       "get_complex_by_email vcards" @? (List.map (fun x -> x#file_name) r'#vcards = (List.map (fun x -> x#file_name) vcards));
       let x = Contact.get_mtime_id_by_first_name ~first_name:"Foo" db in
       "one result" @? (List.length x = 1);
       let mtime, xid = List.hd x in
       assert_equal (Int64.of_float mtime) (Int64.of_float now);
       assert_equal xid (Some cid);
       (* try out multiple by fields *)
       let x = Contact.get_by_first_name_last_name ~first_name:"Foo" ~last_name:"Bar" db in
       "one res" @? (List.length x = 1);
       let x = List.hd x in
       "fname is correct" @? (x#first_name = "Foo");
       "lname is correct" @? (x#last_name = "Bar");
       (* try incorrect get *)
       let x = Contact.get_by_first_name_last_name ~first_name:"Foo" ~last_name:"XXX" db in
       "no res" @? (List.length x = 0);
       (* entry get *)
       let entry = Entry.t ~body:"Body of text" ~received:now ~people_from:contact ~atts:[vcard1;vcard2;vcard3] ~people_to:[contact] db in
       let _ = entry#save in
       let e = Entry.get_people_from_atts_by_body ~body:"Body of text" db in
       "1 res" @? (List.length e = 1);
       let frm,atts = List.hd e in
       "frm same" @? (frm#first_name = "Foo");
       "atts same" @? (List.sort compare (List.map (fun x -> x#file_name) atts) = ["vcard1.vcs"; "vcard2.vcs"; "vcard3.vcs"]);
       let all_files = Attachment.get_file_name db in
       List.iter print_endline all_files;
       "all_filenames" @? (List.sort compare all_files = ["note1.txt";"note2.txt";"vcard1.vcs";"vcard2.vcs";"vcard3.vcs"])

   let suite = [
       "test_init" >:: test_init ;
       "test_simple_insert" >:: test_simple_insert_update_delete; 
       "test_gets" >:: test_gets;
       "test_new_foreign_map" >:: test_new_foreign_map;
       "test_multiple_foreign_map" >:: test_multiple_foreign_map;
       "test_special_gets" >:: test_special_gets;
   ]
end

module Foreign = struct
    open Foreign
    
    let open_db ?(rm=false) name =
       if Sys.file_exists name && rm then Sys.remove name;
       Init.t name
    
    let test_init () =
       (* do two inits, should be idempotent *)
       let _ = open_db "foreign.db" in
       let _ = open_db "foreign.db" in
       let _ = open_db ~rm:true "foreign.db" in
       ()

    let test_mid_insert () =
       let db = open_db ~rm:true "foreign.db" in
       let now = Unix.gettimeofday () in
       let base1 = Base.t ~field1:"field1 text" ~date1:now ~int1:100L db in
       let base2 = Base.t ~field1:"field2 text" ~date1:now ~int1:200L db in
       let base3 = Base.t ~field1:"field3 text" ~date1:now ~int1:300L db in
       let mid1 = Middle.t ~f1:base1 ~f2:base2 ~f3:[base1;base3] ~f4:[base2;base3] db in
       let mid1_id = mid1#save in
       let mid1' = Middle.get ~id:(Some mid1_id) db in
       "mid1 single element" @? (List.length mid1' = 1);
       let mid1' = List.hd mid1' in
       "mid1 f4 correct" @? (List.sort compare (List.map (fun x -> x#int1) mid1'#f4) = [200L; 300L]);
       "mid1 f3 correct" @? (List.sort compare (List.map (fun x -> x#int1) mid1'#f3) = [100L; 300L]);
       "mid1 f1 correct" @? (mid1'#f1#field1 = "field1 text")

    let test_last_insert () =
       let db = open_db ~rm:true "foreign_full.db" in
       let now = Unix.gettimeofday () in
       let base1 = Base.t ~field1:"field1 text" ~date1:now ~int1:100L db in
       let base2 = Base.t ~field1:"field2 text" ~date1:now ~int1:200L db in
       let base3 = Base.t ~field1:"field3 text" ~date1:now ~int1:300L db in
       let mid1 = Middle.t ~f1:base1 ~f2:base2 ~f3:[base1;base3] ~f4:[base2;base3] db in
       let last1 = Last.t ~l1:mid1 db in
       let lastid = last1#save in
       let last1' = Last.get ~id:(Some lastid) db in
       "one last result" @? (List.length last1' = 1);
       let last1' = List.hd last1' in
       "last1 middle base1 f1" @? (last1'#l1#f1#field1= "field1 text");
       "last1 middle base1 f2" @? (last1'#l1#f2#field1= "field2 text");
       "last1 middle base1 date1" @? (Int64.of_float (last1'#l1#f2#date1) = Int64.of_float(now));
       "last1 middle base1 int1" @? (last1'#l1#f2#int1= 200L);
       ()
 
       
    let suite = [
       "foreign_init" >:: test_init;
       "foreign_mid_insert" >:: test_mid_insert;
       "foreign_last_insert" >:: test_last_insert;
    ]
end

let suite = "SQL ORM test" >::: (Basic.suite @ Foreign.suite)

let _ =
   run_test_tt_main suite

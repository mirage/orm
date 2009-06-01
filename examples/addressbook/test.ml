open My_db
open Printf

let _ =

  let db = Init.t "test.db" in

  printf "creating new user Anil\n%!";
  let me = Person.t ~name:"Anil" ~email:"anil@recoil.org" db in

  let id = me#save in
  printf "saved new object (id %Lu)\n%!" id;

  me#set_age (Some 30L);
  let id = me#save in
  printf "updated age field (id %Lu)\n%!" id;

  match Person.get ~age:(Some 30L) db with
  |[] -> printf "no people found\n%!"
  |ps -> List.iter (fun p -> 
    printf "retrieved person: %s %s\n%!" p#name p#email) ps

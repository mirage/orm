let db = "big_list.db" 

open Test_utils
open OUnit

type t = int list with orm

let rec genl acc = function 
  | 0 -> acc
  | n -> genl (n::acc) (n-1) 

let big_save () =
  let db = open_db t_init db in
  for i = 2 to 5 do 
    let l = genl [] (i*100) in
    t_save ~db l
  done 

let big_get () =
  let db = open_db ~rm:false t_init db in
  let all = t_get db in
  let item i =
    (List.exists (fun l -> List.length l = i) all) in
                         
  "cardinal" @? (List.length all = 4);
  "items:200" @? item 200;
  "items:300" @? item 200;
  "items:400" @? item 200;
  "items:500" @? item 200
  

let suite = [
  "big_save" >:: big_save;
  "big_get" >:: big_get;
]

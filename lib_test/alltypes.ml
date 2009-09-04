TYPE_CONV_PATH "Alltypes"

type t = {
  one: char;
  two: string;
  three: int;
  four: int32;
  five: bool;
  six: int64;
  seven: unit;
  eight: string option;
  nine: float;
  ten: (int * string);
  eleven: string list;
  twelve: (char * int32 * unit) option;
  thirteen: (char * (string * int64) option);
} with persist()

let _ =
  let db = orm_init_db "alltypes.db" in
  let t1 = t_new ~one:'a' ~two:"foo" ~three:1 ~four:2l 
     ~five:true ~six:3L ~seven:() ~eight:(Some "bar") 
     ~nine:6.9 ~ten:(100,"hello") ~eleven:["aa";"bb";"cc"]
     ~twelve:(Some ('t',9l,())) ~thirteen:('d', (Some ("abc",999L))) db in
  Printf.printf "saved: %Lu %Lu\n%!" t1#save t1#save;
  List.iter
    (fun t -> Printf.printf "found <one=%c> where two='foo'\n%!" t#one)
    (t_get ~two:(`Eq "foo") db);
  List.iter
    (fun t -> Printf.printf "found <one=%c> where two contains 'o'\n%!" t#one)
    (t_get ~two:(`Contains "o") db)

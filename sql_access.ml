open Sqlite3
open Printf

exception Sql_error of (Rc.t * string)

let raise_sql_error x =
    raise (Sql_error (x, (Rc.to_string x)))

let try_finally fn finalfn =
    try
      let r = fn () in
      finalfn ();
      r
    with e -> begin
      print_endline "WARNING: exception";
      finalfn ();
      raise e
    end

(* retry until a non-BUSY error code is returned *)
let rec db_busy_retry fn =
    match fn () with
    |Rc.BUSY -> 
       print_endline "WARNING: busy";
       Thread.delay (Random.float 1.);
       db_busy_retry fn
    |x -> x

let db_must_ok fn =
    match db_busy_retry fn with
    |Rc.OK -> ()
    |x -> raise_sql_error x

let step_fold stmt iterfn =
    let stepfn () = Sqlite3.step stmt in
    let rec fn a = match db_busy_retry stepfn with
    |Sqlite3.Rc.ROW -> fn (iterfn stmt :: a)
    |Sqlite3.Rc.DONE -> a
    |x -> raise_sql_error x
    in
    fn []

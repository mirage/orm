(*
 * Copyright (c) 2009-2010
 *     Anil Madhavapeddy <anil@recoil.org>
 *     Thomas Gazagnaire <thomas@gazagnaire.com>
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

open Printf
open Sqlite3
open Sql_backend
open Value

exception Sql_process_error of Value.t * string

let process_error t s =
  Printf.printf "ERROR(%s): %s\n%!" s (to_string t);
  raise (Sql_process_error (t,s))

let exec_sql ~env ~db sql binds fn =
	exec_sql ~tag:"save" ~env ~db sql binds fn

let assert_exists ~env ~db table_name id =
	let select = sprintf "SELECT * FROM %s WHERE __id__=?" table_name in
	exec_sql ~env ~db select [Data.INT id]
		(fun stmt ->
			 if not (List.length (step_map db stmt (fun stmt -> column stmt 0)) = 1) then
				 failwith (sprintf "%s:%Ld doesn't exist" table_name id)
		)

(* Return a fresh ID in the given table (and reserve it to update it later) *)
let empty_row ~env ~db table_name : int64 =
	let insert = sprintf "INSERT INTO %s (__id__) VALUES (NULL);" table_name in
	exec_sql ~env ~db insert [] (db_must_step db);
	let id = last_insert_rowid db.db in
	debug (Printf.sprintf "%s:%d" db.name db.uuid) env `Sql "save" (Printf.sprintf "last_insert_rowid = %Ld" id);
	(* assert_exists ~env ~db table_name id; *)
	id


(* Insert/update a specific row in a specific table *)
let process_row ~env ~db table_name field_names field_values v =
	let qmarks = List.map (fun _ -> "?") field_names in
	let constraints =
		List.map2 (fun f v -> if v = Data.NULL then sprintf "%s ISNULL" f else sprintf "%s=?" f) field_names field_values in
    let insert = sprintf "INSERT INTO %s (%s) VALUES (%s);" table_name (String.concat "," field_names) (String.concat "," qmarks) in
    let select = sprintf "SELECT __id__ FROM %s WHERE %s;" table_name (String.concat " AND " constraints) in
    let fn stmt = step_map db stmt (fun stmt -> column stmt 0) in
    match exec_sql ~env ~db select (List.filter (fun v -> v <> Data.NULL) field_values) fn with
    | [Data.INT i ] -> i
    | []            -> exec_sql ~env ~db insert field_values (db_must_step db); last_insert_rowid db.db
    | ds            -> process_error v (sprintf "Found {%s}" (String.concat "," (List.map string_of_data ds)))

(* Cut a list into chunks of size [chunk_size] *)
(* The result is in the reverse order *)
let cut_into_chunks chunk_size l =
  let rec aux chunks n accu = function
    | []   -> (List.rev accu) :: chunks
    | h::t ->
      if n mod chunk_size = 0 then
        aux ((List.rev (h :: accu)) :: chunks) (n+1) [] t
      else
        aux chunks (n+1) (h :: accu) t in
  aux [] 1 [] l


(*
let _ =
  cut_into_chunks 3 [1;2;3;4;5;6;7;8]
*)

let combine chunks =
  let rec one list accu (a,b) = match list with
    | (c,d)::t when b=c -> one t ((a,d)::accu) (a,b)
    | _ :: t -> one t accu (a,b)
    | [] -> accu in
  let rec merge l1 l2 =
    List.fold_left (one l2) [] l1 in
  let rec aux = function
    | []        -> []
    | [l]       -> l
    | l1::l2::t -> aux ((merge l1 l2)::t) in
  aux chunks

(*
let _ =
  combine [ 
    [ (1,2); (1,3) ];
    [ (2,4); (3,5); (5,6); ];
    [ (4,5); (5,6); (7,8); ];
  ]
*)

(* Insert a collection of rows in a specific table *)
let process_enum_rows ~env ~db table_name field_names field_values_enum v =

  let get_chunk chunk next_chunks =
	  let join =
		  sprintf "%s as __t0__" table_name ::
			  list_mapi (fun i _ -> sprintf "%s AS __t%i__ ON __t%i__.__next__=__t%i__.__id__" table_name (i+1) i (i+1)) (List.tl chunk) in
	let constraints =
		List.flatten (list_mapi (fun i _ -> List.map (fun f -> sprintf "__t%i__.%s=?" i f) field_names) chunk) 
    @ (match next_chunks with
      | [] -> [sprintf "__t%i__.__next__ ISNULL" (List.length chunk - 1)]
      | l  -> [sprintf "(%s)" (String.concat " OR " (List.map (sprintf "__t%i__.__next__ = %Ld" (List.length chunk - 1)) l))]) in
	let binds =
		List.flatten chunk in
	let select = sprintf "SELECT __t0__.__id__ FROM %s WHERE %s;"
		(String.concat " JOIN " join)
		(String.concat " AND " constraints) in
	let fn stmt = step_map db stmt (fun stmt -> column stmt 0) in
	match exec_sql ~env ~db select binds fn with
  | [] -> raise Not_found
  | l  -> List.map (function Data.INT i -> i | k -> process_error v "get_chunk") l in

  let get_by_chunks () =
    let next_chunks = ref [] in
    let chunks = Array.of_list (cut_into_chunks max_join field_values_enum) in
    try
      for i = 0 to Array.length chunks - 1 do
        next_chunks := get_chunk chunks.(i) !next_chunks;
      done;
      !next_chunks
    with _ -> [] in

  (* The array is in reverse order *)
  let ids = Array.create (List.length field_values_enum) (-1L) in
  let get_id n =
    if n < 0 then
      Data.NULL
    else
      Data.INT ids.(n) in
  let first () =
    match ids.(List.length field_values_enum - 1) with
    | -1L -> process_error v "Empy enum"
    | i   -> i in

  match get_by_chunks () with
		| [i] -> i
		| []  ->
			let rec save i = function
				| []                -> first ()
				| field_values :: t ->
					let id = process_row ~env ~db
						table_name
						("__next__" :: "__next_chunk__" :: "__size__" :: field_names)
						(get_id (i-1) :: get_id (i-max_join) :: Data.INT (Int64.of_int (i+1)) :: field_values)
						v in
          ids.(i) <- id;
					save (i+1) t in
			save 0 (List.rev field_values_enum)
		| ds           -> process_error v (sprintf "Found {%s}" (String.concat "," (List.map Int64.to_string ds)))

let rec value_of_field ~env ~db name v =
	match v with
	| Unit          -> [ Data.INT 0L ] 
	| Null          -> [ Data.INT 0L ]
	| Value v       -> Data.INT 1L :: value_of_field ~env ~db (Name.option name) v
	| Int i         -> [ Data.INT i ]
	| Bool b        -> if b then [ Data.INT 1L ] else [ Data.INT 0L ]
	| Float f       -> [ Data.FLOAT f ]
	| String s      -> [ Data.TEXT s ]
	| Arrow a       -> [ Data.BLOB a ]
	| Enum []       -> [ Data.NULL ]
	| Enum (h::tl)  ->
		let id = process_enum_rows ~env ~db
			(Name.enum name)
			(field_names_of_value ~id:false h)
			(List.map (value_of_field ~env ~db (Name.enum name)) (h::tl)) v in
		[ Data.INT id ]
	| Tuple tl      -> list_foldi (fun accu i t -> accu @ value_of_field ~env ~db (Name.tuple name i) t) [] tl
	| Dict tl       -> List.fold_left (fun accu (n,t) -> accu @ value_of_field ~env ~db (Name.dict name n) t) [] tl
	| Sum (r,tl)    -> Data.TEXT r :: list_foldi (fun accu i t -> accu @ value_of_field ~env ~db (Name.sum name r i) t) [] tl
	| Var (_,i)     
	| Rec ((_,i),_) 
	| Ext ((_,i),_) -> [ Data.INT i ]

let replace_row ~env ~db table_name id field_names field_values =
	(* assert_exists ~env ~db table_name id; *)
	let field_names = List.map (fun f -> sprintf "%s=?" f) field_names in
	let replace = sprintf "UPDATE %s SET %s WHERE __id__=%Ld;" table_name (String.concat "," field_names) id in
	exec_sql ~env ~db replace field_values (db_must_step db)

let rec update_value ~env ~db v =
	match v with
	| Ext ((n,i), s)
	| Rec ((n,i), s) ->
		let field_names = field_names_of_value ~id:false s in
		let field_values = value_of_field ~env ~db n s in
		replace_row ~env ~db n i field_names field_values;
		update_value ~env ~db s
	| Enum el        -> List.iter (update_value ~env ~db) el
	| Tuple tl       -> List.iter (update_value ~env ~db) tl
	| Dict tl        -> List.iter (fun (_,s) -> update_value ~env ~db s) tl
	| Sum (r, tl)    -> List.iter (update_value ~env ~db) tl
	| Value s        -> update_value ~env ~db s
	| _              -> ()

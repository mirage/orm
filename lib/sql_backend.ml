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

open Sqlite3
open Printf

(* Maximum number of JOINS *)
let max_join = 64

type transaction_mode = [ `Deferred |`Immediate |`Exclusive ]

type state = {
	uuid : int;
	name : string;
	db : db;
	mutable in_transaction: int;
	busyfn: db -> unit;
	mode: transaction_mode;
}

type env = [
      `Debug of string list
    | `Dot of string
    | `Index of (string * string list) list
    | `Unique of (string * string list) list ] list

let debug db (env:env) ty n e = 
	let in_env s = List.exists (function | `Debug sl -> List.mem s sl | _ -> false)  env in
	let d () = Printf.eprintf "%s(%s): %s\n%!" n db e in
	let b () = () in
	if match ty with
		|`Sql -> in_env "sql" || in_env "all"
		|`Cache -> in_env "cache" || in_env "all"
		|`Bind -> in_env "bind" || in_env "all"
	then d() else b()

let default_busyfn (db:Sqlite3.db) =
    print_endline "WARNING: busy";
    Unix.sleep 1

let new_state name = {
	uuid = Random.int 100;
	name = name;
	db = db_open name;
	in_transaction = 0;
	busyfn = default_busyfn;
	mode = `Deferred
}

let raise_sql_error x =
	raise (Sqlite3.Error (Rc.to_string x))

let try_finally fn finalfn =
	try
		let r = fn () in
		finalfn ();
		r
    with e -> begin
		print_endline (sprintf "WARNING: exception: %s" (Printexc.to_string e));
		finalfn ();
		raise e
    end

(* retry until a non-BUSY error code is returned *)
let rec db_busy_retry db fn =
	match fn () with
	| Rc.BUSY -> 
		db.busyfn db.db;
		db_busy_retry db fn;
    | x -> x

(* make sure an OK is returned from the database *)
let db_must_ok db fn =
	match db_busy_retry db fn with
	| Rc.OK -> ()
	| x -> raise_sql_error x

(* make sure a DONE is returned from the database *)
let db_must_done db fn = 
	match db_busy_retry db fn with
	| Rc.DONE -> ()
	| x -> raise_sql_error x

let db_must_bind db stmt pos data =
	db_must_ok db (fun () -> Sqlite3.bind stmt pos data)

let db_must_reset db stmt =
	db_must_ok db (fun () -> Sqlite3.reset stmt)

let db_must_step db stmt =
	db_must_done db (fun () -> Sqlite3.step stmt)

(* request a transaction *)
let transaction db fn =
	let m = match db.mode with
	| `Deferred -> "DEFERRED"
	| `Immediate -> "IMMEDIATE"
	| `Exclusive -> "EXCLUSIVE" in
	try_finally
		(fun () ->
			if db.in_transaction = 0 then
				db_must_ok db (fun () -> exec db.db (sprintf "BEGIN %s TRANSACTION" m));
			db.in_transaction <- db.in_transaction + 1;
			fn ();
		) (fun () ->
			if db.in_transaction = 1 then
				db_must_ok db (fun () -> exec db.db "END TRANSACTION");
			 db.in_transaction <- db.in_transaction - 1
		)

(* iterate over a result set *)
let step_map db stmt iterfn =
	let stepfn () = Sqlite3.step stmt in
	let rec fn a = match db_busy_retry db stepfn with
		| Sqlite3.Rc.ROW -> fn (iterfn stmt :: a)
		| Sqlite3.Rc.DONE -> a
		| x -> raise_sql_error x in
	fn []

let list_foldi fn accu l =
	let accu, _ = List.fold_left (fun (accu, i) x -> fn accu i x, i + 1) (accu, 0) l in accu

let list_mapi fn l = list_foldi (fun accu i x -> fn i x :: accu) [] l

let map_strings sep fn sl = String.concat sep (List.map fn sl)

let map_stringsi sep fn sl = String.concat sep (list_mapi fn sl)

(* List version of Array.iteri *)
let list_iteri fn =
	let p = ref 0 in
	List.iter (fun x -> fn !p x; incr p)

let string_of_data = function
	| Data.NULL    -> "NULL"
	| Data.NONE    -> "NONE"
	| Data.INT i   -> Int64.to_string i
	| Data.TEXT t  -> t
	| Data.FLOAT f -> string_of_float f
	| Data.BLOB _  -> "<blob>"

module Name = struct
	let default = "val"
	let tuple n i = sprintf "%s__%i" n (i+1)
	let sum n r i = if n = "" then sprintf "%s__%i" r (i+1) else sprintf "%s__%s__%i" n r (i+1)
	let dict n f = if n = "" then f else sprintf "%s__%s" n f
	let option n = sprintf "%s__0" n
	let option_is_set n = sprintf "%s__0__isset" n
	let enum n = sprintf "%s__0" n
end

exception Process_error of Type.t * string
let process_error t s =
	Printf.printf "ERROR(%s): %s\n%!" (Type.to_string t) s;
	raise (Process_error (t, s))

let with_valid_type fn t =
	let module T = Type in
	match t with
		| T.Ext (v, t) | T.Rec (v, t) -> fn v t
		| _                           -> process_error t "This is not a well-formed type"

let is_enum = function
	| Type.Enum _ -> true
	| _           -> false

let get_enum_type = function
	| Type.Enum t -> t
	| t           -> process_error t "This is not a enum type"

let get_internal_type = with_valid_type (fun name t -> t)

let nb_of_qmarks str =
	let q = ref 0 in
	for i = 0 to String.length str - 1 do
		if str.[i] = '?' then incr q
	done;
	!q

let exec_sql ~tag ~env ~db sql binds fn =
	let name = Printf.sprintf "%s:%d" db.name db.uuid in
	debug name env `Sql tag sql;
	if (nb_of_qmarks sql <> List.length binds) then
		failwith (Printf.sprintf "Wrong number of ? in '%s'" sql);
	let stmt = prepare db.db sql in
	list_iteri (fun i v ->
		debug name env `Bind tag (string_of_data v);
		db_must_bind db stmt (i+1) v
		) binds;
	fn stmt

(* Build up the list of fields from a Type.t *)
let field_names_of_type ~id t =
	let module T = Type in
	let rec aux name = function
		| T.Unit | T.Int _ | T.Char | T.Bool | T.String | T.Float | T.Var _ | T.Rec _ | T.Ext _ | T.Enum _ | T.Arrow _ ->
			[ if name = "" then Name.default else name ]
		| T.Option t -> Name.option_is_set name :: aux (Name.option name) t
		| T.Tuple tl -> list_foldi (fun accu i t -> accu @ aux (Name.tuple name i) t) [] tl
		| T.Dict tl  -> List.fold_left (fun accu (n,_,t) -> accu @ aux (Name.dict name n) t) [] tl
		| T.Sum tl   -> 
			"__row__" :: List.fold_left 
				(fun accu (r,tl) -> list_foldi (fun accu i t -> accu @ aux (Name.sum name r i) t) accu tl)
				[] tl in
	if id then "__id__" :: aux "" t else aux "" t

(* Build up the list of field types from a Type.t *)
let field_types_of_type ~id t =
	let module T = Type in
	let rec aux = function
		| T.Unit | T.Int _ | T.Char | T.Bool
		| T.Var _ | T.Rec _ | T.Ext _ | T.Enum _ -> [ "INTEGER" ]
		| T.Float    -> [ "FLOAT" ]
		| T.String   -> [ "STRING" ]
		| T.Arrow _  -> [ "BLOB" ]
		| T.Tuple tl -> List.fold_left (fun accu t -> accu @ aux t) [] tl
		| T.Dict tl  -> List.fold_left (fun accu (_,_,t) -> accu @ aux t) [] tl
		| T.Sum tl   -> "TEXT" :: List.fold_left (fun accu (_,tl) -> accu @ List.fold_left (fun accu t -> accu @ aux t) [] tl) [] tl
		| T.Option t -> "INTEGER" :: aux t in
	if id then "INTEGER" :: aux t else aux t

type table = [ `Enum | `Foreign ]

(* Return the sub-tables for a Type.t and the links between them *)
let subtables_of_type t =
	let module T = Type in
	let default f = if f = "" then Name.default else f in
	let (>>) (l1, l2) (l3, l4) = ( l1 @ l3, l2 @ l4 ) in
	let rec aux ?parent ~field name ((tables,_) as accu) = function
		| T.Unit | T.Int _ | T.Char | T.Bool
		| T.Float | T.String | T.Arrow _ -> accu
		| T.Option t  -> aux ?parent ~field:(Name.option field) (Name.option name) accu t
		| T.Tuple tl  -> list_foldi (fun accu i t -> aux ?parent ?field:(Name.tuple field i) (Name.tuple name i) accu t) accu tl
		| T.Dict tl   -> List.fold_left (fun accu (n,_,t) -> aux ?parent ?field:(Name.dict field n) (Name.dict name n) accu t) accu tl
		| T.Sum tl    ->
			  List.fold_left
				  (fun accu (r,tl) -> list_foldi (fun accu i t -> aux ?parent ?field:(Name.sum field r i) (Name.sum name r i) accu t) accu tl)
				  accu tl
		| T.Var v     -> ( [], match parent with Some p -> [p, default field, `Foreign, v] | _ -> [] ) >> accu
		| T.Rec (v,s)
		| T.Ext (v,s) as t ->
			let res = ( [v, Type.unroll tables t], match parent with Some p -> [p, default field, `Foreign, v] | _ -> [] ) in
			if List.mem_assoc v tables then accu else aux ~parent:v ~field:"" v (res >> accu) s
		| T.Enum s    as t ->
			let name = Name.enum name in
			let res = ( [name, Type.unroll tables t], match parent with Some p -> [p, default field, `Enum, name] | _ -> [] ) in
			res >> (aux ~parent:name ~field:"" name accu s) in
	aux ~field:"" "" ([], []) t

let enum_subtables_of_type t =
	let _, links = subtables_of_type t in
	List.filter (function (_, _, `Enum, _) -> true | _ -> false) links

(* Build up the list of fields from a Value.t *)
let field_names_of_value ~id v =
	let module V = Value in
	let rec aux name = function
		| V.Unit | V.Int _ | V.String _ | V.Bool _ | V.Float _ | V.Var _ | V.Rec _ | V.Ext _ | V.Enum _ | V.Arrow _ -> 
			[ if name = "" then Name.default else name ]
		| V.Null       -> [ Name.option_is_set name ]
		| V.Value v    -> Name.option_is_set name :: aux (Name.option name) v
		| V.Tuple vs   -> list_foldi (fun accu i v -> accu @ aux (Name.tuple name i) v) [] vs
		| V.Dict vs    -> List.fold_left (fun accu (n,v) -> accu @ aux (Name.dict name n) v) [] vs
		| V.Sum (r,vs) -> "__row__" :: list_foldi (fun accu i v -> accu @ aux (Name.sum name r i) v) [] vs in
	if id then "__id__" :: aux "" v else aux "" v

let subtables_of_value v =
	let module V = Value in
	let rec aux name accu = function
		| V.Unit | V.Int _ | V.String _ | V.Bool _ | V.Float _ | V.Arrow _ | V.Null | V.Var _ -> accu
		| V.Value v      -> aux (Name.option name) accu v
		| V.Tuple vs     -> list_foldi (fun accu i v -> aux (Name.tuple name i) accu v) accu vs
		| V.Dict vs      -> List.fold_left (fun accu (n,v) -> aux (Name.dict name n) accu v) accu vs
		| V.Sum (r,vs)   -> list_foldi (fun accu i v -> aux (Name.sum name r i) accu v) accu vs
		| V.Ext((n,i),v)
		| V.Rec((n,i),v) -> aux n ((n,[v]) :: accu) v
		| V.Enum vs      -> let name = Name.enum name in List.fold_left (aux name) ((name,vs) :: accu) vs in
	aux "" [] v

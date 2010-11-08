(*
 * Copyright (c) 2009 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2009 Thomas Gazagnaire <thomas@gazagnaire.com>
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

module T = Type
module V = Value

exception Sql_process_error of T.t * Data.t * string

let process_error v d s =
  Printf.printf "ERROR(%s): %s - %s\n%!" s (T.to_string v) (string_of_data d);
  raise (Sql_process_error (v, d, s))

let exec_sql ~env ~db = exec_sql ~tag:"get" ~db ~env

let process ~env ~db ~constraints name field_names fn =
	let where_str = String.concat " AND " (List.map (function | (n,c,None) -> sprintf "%s %s" n c | (n,c,Some _) -> sprintf "%s %s ?" n c) constraints) in
	let where = if where_str = "" then "" else sprintf " WHERE %s" where_str in
	let sql = sprintf "SELECT %s FROM %s%s;" (String.concat "," field_names) name where in
	let binds = List.rev (List.fold_left (function accu -> function (_,_,None) -> accu | (_,_,Some c) -> c :: accu) [] constraints) in
	exec_sql ~env ~db sql binds (fun stmt -> step_map db stmt fn)

let string_of_constraint (name, c) =
	let make name = String.concat "__" name in
	let bool name = function
		| `T  -> make name, "=", Some (Data.INT 1L)
		| `F  -> make name, "=", Some (Data.INT 0L)
	and int_like name conv = function
		| `Eq i  -> make name, "=", Some (conv i)
		| `Neq i -> make name, "!=", Some (conv i)
		| `Le i  -> make name, "<", Some (conv i)
		| `Ge i  -> make name, ">", Some (conv i)
		| `Leq i -> make name, "<=", Some (conv i)
		| `Geq i -> make name, ">=", Some (conv i)
        and direct name conv = function
                | `Eq i  -> make name, "=", Some (conv i)
	and string name = function
		| `Eq s       -> make name, "IS", Some (Data.TEXT s)
		| `Contains s -> make name, "IS", Some (Data.TEXT (sprintf "*%s*" s)) in
	match c with
		| `Bool b    -> bool name b
		| `String s  -> string name s
		| `Float f   -> int_like name (fun f -> Data.FLOAT f) f
		| `Char c    -> int_like name (fun c -> Data.INT (Int64.of_int (Char.code c))) c
		| `Int i     -> int_like name (fun i -> Data.INT (Int64.of_int i)) i
		| `Int32 i   -> int_like name (fun i -> Data.INT (Int64.of_int32 i)) i
		| `Int64 i   -> int_like name (fun i -> Data.INT i) i
                | `Opaque_id i -> direct name (fun i -> Data.INT i) i
		| `Big_int i -> int_like name (fun i -> Data.TEXT (Big_int.string_of_big_int i)) i

(* Build up the list of fields actually needed to save the row *)
let rec parse_row ~env ~db ~skip ~name t row n =
  match t, row.(n) with
  | T.Unit    , Data.INT 0L -> V.Unit, n + 1
  | T.Int _   , Data.INT i
  | T.Char    , Data.INT i   -> V.Int i, n + 1
  | T.Bool    , Data.INT 0L  -> V.Bool false, n + 1
  | T.Bool    , Data.INT 1L  -> V.Bool true, n + 1
  | T.Float   , Data.FLOAT f -> V.Float f, n + 1
  | T.String  , Data.TEXT t  -> V.String t, n + 1
  | T.String  , Data.INT t   -> V.String (Int64.to_string t), n + 1
  | T.String  , Data.FLOAT f -> V.String (string_of_float f), n + 1
  | T.Enum t  , Data.NULL    -> V.Enum [], n + 1
  | T.Enum t  , Data.INT id  -> V.Enum (get_enum_values ~env ~db ~id (Name.enum name) t), n + 1
  | T.Arrow _ , Data.BLOB b  -> V.Arrow b, n + 1
  | T.Option t, Data.INT r   -> let res, j = parse_row ~env ~db ~skip:(r=0L) ~name:(Name.option name) t row (n + 1) in (if r=0L then V.Null else V.Value res), j
  | T.Tuple tl, _            ->
    let tuple, n = list_foldi (fun (accu, n1) i t ->
      let res, n2 = parse_row ~env ~db ~skip ~name:(Name.tuple name i) t row n1 in res :: accu, n2
      ) ([], n) tl in
    V.Tuple (List.rev tuple), n
  | T.Dict tl , _            ->
    let dict, n = List.fold_left (fun (accu, n1) (f,_,t) ->
      let res, n2 = parse_row ~env ~db ~skip ~name:(Name.dict name f) t row n1 in (f, res) :: accu, n2
      ) ([], n) tl in
    V.Dict (List.rev dict), n
  | T.Sum tl  , Data.TEXT r  ->
    let row, n = List.fold_left (fun (accu, n1) (rn, tl) ->
      list_foldi (fun (accu, n2) i t ->
        let res, n3 = parse_row ~skip:(rn<>r) ~env ~db ~name:(Name.sum "" rn i) t row n2 in (if rn<>r then accu else res :: accu), n3
        ) (accu, n1) tl)
      ([], n + 1) tl in
    V.Sum (r, List.rev row), n
  | T.Rec(r, s), Data.INT i  ->
    begin match get_values ~env ~db ~id:i t with
    | [_,v] -> v, n + 1
    | []    -> process_error t row.(n) "No value found"
    | _     -> process_error t row.(n) "Too many values found"
    end
  | T.Ext(e, s), Data.INT i  ->
    begin match get_values ~env ~db ~id:i t with
    | [_,v] -> v, n + 1
    | []    -> process_error t row.(n) "No value found"
    | _     -> process_error t row.(n) "Too many values found"
    end
  | T.Var v   , Data.INT i   -> V.Var (v, i), n + 1
  | _ when skip              -> V.Null, n + 1
  | _                        -> process_error t row.(n) (sprintf "%s: unknown" name)

and get_values ~env ~db ?id ?(constraints=[]) ?custom_fn t =

	let value_of_row name s row =
		let id = match row.(0) with Data.INT i -> i | _ -> failwith "TODO:4" in
		let r, _ = parse_row ~env ~db ~skip:false ~name s row 1 in
		if List.mem (name, id) (V.free_vars r) then
			id, V.Rec ((name,id), r)
		else
			id, V.Ext ((name,id), r) in

	let value_of_stmt name s stmt =
		value_of_row name s (row_data stmt) in

	let _custom = ref None in
	let custom name body fn =
		let custom_name = sprintf "%s_custom" name in
		let custom_str = sprintf "%s(%s)" custom_name (String.concat "," (field_names_of_type ~id:true body)) in
		create_funN db.db custom_name (fun row -> let _,v = value_of_row name body row in if fn v then Data.INT 1L else Data.INT 0L);
		_custom := Some custom_name;
		[ custom_str, "", None ] in

	let make_constraints name s =
		(match id with None -> [] | Some id -> [ "__id__", "=", Some (Data.INT id) ]) @
		(match custom_fn with None -> [] | Some fn -> custom name s fn) @
		List.map string_of_constraint constraints in

	match t with
	| T.Rec (n, s)
	| T.Ext (n, s) ->
		let res = process ~env ~db ~constraints:(make_constraints n s) n (field_names_of_type ~id:true s) (value_of_stmt n s) in
		(match !_custom with None -> () | Some name -> delete_function db.db name);
		res
	| _            -> failwith "TODO"  

and get_enum_values ~env ~db ~id name t =
  let aux stmt =
    let row = row_data stmt in
    let id = match row.(0) with Data.INT i -> i | s -> process_error t s "__id__" in
    let next = match row.(1) with Data.INT i -> Some i | Data.NULL -> None | s -> process_error t s "__next__" in
    let next_chunk = match row.(2) with Data.INT i -> Some i | Data.NULL -> None | s -> process_error t s "__next_chunk__" in
    let size = match row.(3) with Data.INT i -> Int64.to_int i | s -> process_error t s "__size__" in
    let v, _ = parse_row ~env ~db ~skip:false ~name t row 4 in
    id, next, next_chunk, size, v in

  let get_chunk first_id =
    let constraints = [ "__id__", "=", Some (Data.INT first_id) ] in
    let field_names = "__id__" :: "__next__" :: "__next_chunk__" :: "__size__" :: field_names_of_type ~id:false t in
    match process ~env ~db ~constraints name field_names aux with
    | [ _ , None     , None      , _   , v ] -> None, [ v ]
    | [ id, Some next, next_chunk, size, _ ] ->
      let size = min size max_join in
      let rec joints i =
        if i = 0
        then sprintf "%s AS __t0__" name :: joints (i+1)
        else if i = size
        then []
        else sprintf "%s AS __t%i__ ON __t%i__.__next__=__t%i__.__id__" name i (i-1) i :: joints (i+1) in
      let names = field_names_of_type ~id:false t in
      let rec field_names i =
        if i = size
        then []
        else List.map (fun f -> sprintf "__t%i__.%s" i f) names @ field_names (i+1) in
      let table_name = String.concat " JOIN " (joints 0) in
      let constraints = [
        (match next_chunk with
        | None         -> sprintf "__t%i__.__next__" (size-1), "ISNULL", None;
        | Some next_id -> sprintf "__t%i__.__next__" (size-1), "="     , Some (Data.INT next_id));
        "__t0__.__id__","=", Some (Data.INT first_id);
      ] in
      let fn stmt =
        let row = row_data stmt in
        let rec aux n =
          if n >= Array.length row
          then []
          else
            let v, m = parse_row ~env ~db ~skip:false ~name t row n in
            v :: aux m in
        aux 0 in
      begin match process ~env ~db ~constraints table_name (field_names 0) fn with
      | [r] -> next_chunk, r
      | []  -> process_error t Data.NULL "No result found"
      | rs  -> process_error t Data.NULL "Too many results found"
      end
    | l ->
		let aux (id, next, next_chunk, size, v) =
      Printf.sprintf "(%Ld,%s,%s,%i,%s)"
        id
        (match next with None -> "NULL" | Some n -> Int64.to_string n)
        (match next_chunk with None -> "NULL" | Some n -> Int64.to_string n)
        size
        (Value.to_string v) in
		process_error t Data.NULL (Printf.sprintf "get_enum_values{%s}" (String.concat ";" (List.map aux l))) in
  
  let result = ref [[]] in
  let stop = ref false in
  let first_id = ref id in
  while not !stop do
    let n, r = get_chunk !first_id in
    (match n with
    | None   -> stop := true;
    | Some k -> first_id := k);
    result := r :: !result;
  done;
  List.concat (List.rev !result)

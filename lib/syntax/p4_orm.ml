(*pp camlp4orf *)
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

open Camlp4
open PreCast
open Ast

open P4_utils

let init n   = n ^ "_init"
let initRO n = n ^ "_init_read_only"
let save n   = n ^ "_save"
let get n    = n ^ "_get"
let id n     = n ^ "_id"
let delete n = n ^ "_delete"

module Env = struct
  let value = "__value__"
  let create_sig tds =
    let bindings = List.flatten (List.map (fun (_loc, n, _) -> [
      <:ctyp< $lid:P4_weakid.weakid_of n$ : $lid:n$ -> int64 >> ;
      <:ctyp< $lid:P4_weakid.of_weakid n$ : int64 -> $lid:n$ >> ;
      <:ctyp< $lid:P4_weakid.has_weakid n$ : $lid:n$ -> bool >> ;
      <:ctyp< $lid:P4_weakid.rm_weakid n$ : $lid:n$ -> unit >> ;
      <:ctyp< $lid:P4_weakid.set_weakid n$ : $lid:n$ -> int64 -> unit >> ]
      ) (list_of_ctyp_decl tds)) in
    let _loc = loc_of_ctyp tds in
    <:ctyp< {
      $tySem_of_list bindings$;
      $lid:P4_weakid.weakid_of value$ : Value.t -> int64;
      $lid:P4_weakid.of_weakid value$ : int64 -> Value.t;
      $lid:P4_weakid.has_weakid value$ : Value.t -> bool;
      $lid:P4_weakid.set_weakid value$ : Value.t -> int64 -> unit;
     } >>

  let create tds =
    let bindings = List.flatten (List.map (fun (_loc, n, _) -> [
      <:rec_binding< Deps.$lid:P4_weakid.weakid_of n$ = W.$lid:P4_weakid.weakid_of n$ >> ;
      <:rec_binding< Deps.$lid:P4_weakid.of_weakid n$ = W.$lid:P4_weakid.of_weakid n$ >> ;
      <:rec_binding< Deps.$lid:P4_weakid.has_weakid n$ = W.$lid:P4_weakid.has_weakid n$ >> ;
      <:rec_binding< Deps.$lid:P4_weakid.rm_weakid n$ = W.$lid:P4_weakid.rm_weakid n$ >> ;
      <:rec_binding< Deps.$lid:P4_weakid.set_weakid n$ = W.$lid:P4_weakid.set_weakid n$ >> ]
      ) (list_of_ctyp_decl tds)) in
    let _loc = loc_of_ctyp tds in
    <:expr<
      let module W = struct $P4_weakid.gen tds$ end in
      let module WV = struct
        value ( $lid:P4_weakid.weakid_of value$,
                $lid:P4_weakid.of_weakid value$,
                $lid:P4_weakid.has_weakid value$,
                _,
                _,
                $lid:P4_weakid.set_weakid value$ ) =
          Value.$lid:P4_weakid.create_weakid_fns "t"$ ();
      end in
      { 
        $rbSem_of_list bindings$;
        Deps.$lid:P4_weakid.weakid_of value$ = WV.$lid:P4_weakid.weakid_of value$;
        Deps.$lid:P4_weakid.of_weakid value$ = WV.$lid:P4_weakid.of_weakid value$;
        Deps.$lid:P4_weakid.has_weakid value$ = WV.$lid:P4_weakid.has_weakid value$;
        Deps.$lid:P4_weakid.set_weakid value$ = WV.$lid:P4_weakid.set_weakid value$;
      } >>
end

let env_to_env _loc env =
  let sl_of_sl sl = 
    expr_list_of_list _loc (List.map (fun s -> <:expr< $str:s$ >>) sl) in
  let aux = function
  | `Unique l -> <:expr< `Unique $expr_list_of_list _loc (List.map (fun (x,y) -> <:expr< ($str:x$, $sl_of_sl y$) >>) l)$ >>
  | `Index l  -> <:expr< `Index $expr_list_of_list _loc (List.map (fun (x,y) -> <:expr< ($str:x$, $sl_of_sl y$) >>) l)$ >>
  | `Debug l  -> <:expr< `Debug $sl_of_sl l$ >>
  | `Dot f    -> <:expr< `Dot $str:f$ >> in
  expr_list_of_list _loc (List.map aux env)

let init_binding tds (_loc, n, t) =
  <:binding< $lid:init n$ =
    fun db_name ->
      let db = Orm.Sql_backend.new_state $Env.create tds$ db_name in
      let () = Orm.Sql_init.init_tables ~mode:`RW ~env:Deps.env ~db Deps.$lid:P4_type.type_of n$ in
      db
  >>

let initRO_binding tds (_loc, n, t) =
  <:binding< $lid:initRO n$ =
    fun db_name ->
      let db = Orm.Sql_backend.new_state $Env.create tds$ db_name in
      let () = Orm.Sql_init.init_tables ~mode:`RO ~env:Deps.env ~db Deps.$lid:P4_type.type_of n$ in
      db
  >>

let save_binding (_loc, n, t) =
  <:binding< $lid:save n$ =
    if not (Type.is_mutable Deps.$lid:P4_type.type_of n$) then (
      fun ~db -> fun $lid:n$ ->
        if not (db.OS.cache.Deps.$lid:P4_weakid.has_weakid n$ $lid:n$) then (
          let id = Orm.Sql_save.save_value ~env:Deps.env ~db (Deps.$lid:P4_value.value_of n$ $lid:n$) in
          db.OS.cache.Deps.$lid:P4_weakid.set_weakid n$ $lid:n$ id )
        else ()
    ) else (
      fun ~db -> fun $lid:n$ ->
        let id = Orm.Sql_save.save_value ~env:Deps.env ~db (Deps.$lid:P4_value.value_of n$ $lid:n$) in
        db.OS.cache.Deps.$lid:P4_weakid.set_weakid n$ $lid:n$ id
    )
  >> 

module Get = struct
  (* This type is computed at preprocessing time, so no information is available on external types *)
  let pp_type_of _loc tds name =
    let tys = P4_type.create tds in
    let _, _, t = List.find (fun (_, n, _) -> n = name) tys in
    t

  let map_type fn t =
    let module T = Type in
    let rec aux name accu t =
      (match fn name t with None -> [] | Some r -> [r]) @ match t with
      | T.Unit | T.Sum _ | T.Var _ | T.Arrow _
      | T.Bool | T.Float | T.Char | T.String | T.Int _ | T.Option _ | T.Enum _
                  -> accu
      | T.Dict d when name = []
                  -> List.fold_left (fun accu (n,_,t) -> aux [n] accu t) accu d
      | T.Dict _  -> accu
      | T.Tuple t when name = []
                  -> fst (List.fold_left (fun (accu, i) t -> aux ["val"; string_of_int i] accu t, i+1) (accu, 1) t)
      | T.Tuple t -> fst (List.fold_left (fun (accu, i) t -> aux (name @ [string_of_int i]) accu t, i+1) (accu, 1) t)
      | T.Rec (n,t) | T.Ext (n,t) when name = []
                  -> aux [] accu t
      | T.Rec (n,t) | T.Ext (n,t)
                  -> accu in
    aux [] [] t

  let arg_names_of_type t =
    let module T = Type in
    let fn name = function
      | T.Bool | T.Float | T.Char | T.String | T.Int _  -> Some (if name = [] then "val" else String.concat "_" name)
      | _ -> None in
    map_type fn t

  let fun_of_type _loc t body =
    List.fold_left (fun accu n -> <:expr< fun ? $lid:n$ -> $accu$ >>) body (arg_names_of_type t)

  let ctyp_of_arg _loc t =
    let module T = Type in
    let int_like t =
      <:ctyp< [= `Eq of $lid:t$ | `Neq of $lid:t$ | `Le of $lid:t$ | `Ge of $lid:t$ | `Leq of $lid:t$ | `Geq of $lid:t$ ] >> in
    let fn _ = function
    | T.Bool     -> Some (<:ctyp< [= `True | `False ] >>)
    | T.Float    -> Some (int_like "float")
    | T.Char     -> Some (int_like "char")
    | T.String   -> Some (<:ctyp< [=`Eq of string | `Contains of string ] >>)
    | T.Int (Some i) when i + 1 = Sys.word_size
                 -> Some (int_like "int")
    | T.Int (Some i) when i <= 32
                 -> Some (int_like "int32")
    | T.Int (Some i) when i <= 64
                 -> Some (int_like "int64")
    | T.Int _    -> Some (int_like "Big_int.big_int")
    | _          -> None in
    map_type fn t

  let sig_of_type _loc t body =
    List.fold_left2
      (fun accu n ctyp -> <:ctyp< ? $lid:n$ : $ctyp$ -> $accu$ >> )
      body
      (arg_names_of_type t)
      (ctyp_of_arg _loc t)

  let fun_of_name _loc tds n body =
    let t = pp_type_of _loc tds n in
    fun_of_type _loc t body

  let sig_of_name _loc tds n body =
    let t = pp_type_of _loc tds n in
    sig_of_type _loc t body

  let constraints_of_args _loc tds n =
    let t = pp_type_of _loc tds n in
    let make name str =
		let name_str = match name with [] -> "val" | l -> String.concat "_" l in
		let name_lst = expr_list_of_list _loc (List.map (fun s -> <:expr< $str:s$ >>) name) in
		<:expr< match $lid:name_str$ with [ None -> [] | Some x -> [ ($name_lst$, ` $uid:str$ x) ] ] >> in
    let module T = Type in
    let fn name = function
      | T.Bool     -> Some (make name "Bool")
      | T.Float    -> Some (make name "Float")
      | T.Char     -> Some (make name "Char")
      | T.Int (Some i) when i + 1 = Sys.word_size
                   -> Some (make name "Int")
      | T.Int (Some i) when i <= 32
                   -> Some (make name "Int32")
      | T.Int (Some i) when i <= 64
                   -> Some (make name "Int64")
      | T.Int _    -> Some (make name "Big_int")
      | _          -> None in
    List.fold_left (fun accu expr -> <:expr< $expr$ @ $accu$ >>) <:expr< [] >> (map_type fn t)
  
end

let get_binding tds (_loc, n, t) =
  <:binding< $lid:get n$ = $Get.fun_of_name _loc tds n <:expr<
    let constraints = $Get.constraints_of_args _loc tds n$ in
    if Type.is_mutable Deps.$lid:P4_type.type_of n$ then (
      fun db ->
        List.map
         (fun (id,v) ->
           let aux () =
             let $lid:n$ = Deps.$lid:P4_value.of_value n$ v in
             do { db.OS.cache.Deps.$lid:P4_weakid.set_weakid n$ $lid:n$ id; $lid:n$ } in
           try
             let t = db.OS.cache.Deps.$lid:P4_weakid.of_weakid n$ id in
             if Value.equal (Deps.$lid:P4_value.value_of n$ t) v then t else aux ()
           with [ Not_found -> aux () ]
         ) (Orm.Sql_get.get_values ~env:Deps.env ~db Deps.$lid:P4_type.type_of n$)
    ) else (
      fun db ->
        List.map
          (fun (id,v) ->
            try db.OS.cache.Deps.$lid:P4_weakid.of_weakid n$ id
            with [ Not_found ->
              let $lid:n$ = Deps.$lid:P4_value.of_value n$ v in
              do { db.OS.cache.Deps.$lid:P4_weakid.set_weakid n$ $lid:n$ id; $lid:n$ } ])
          (Orm.Sql_get.get_values ~env:Deps.env ~db ~constraints Deps.$lid:P4_type.type_of n$)
    ) >>$
  >>

let delete_binding (_loc, n, t) =
  <:binding< $lid:delete n$ =
    fun ~db -> fun $lid:n$ ->
      let id = db.OS.cache.Deps.$lid:P4_weakid.weakid_of n$ $lid:n$ in
      let () = db.OS.cache.Deps.$lid:P4_weakid.rm_weakid n$ $lid:n$ in
      Orm.Sql_delete.delete_value ~env:Deps.env ~db ~id (Deps.$lid:P4_value.value_of n$ $lid:n$)
  >>

let id_binding (_loc, n, t) =
  <:binding< $lid:id n$ : ~db:(db $lid:n$ [<`RW|`RO]) -> $lid:n$ -> int64 =
    fun ~db -> fun $lid:n$ ->
      db.OS.cache.Deps.$lid:P4_weakid.weakid_of n$ $lid:n$
  >>

let gen env tds =
  let _loc = loc_of_ctyp tds in

  let ts = list_of_ctyp_decl tds in
  let init_bindings = List.map (init_binding tds) ts in
  let initRO_bindings = List.map (initRO_binding tds) ts in
  let save_bindings = List.map save_binding ts in
  let get_bindings = List.map (get_binding tds) ts in
  let delete_bindings = List.map delete_binding ts in
  let id_bindings = List.map id_binding ts in

  let sigs =
    List.map (fun (_,n,_) -> <:sig_item< value $lid:init n$ : string -> db $lid:n$ [=`RW] >>) ts @
    List.map (fun (_,n,_) -> <:sig_item< value $lid:initRO n$ : string -> db $lid:n$ [=`RO] >>) ts @
    List.map (fun (_,n,_) -> <:sig_item< value $lid:save n$ : ~db:(db $lid:n$ [=`RW]) -> $lid:n$ -> unit >>) ts @
    List.map (fun (_,n,_) -> <:sig_item< value $lid:get n$ :
      $Get.sig_of_name _loc tds n <:ctyp< (db $lid:n$ [<`RW|`RO]) -> list $lid:n$ >>$ >>) ts @
    List.map (fun (_,n,_) -> <:sig_item< value $lid:delete n$ : ~db:(db $lid:n$ [=`RW]) -> $lid:n$ -> unit >>) ts @
    List.map (fun (_,n,_) -> <:sig_item< value $lid:id n$ : ~db:(db $lid:n$ [<`RW|`RO]) -> $lid:n$ -> int64 >>) ts in

  <:str_item<
    module Internal : sig
      type db 'a 'b;
      $sgSem_of_list sigs$;
    end = struct
      module OS = Orm.Sql_backend;
      module Deps = struct
        $P4_type.gen tds$;
        $P4_value.gen tds$;
        type env = $Env.create_sig tds$;
        value env = $env_to_env _loc env$;
      end;
      type db 'a 'b = OS.state Deps.env;
      value $biAnd_of_list init_bindings$;
      value $biAnd_of_list initRO_bindings$;
      value rec $biAnd_of_list save_bindings$;
      value rec $biAnd_of_list get_bindings$;
      value $biAnd_of_list id_bindings$;
      value $biAnd_of_list delete_bindings$;
    end;
    include Internal
  >>

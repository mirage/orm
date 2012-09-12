(*pp camlp4orf *)
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

open Camlp4
open PreCast
open Ast

open P4_utils

open Pa_dyntype

let init n   = n ^ "_init"
let initRO n = n ^ "_init_read_only"
let save n   = n ^ "_save"
let get n    = n ^ "_get"
let delete n = n ^ "_delete"
let id n     = n ^ "_id"
let cache n  = n ^ "_cache"

let env_to_env _loc env =
    let sl_of_sl sl = 
        expr_list_of_list _loc (List.map (fun s -> <:expr< $str:s$ >>) sl) in
    let aux = function
        | `Unique l -> <:expr< `Unique $expr_list_of_list _loc (List.map (fun (x,y) -> <:expr< ($str:x$, $sl_of_sl y$) >>) l)$ >>
        | `Index l  -> <:expr< `Index $expr_list_of_list _loc (List.map (fun (x,y) -> <:expr< ($str:x$, $sl_of_sl y$) >>) l)$ >>
        | `Debug l  -> <:expr< `Debug $sl_of_sl l$ >>
        | `Dot f    -> <:expr< `Dot $str:f$ >> 
        | `Mode _   -> assert false in
    expr_list_of_list _loc (List.map aux env)

let init_binding env tds (_loc, n, t) =
    <:binding< $lid:init n$ __name__ : Orm.Ae_db.t $lid:n$ [=`RW] =
        let __db__ = Orm.Appengine_backend.new_state __name__ in
        Orm.Ae_db.of_state __db__
    >>

let initRO_binding env tds (_loc, n, t) =
    <:binding< $lid:initRO n$ __name__ : Orm.Ae_db.t $lid:n$ [=`RO] =
        let __db__ = Orm.Appengine_backend.new_state __name__ in
        Orm.Ae_db.of_state __db__
    >>

let save_binding env tds (_loc, n, t) =
    <:binding< $lid:save n$ __db__ $lid:n$ =
        let __db__ = Orm.Ae_db.to_state __db__ in
        let __ty__ = $lid:P4_type.type_of n$ in
        let __val__ = $lid:P4_value.value_of n$ $lid:n$ in 
        Orm.Appengine_save.update_value ~env:__env__ ~db:__db__ __ty__ __val__
    >>

let get_binding env tds (_loc, n, t) =
        <:binding< $lid:get n$ =
            $P4_orm_sqlite.Get.fun_of_name _loc tds n <:expr<
                fun ?custom: (__custom_fn__) ->
                    fun (__db__ : Orm.Ae_db.t $lid:n$ [<`RO|`RW])  ->
                        let __db__ = Orm.Ae_db.to_state __db__ in
                        let __constraints__ = $P4_orm_sqlite.Get.constraints_of_args _loc tds n$ in
                        let __custom_fn__ = match __custom_fn__ with [
                            None    -> None
                          | Some fn -> Some (fun __v__ -> fn ($lid:P4_value.of_value n$ __v__))
                          ] in
                        List.map
                          (fun (__id__, __v__) -> $lid:P4_value.of_value n$ __v__) 
                              (Orm.Appengine_get.get_values ~env:__env__ ~db:__db__ ~constraints:__constraints__ ?custom_fn:__custom_fn__ $lid:P4_type.type_of n$)

                >>$
        >>

let gen env tds =
    let _loc = loc_of_ctyp tds in

    let ts = list_of_ctyp_decl tds in
    let init_bindings = List.map (init_binding env tds) ts in
    let initRO_bindings = List.map (initRO_binding env tds) ts in
    let save_bindings = List.map (save_binding env tds) ts in
    let get_bindings = List.map (get_binding env tds) ts in
    <:str_item<
        $P4_hash.gen tds$;
        $P4_type.gen tds$;
        $P4_value.gen tds$;

        value __env__ : Orm.Appengine_backend.env = $env_to_env _loc env$;
        value $biAnd_of_list init_bindings$;
        value $biAnd_of_list initRO_bindings$;
        value rec $biAnd_of_list save_bindings$;
        value rec $biAnd_of_list get_bindings$;
    >>

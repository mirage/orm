The ORM library provides a storage backend to persist ML values. This backend is integrated seamlessly with OCaml and currently uses SQLite (although other backends are easily possible). The user does not have to worry about writing any SQL queries manually.

Installation
============

You can download the latest distribution from Github at <http://github.com/mirage/orm>.  It also depends on the following libraries:

* `dyntype` : available from <http://github.com/mirage/dyntype>

* `ocaml-sqlite3`: version 1.5.7+, available from <http://www.ocaml.info/home/ocaml_sources.html>. Earlier versions had crash bugs which are easily triggered by the ORM library, so please ensure you are up-to-date before reporting bugs.

* `type-conv`: available from <http://www.ocaml.info/home/ocaml_sources.html>

The library installs an ocamlfind META file, so use it with the `orm.syntax` package.  To compile a file `foo.ml` with the ORM and findlib, do:

    ocamlfind ocamlopt -syntax camlp4o -package orm.syntax -c t.ml

To link it into a standalone executable:

    ocamlfind ocamlopt -syntax camlp4o -linkpkg -package orm.syntax t.ml

You can report issues using the Github issue tracker at <http://github.com/mirage/orm/issues>, or mail the authors at <mailto:mirage@recoil.org>.  If you use the ORM somewhere, feel free to drop us a short line and we can add your project to the Wiki as well.

Usage
=====

For each type definition `t` annotated with the keyword `orm`, a tuple of functions to persist and query the saved values are automatically generated:

    (* User-defined datatype *)
    type t = ... with orm

    (* Auto-generated signatures *)
    val t_init: string -> (t, [ `RW ]) db
    val t_init_read_only: string -> (t, [ `RO ]) db
    val t_get: (t, [< `RW | `RO ]) db -> ... -> t list
    val t_save: (t, [ `RW ]) db -> t -> unit
    val t_delete: (t, [ `RW ]) db -> t -> unit

Example
-------

This example define a basic ML types corresponding to a photo gallery:

    type image = string
    and gallery = {
        name: string;
        date: float;
        contents: image list;
    } with orm

We hold an `image` as a binary string, and a gallery is a named list of images. First, init functions are generated for both `image` and `gallery`:

    val image_init : string -> (image, [ `RW ]) db
    val gallery_init : string -> (gallery, [ `RW ]) db
    val image_init_read_only : string -> (image, [ `RO ]) db
    val gallery_init_read_only : string -> (gallery, [ `RO ]) db

Query functions are generated with signatures matching the various fields in the record or object, for example:

    val gallery_get : (gallery, [< `RO | `RW ]) db ->
        ?name:[ `Eq string | `Contains string] ->
        ?date:[ `Le float | `Ge float | `Eq float | `Neq float] ->
        ?custom:(gallery -> bool) ->
        gallery list

    let my_pics db = gallery_get ~name:(`Contains "Anil") db
    let my_pics db = gallery_get ~custom:(fun g -> String.lowercase g.name = "anil") db

To use this, you simply pass the database handle and specify any constraints to the optional variables.  More complex functions can be specified using the `custom` function which filters the full result set (as seen in the second example above).

Be aware that custom functions currently disable the query optimizer and force a full scan.  We are investigating ways of exposing relational operations in a future release, and ideas (or even better, patches) are always appreciated.

How It Works
------------

Intuitively, calling `gallery_init` will:

1. use `dyntype.type-of` to translate the type definitions into:

        let type_of_image = Ext ( "image", String )
        let type_of_gallery =
            Ext("gallery", Dict [ 
                ("name", String); ("date", Float) ; ("contents", Enum type_of_image)
        ])

2. use some basic inductive rules to generate the database schema:

        CREATE TABLE image (__id__ INTEGER PRIMARY KEY, image TEXT);
        CREATE TABLE gallery (__id__ INTEGER PRIMARY KEY, gallery__name TEXT, 
            gallery__date REAL, gallery__contents__0 INTEGER);
        CREATE TABLE gallery__contents__0 (__id__ INTEGER PRIMARY KEY,  
            __next__ INTEGER, __size__ INTEGER, gallery__contents__0 INTEGER);

Second, using `dyntype.value`, any value of type `image` or `gallery` can be translated into a value of type `Value.t`. Save functions can be then defined with the signature:

    val image_save : (image, [ `RW ]) db -> image -> unit
    val gallery_save : (gallery, [ 'RW ]) db -> gallery -> unit

Finally, using `Dyntype.type-of`, functions to access the database are generated, with the signature:

    val image_get : (image, [< `RO | `RW ]) db ->
        ?value:[`Contains of string | `Eq of string] ] ->
        ?custom:(image -> bool) ->
        image list

    val gallery_get : (gallery, [< `RO | `RW ]) db ->
        ?name:[ `Eq string | `Contains string] ->
        ?date:[ `Le float | `Ge float | `Eq float | `Neq float] ->
        ?custom:(gallery -> bool) ->
        gallery list

For both types, we are generating:
1. arguments that can be easily translated into an optimized SQL queries;
2. a more general (and thus slow) custom query function directly written in OCaml.

On one hand, (1) is achieved by generating optional labelled arguments with the OCaml type corresponding to what `Dyntype.type_of` generated. This allows the programmer to specify a conjunction of type-safe constraints for his queries. For example, the field `name` is of type string which is associated to the constraint of type `Eq of string | Contains of string`. Values of this type can then be mapped to SQL equality or the `LIKE` operator.

On the other hand, (2) is achieved using a SQLite extension to define custom SQL functions---in our case we register an OCaml callback directly. This is relatively slow as it bypasse the query optimizer, but allows the programmer to define very complex queries.

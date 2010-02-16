ORM is using the `dyntype` library to build an integrated SQL backend to persist ML values. This backend is integrated seamlessly with OCaml: the user does not have to worry about writing SQL queries manually.

For each type definition `t` annotated with the keyword `orm`, a tuple of functions to persist and access the saved values are automatically generated:

    (* User-defined datatype *)
    type t = ... with orm \\

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

We hold an `image` as a binary string, and a gallery is a named list of images. First, initializations functions are generated for both `image` and `gallery`:

    val image_init : string -> (image, [ `RW ]) db
    val gallery_init : string -> (gallery, [ `RW ]) db
    val image_init_read_only : string -> (image, [ `RO ]) db
    val gallery_init_read_only : string -> (gallery, [ `RO ]) db

Intuitively, calling `gallery_init` will:

(i) use `dyntype.type-of` to translate the type definitions into:

    let type_of_image = Ext ( "image", String )
    let type_of_gallery =
	    Ext("gallery", Dict [("name", String); ("date", Float) ; ("contents", Enum type_of_image)])

(ii) use some basic inductive rules to generate the database schema:

    CREATE TABLE image (__id__ INTEGER PRIMARY KEY, image TEXT);
    CREATE TABLE gallery (__id__ INTEGER PRIMARY KEY, gallery__name TEXT, gallery__date REAL, gallery__contents__0 INTEGER);
    CREATE TABLE gallery__contents__0 (__id__ INTEGER PRIMARY KEY,  __next__ INTEGER, __size__ INTEGER, gallery__contents__0 INTEGER);


Second, using `dyntype.value`, any value of type `image` or `gallery` can be translated into a value of type `Value.t`. Using induction rules similar to the compute initialization,saving functions can be then defined, having as signature:

    val image_save : (image, [ `RW ]) db -> image -> unit
    val gallery_save : (gallery, [ 'RW ]) db -> gallery -> unit

Finally, using `Dyntype.type-of`, functions to access the database are generated, with the following signature:

    val image_get : (image, [< `RO | `RW ]) db ->
	    ?value:[`Contains of string | `Eq of string] ] ->
	    ?custom:(image -> bool) ->
	    image list

    val gallery_get : (gallery, [< `RO | `RW ]) db ->
	    ?name:[ `Eq string | `Contains string] ->
	    ?date:[ `Le float | `Ge float | `Eq float | `Neq float] ->
	    ?custom:(gallery -> bool) ->
	    gallery list

For both types, we are generating: (i) arguments that can be easily translated into an optimized SQL queries; and (ii) a more general (and thus slow) custom query function directly written in OCaml.

On one hand, (i) is achieved by generating optional labelled arguments with the OCaml type corresponding to what `Dyntype.type_of` generated. This allows the programmer to specify a conjunction of type-safe constraints for his queries. For example, the field `name` is of type string which is associated to the constraint of type `Eq of string | Contains of string`. Values of this type can then be mapped to SQL equality or the `LIKE` operator.

On the other hand,  (ii) is achieved using a SQLite extension to define custom SQL functions---in our case we register an OCaml callback directly. This is relatively slow as it bypasse the query optimizer, but allows the programmer to define very complex queries.

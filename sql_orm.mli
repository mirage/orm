module Schema : sig
    type field_options = [ `Optional | `Unique | `Index ]

    type table_options = {
        unique : string list list;
     }

    type s
    type g

    val text : ?flags:field_options list -> string -> s
    val blob : ?flags:field_options list -> string -> s
    val real : ?flags:field_options list -> string -> s
    val date : ?flags:field_options list -> string -> s
    val integer : ?flags:field_options list -> string -> s
    val foreign : ?flags:field_options list -> string -> string -> s
    val foreign_many : ?flags:field_options list -> string -> string -> s
    type collection
    val make : (string * s list * ((string list * string list) list) * table_options) list -> collection
    val default_opts : table_options

  end
val generate : ?debug:bool -> Schema.collection -> string -> unit


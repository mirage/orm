module Schema : sig
    type options = [ `Optional | `Unique | `Index ]
    type s
    type g

    val text : ?flags:options list -> string -> s
    val blob : ?flags:options list -> string -> s
    val real : ?flags:options list -> string -> s
    val date : ?flags:options list -> string -> s
    val integer : ?flags:options list -> string -> s
    val foreign : ?flags:options list -> string -> string -> s
    val foreign_many : ?flags:options list -> string -> string -> s
    type collection
    val make : (string * s list * ((string list * string list) list)) list -> collection
  end
val generate : ?debug:bool -> Schema.collection -> string -> unit


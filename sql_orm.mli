module Schema : sig
    type options = [ `Optional ]
    type s
    val text : ?flags:options list -> string -> s
    val blob : ?flags:options list -> string -> s
    val date : ?flags:options list -> string -> s
    val integer : ?flags:options list -> string -> s
    val foreign : ?flags:options list -> string -> string -> s
    val foreign_many : ?flags:options list -> string -> string -> s
    type collection
    val make : (string * s list) list -> collection
  end
val generate : ?debug:bool -> Schema.collection -> string -> unit


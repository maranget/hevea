val get_base : unit -> string
val set_base : string -> unit

val get : unit -> string
val set : string -> Lexing.lexbuf -> unit
val restore : unit -> unit

val print_pos : unit -> unit

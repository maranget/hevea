type t

val create_buff : unit -> t
val create_chan : out_channel -> t
val create_null : unit -> t

val reset : t -> unit

val put : t -> string -> unit
val put_char : t -> char -> unit

val to_string : t -> string
val to_chan : out_channel -> t -> unit
val copy : t -> t -> unit
val close : t -> unit

val debug : out_channel -> t -> unit

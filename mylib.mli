exception Error of string

val libdir : string
val put_from_lib : string -> (string -> unit) -> unit
val copy_from_lib : string -> unit

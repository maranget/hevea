exception Error of string

val init :
      (bool -> string -> string) ->
        (string -> unit) ->
          (string -> unit) ->
            (string -> unit) -> unit

val get_int : string -> int
val get_bool : string -> bool
val get_length : string -> Length.t

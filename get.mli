open Lexstate

exception Error of string

val init :
  (bool -> string * Lexstate.subst -> string) ->
  ((Lexing.lexbuf -> unit) -> Lexing.lexbuf -> string) ->
  (string -> unit) -> (string -> unit) -> (string -> unit) ->
  (Lexing.lexbuf -> string) ->
  (Lexing.lexbuf -> unit) -> unit

type saved
val check : unit -> saved
val hot : saved -> unit

val get_int : string * subst -> int
val get_bool : string * subst -> bool
val get_length : string * subst  -> Length.t


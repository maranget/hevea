open Lexstate

exception Error of string

val init :
  (string arg -> string) ->
  ((Lexing.lexbuf -> unit) -> Lexing.lexbuf -> string) ->
  (string -> unit) -> (string -> unit) ->
  (Lexing.lexbuf -> string) ->
  (Lexing.lexbuf -> unit) -> unit

type saved
val check : unit -> saved
val hot : saved -> unit

val get_int : string arg -> int
val get_bool : string arg -> bool
val get_length : string arg  -> Length.t


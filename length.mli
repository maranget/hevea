exception No
;;

val font : int

type t = Absolute of int | Percent of int

val main: Lexing.lexbuf -> t

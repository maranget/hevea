exception No
;;

val font : int
val chars_per_line : int

type t = Absolute of int | Percent of int

val main: Lexing.lexbuf -> t

val font : int

type t = Char of int | Pixel of int | Percent of int | No of string | Default
val pretty : t -> string

val font : int
val pixel_to_char : int -> int
val char_to_pixel : int -> int
val main: Lexing.lexbuf -> t

exception Error of string

val to_string : Lexeme.token -> string
val next_token : Lexing.lexbuf -> Lexeme.token


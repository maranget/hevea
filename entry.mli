type res = Bang of string | Arobas of string | Bar of string 
| Eof of string


exception Fini

val entry : Lexing.lexbuf -> res
val idx   : Lexing.lexbuf -> string

open Lexeme

type style =
 {tag : tag ; attrs : attrs ; txt : string ; ctxt : string}


type 'a t =
  | Text of string
  | Blanks of string
  | Node of 'a * ('a t) list
  | ONode of string * string * ('a t) list

exception Error of string

type align =
    {hor : string ; vert : string ; wrap : bool ;
      mutable pre : string ; mutable post : string ; width : Length.t option}
type format =
  Align of align
| Inside of string


val border : bool ref

val pretty_format : format -> string
val pretty_formats : format array -> unit


val main :
  (string -> string) -> (string -> int) -> string  -> format array

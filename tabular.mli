exception Error of string

type align =
    {hor : string ; mutable vert : string ; wrap : bool ;
      mutable pre : string ; mutable post : string ; width : Length.t}
type format =
    Align of align
  | Inside of string
  | Border of string

val border : bool ref

val pretty_format : format -> string
val pretty_formats : format array -> unit


val main : string  Lexstate.arg -> format array

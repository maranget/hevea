exception Error of string

type align =
    {hor : string ; vert : string ; wrap : bool ;
      mutable pre : string ; mutable post : string ; width : Length.t option}
type format =
    Align of align
  | Inside of string
  | Border of string

val border : bool ref

val pretty_format : format -> string
val pretty_formats : format array -> unit


val init : (string -> string) (* -> (string -> int)*) -> unit
val main : string  -> format array

exception Error of string
val set_verbose : bool -> int -> unit
val seen_par : bool ref

type align =
    {hor : string ; vert : string ; wrap : bool ;
      mutable pre : string ; mutable post : string ; width : Length.t option}
type format =
  Align of align
| Inside of string


val border : bool ref
exception Eof
exception NoOpt
val get_echo : unit -> string
val start_echo : unit -> unit
val opt : Lexing.lexbuf -> string
val arg : Lexing.lexbuf -> string
val csname : Lexing.lexbuf -> string
val cite_arg : Lexing.lexbuf -> string list
val macro_names : Lexing.lexbuf -> string list
val num_arg : Lexing.lexbuf -> int
val skip_equal : Lexing.lexbuf -> unit
val input_arg : Lexing.lexbuf -> string
val tformat : Lexing.lexbuf -> format list
val get_sup_sub : Lexing.lexbuf -> string * string
val get_sup : Lexing.lexbuf -> string
val get_sub : Lexing.lexbuf -> string
val defargs : Lexing.lexbuf -> string list
val tagout : Lexing.lexbuf -> string



module type S =
  sig
  val get_this_arg :
        (Lexing.lexbuf -> unit) -> string * Lexstate.subst -> string
      val get_this_nostyle_arg :
        (Lexing.lexbuf -> unit) -> string * Lexstate.subst -> string
      val get_this : (Lexing.lexbuf -> unit) -> string -> string
      val get_this_nostyle :
        (Lexing.lexbuf -> unit) -> string -> string
      val get_this_clearstyle :
        (Lexing.lexbuf -> unit) -> string -> string
  end

module Make (Dest:OutManager.S) : S

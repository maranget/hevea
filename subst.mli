val do_subst_this : string * Lexstate.subst -> string
val subst_this : string -> string
val subst_arg : Lexing.lexbuf -> string
val subst_opt : string -> Lexing.lexbuf -> string
val subst_csname : Lexing.lexbuf -> string
val subst_body : Lexing.lexbuf -> string

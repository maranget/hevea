exception Error of string
exception Delim of string
val empty_buffs : unit -> unit
val set_verbose : bool -> int -> unit
val seen_par : bool ref

exception Eof
exception NoOpt
val get_echo : unit -> string
val start_echo : unit -> unit
val opt : Lexing.lexbuf -> string
val arg : Lexing.lexbuf -> string
val arg_verbatim : Lexing.lexbuf -> string
val csname : Lexing.lexbuf ->
  (string -> string) -> (string -> string) -> string
val incsname : Lexing.lexbuf -> string
val cite_arg : Lexing.lexbuf -> string list
val rest : Lexing.lexbuf -> string
val macro_names : Lexing.lexbuf -> string list
val num_arg : Lexing.lexbuf -> (string -> int) -> int
val skip_equal : Lexing.lexbuf -> unit
val check_equal : Lexing.lexbuf -> bool
val filename : Lexing.lexbuf -> string
val get_sup_sub : Lexing.lexbuf -> string * string
val get_sup : Lexing.lexbuf -> string
val get_sub : Lexing.lexbuf -> string
val defargs : Lexing.lexbuf -> string list
val get_defargs : Lexing.lexbuf -> string
val tagout : Lexing.lexbuf -> string
val checklimits : Lexing.lexbuf -> bool
val skip_delim : string -> Lexing.lexbuf -> unit
val with_delim : string -> Lexing.lexbuf -> string
val skip_blanks_init : Lexing.lexbuf -> unit

(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

val if_next_char : char -> Lexing.lexbuf  -> bool
val if_next_string : string -> Lexing.lexbuf -> bool

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
val num_arg : Lexing.lexbuf -> (string -> int) -> int
val skip_equal : Lexing.lexbuf -> unit
val check_equal : Lexing.lexbuf -> bool
val filename : Lexing.lexbuf -> string
val remain : Lexing.lexbuf -> string
(* Superscript and subscripts *)
val get_limits : Lexing.lexbuf -> Misc.limits option
val get_sup : Lexing.lexbuf -> string option
val get_sub : Lexing.lexbuf -> string option

val defargs : Lexing.lexbuf -> string list
val get_defargs : Lexing.lexbuf -> string
val tagout : Lexing.lexbuf -> string
val checklimits : Lexing.lexbuf -> bool
val skip_delim : string -> Lexing.lexbuf -> unit
val with_delim : string -> Lexing.lexbuf -> string
val skip_blanks_init : Lexing.lexbuf -> unit

val xy_arg : Lexing.lexbuf -> string

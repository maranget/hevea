(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

val to_string : Lexeme.token -> string
val cost : Lexeme.style -> int * int

module Make(C:DoOut.Config) : sig
  val ptop : unit -> unit
  val reset : unit -> unit
  val next_token : Lexing.lexbuf -> Lexeme.token
  val styles : Lexing.lexbuf -> Css.id list
  val classes : Lexing.lexbuf -> Emisc.Strings.t
end

(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: htmllex.mli,v 1.3 2001-05-25 12:37:23 maranget Exp $          *)
(***********************************************************************)
exception Error of string

val to_string : Lexeme.token -> string
val next_token : Lexing.lexbuf -> Lexeme.token


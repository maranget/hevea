(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: htmlparse.mli,v 1.4 2001-05-28 17:28:55 maranget Exp $        *)
(***********************************************************************)
exception Error of string

val reset : unit -> unit
val main : Lexing.lexbuf -> Lexeme.style Tree.t list


(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: htmlparse.mli,v 1.2 2001-05-25 09:20:46 maranget Exp $"            *)
(***********************************************************************)
exception Error of string

val main : Lexing.lexbuf -> Tree.style Tree.t list


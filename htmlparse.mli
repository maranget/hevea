(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: htmlparse.mli,v 1.3 2001-05-25 12:37:24 maranget Exp $        *)
(***********************************************************************)
exception Error of string

val main : Lexing.lexbuf -> Tree.style Tree.t list


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

type res = Bang of string | Arobas of string | Bar of string 
| Eof of string


exception Fini

val entry : Lexing.lexbuf -> res
val idx   : Lexing.lexbuf -> string

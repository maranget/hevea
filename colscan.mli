(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: colscan.mli,v 1.5 2001-05-25 12:37:20 maranget Exp $          *)
(***********************************************************************)
exception Error of string
val one : Lexing.lexbuf -> float
val three : Lexing.lexbuf -> float * float * float
val four : Lexing.lexbuf -> float * float * float * float

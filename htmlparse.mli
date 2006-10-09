(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: htmlparse.mli,v 1.5 2006-10-09 08:25:16 maranget Exp $        *)
(***********************************************************************)
exception Error of string

val reset : unit -> unit
val main :
    Emisc.Strings.t option -> Lexing.lexbuf -> Lexeme.style Tree.t list


(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

let header = "$Id: misc.ml,v 1.2 1999-03-03 18:08:48 maranget Exp $" 

exception Fatal of string

let verbose = ref 0
and readverb = ref 0

let silent = ref false

let column_to_command s = "\\@"^s^"@"

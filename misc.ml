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

let header = "$Id: misc.ml,v 1.3 1999-05-07 11:33:56 maranget Exp $" 

exception Fatal of string
exception ScanError of string

let verbose = ref 0
and readverb = ref 0

let silent = ref false

let column_to_command s = "\\@"^s^"@"

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

let header = "$Id: misc.ml,v 1.5 1999-06-04 14:19:52 tessaud Exp $" 

exception Fatal of string
exception ScanError of string
exception EndInput
exception Close of string

let verbose = ref 0
and readverb = ref 0

let silent = ref false

let column_to_command s = "\\@"^s^"@"

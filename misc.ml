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

let header = "$Id: misc.ml,v 1.6 1999-08-16 08:10:14 maranget Exp $" 

exception Fatal of string
exception ScanError of string
exception EndInput
exception Close of string
exception EndOfLispComment of int (* QNC *)

let verbose = ref 0
and readverb = ref 0

let silent = ref false

let column_to_command s = "\\@"^s^"@"

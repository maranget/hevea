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

exception Fatal of string
exception ScanError of string
exception EndInput
exception Close of string

val verbose : int ref
val readverb : int ref
val silent : bool ref
val column_to_command : string -> string

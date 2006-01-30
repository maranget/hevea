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

val setup : string -> string -> unit
val setprev : string -> string -> unit
val setnext : string -> string -> unit
val setprevnext : string -> string -> unit

val next : string -> string
val prev : string -> string
val up   : string -> string

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

val compute : string -> string -> string
val define : string -> string -> string -> unit
val define_named : string -> string -> string -> unit
val retrieve : string -> string
val remove : string -> unit

type saved
val checkpoint : unit -> saved
val hot_start : saved -> unit


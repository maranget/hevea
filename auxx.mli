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

val rset : string -> string -> unit
val rget : string -> string
val bset : string -> string -> unit
val bget : string -> string
val init : string -> unit
val finalize : unit -> unit
val bwrite : string -> string -> unit
val rwrite : string -> string -> unit

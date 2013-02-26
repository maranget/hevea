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
val bget : bool -> string -> string option
val init : string -> unit
val final : string -> unit
val finalize : bool -> bool
val bwrite : string -> string -> unit
val rwrite : string -> string -> unit
val rwrite2 : string -> string -> string -> unit
val swrite : string -> unit

val addtoc : string -> int -> string -> unit

val hot_start : unit -> unit

type saved

val check : unit -> saved
val hot : saved -> unit

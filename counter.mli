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

type saved
val checkpoint : unit -> saved
val hot_start : saved -> unit

val value_counter : string -> int
val def_counter: string -> string -> unit
val set_counter: string -> int -> unit
val add_counter:string -> int -> unit
val step_counter: string -> unit
val number_within: string -> string -> unit

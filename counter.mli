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

type t = int ref * ((int ref) list) ref

val value_counter : string -> int
val def_counter: string -> string -> unit
val set_counter: string -> int -> unit
val add_counter:string -> int -> unit
val step_counter: string -> unit

val setrefvalue: string -> unit
val getrefvalue: unit -> string

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

val newindex : string -> string -> string -> string -> unit
val changename : string -> string -> unit
val treat:  string -> string -> string -> string
val treat_anchor : string -> string -> string -> string -> unit
val print: (string -> unit) -> string -> unit
val finalize : bool -> bool

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

module type T =
  sig
    exception Error of string
    val newindex : string -> string -> string -> unit
    val changename : string -> string -> unit
    val treat: (string -> bool) -> string -> string -> string -> unit
    val print: (string -> unit) -> string -> unit
  end

module Make (Dest : OutManager.S) : T

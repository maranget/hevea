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

type input = File of string | Prog of string

val symbols : bool ref
val iso : bool ref
type language = Francais | English
val language : language  ref
type destination = Html | Text | Info
val destination : destination ref
val mathml : bool ref
val entities : bool ref
val pedantic : bool ref
val fixpoint : bool ref
val width : int ref
val except : string list ref
val path : string list ref

val filter : bool
val styles : input list
val base_in : string
val name_in : string
val base_out : string
val name_out : string

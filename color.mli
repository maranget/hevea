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

(*
"$Id: color.mli,v 1.6 2005-03-08 15:15:03 maranget Exp $" 
*)

type t = Name of string | Hex of string

val compute : string -> string -> t
val define : string -> string -> string -> unit
val define_named : string -> string -> string -> unit
val retrieve : string -> t
val remove : string -> unit

type saved
val checkpoint : unit -> saved
val hot_start : saved -> unit


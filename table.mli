(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

exception Empty
type 'a t

val create : 'a -> 'a t
val reset : 'a t -> unit

val emit : 'a t -> 'a -> unit
val apply : 'a t -> ('a -> unit) -> unit
val trim : 'a t -> 'a array
val to_array : 'a t -> 'a array
val remove_last : 'a t -> unit
val get_size : 'a t -> int

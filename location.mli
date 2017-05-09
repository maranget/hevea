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
val check : unit -> saved
val hot : saved -> unit

val get : unit -> string
val set : string -> Lexing.lexbuf -> unit
val restore : unit -> unit

type t
val get_pos : unit -> t
val get_lineno : unit -> int
val print_pos : unit -> unit
val print_fullpos : unit -> unit
val print_this_pos : t -> unit 
val print_this_fullpos : t -> unit 


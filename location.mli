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

exception Fatal of string
;;
type fileOption = No | Yes of in_channel
;;
val get_base : unit -> string
val set_base : string -> unit

val get : unit -> string
val set : string -> Lexing.lexbuf -> unit
val restore : unit -> unit

val print_pos : unit -> unit
val print_top_pos : unit -> unit
val push_pos : unit -> unit
val pop_pos : unit -> unit

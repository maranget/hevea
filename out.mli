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

type t

val create_buff : unit -> t
val create_chan : out_channel -> t
val create_null : unit -> t
val is_null : t -> bool

val reset : t -> unit
val is_empty: t -> bool

val put : t -> string -> unit
val put_char : t -> char -> unit
val flush: t -> unit

val to_string : t -> string
val to_chan : out_channel -> t -> unit
val copy : t -> t -> unit
val copy_fun : (string -> string) -> t -> t -> unit
val copy_no_tag : t -> t -> unit
val close : t -> unit

val debug : out_channel -> t -> unit

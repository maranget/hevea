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

exception Error of string
exception Close of string
val iso : char -> string

val set_out : Out.t -> unit
val get_last_closed : unit -> string
val set_last_closed : string -> unit
val is_empty : unit -> bool

val get_fontsize : unit -> int
val nostyle : unit -> unit
val clearstyle : unit -> unit
val open_mod : Latexmacros.env -> unit
val erase_mods : Latexmacros.env list -> unit
val open_mods : Latexmacros.env list -> unit
val close_mods : unit -> unit
val par : int option -> unit
val forget_par : unit -> int option


val open_block : string -> string -> unit
val close_flow : string -> unit
val close_block : string -> unit
val force_block : string -> string -> unit
val insert_block : string -> string -> unit

val open_display : string -> unit
val close_display : unit -> unit
val item_display : unit -> unit
val force_item_display : unit -> unit
val end_item_display : unit -> int * (unit -> unit) * bool
val begin_item_display : (unit -> unit) -> bool -> unit
val erase_display : unit -> unit

val set_dt : string -> unit
val set_dcount : string -> unit
val item : (string -> unit) -> string -> unit
val change_block : string -> string -> unit
val erase_block : string -> unit
val open_group : string -> unit
val open_aftergroup : (string -> string) -> unit
val close_group : unit -> unit
val put : string -> unit
val put_char : char -> unit
val flush_out : unit -> unit
val skip_line : unit -> unit


val delay : (int -> unit) -> unit
val flush : int -> unit
val forget : unit -> unit

val loc_ref: string -> string -> unit
val loc_name: string -> string -> unit

val insert_vdisplay: (unit -> unit) -> Latexmacros.env list
val freeze : (unit -> unit) -> unit

val open_chan: out_channel  -> unit
val close_chan: unit -> unit
val to_string: (unit -> unit) -> string
val to_style: (unit -> unit) -> Latexmacros.env list
val get_current_output : unit -> string

val finalize : bool -> unit

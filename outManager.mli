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
open Lexstate
module type S = sig
exception Error of string

val set_out : Out.t -> unit
val stop : unit -> unit
val restart : unit -> unit
val is_empty : unit -> bool
    
val get_fontsize : unit -> int
val nostyle : unit -> unit
val clearstyle : unit -> unit
val open_mod : Element.text -> unit
val erase_mods : Element.text list -> unit
val has_mod : Element.text -> bool
val forget_par : unit -> int option
val close_par : unit -> bool
val open_par : ?attr:string -> unit -> unit
val par : int option -> unit
val open_block : ?force_inline:bool -> string -> string -> unit
val close_block : string -> unit
val force_block : string -> string -> unit
val close_flow : string -> unit
val insert_block : string -> string -> unit
val insert_attr : string -> string -> unit

val open_maths : bool -> unit
val close_maths : bool -> unit
val open_display_varg : string -> unit
val open_display : unit -> unit
val close_display : unit -> unit
val item_display : unit -> unit
val force_item_display : unit -> unit
val erase_display : unit -> unit
val standard_sup_sub :
    (string arg -> unit) -> (unit -> unit) -> string arg -> string arg -> bool -> unit
val limit_sup_sub :
    (string arg -> unit) -> (unit -> unit) -> string arg -> string arg -> bool -> unit
val int_sup_sub :
    bool -> int ->
      (string arg -> unit) -> (unit -> unit) -> string arg -> string arg -> bool -> unit
val addvsize : int -> unit
val over : Lexing.lexbuf -> unit
val left : string -> (int -> unit) -> (int -> unit) -> unit
val right : string -> (int -> unit) -> int

val set_dcount : string -> unit
val item : string-> unit
val nitem : string-> unit
val ditem : (string -> unit) -> string -> string -> string -> unit
val erase_block : string -> unit
val open_group : string -> unit
val open_aftergroup : (string -> string) -> unit
val close_group : unit -> unit
val put : string -> unit
val put_char : char -> unit
val put_unicode : OutUnicode.unichar -> unit
val flush_out : unit -> unit
val skip_line : unit -> unit

val loc_name : string -> unit

val open_chan : out_channel  -> unit
val close_chan : unit -> unit
val to_string : (unit -> unit) -> string
val to_style : (unit -> unit) -> Element.text list
val get_current_output : unit -> string

val finalize : bool -> unit

val horizontal_line : string -> Length.t -> Length.t -> unit
val put_separator : unit -> unit
val unskip : unit -> unit
val put_tag : string -> unit
val put_hspace : bool -> Length.t -> unit
val put_open_group : unit -> unit
val put_close_group : unit -> unit
val put_in_math : string -> unit

val open_table : bool -> string -> unit
val new_row : unit -> unit
val open_cell : Tabular.format -> int -> int -> bool -> unit
val erase_cell : unit -> unit
val close_cell : string -> unit
val do_close_cell : unit -> unit
val open_cell_group : unit -> unit
val close_cell_group : unit -> unit
val erase_cell_group : unit -> unit
val close_row : unit -> unit
val erase_row : unit -> unit
val close_table : unit -> unit
val make_border : string -> unit
val make_inside : string -> bool -> unit
val make_hline : int -> bool -> unit

val infomenu : string -> unit
val infonode : string -> string -> string -> unit
val infoextranode : string -> string -> string -> unit

val image : string -> string -> unit

type saved 
val check : unit -> saved
val hot : saved -> unit
end

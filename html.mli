val set_out : Out.t -> unit
val last_closed : string ref

val get_fontsize : unit -> int
val nostyle : unit -> unit
val clearstyle : unit -> unit
val open_mod : Latexmacros.env -> unit
val erase_mods : Latexmacros.env list -> unit
val open_mods : Latexmacros.env list -> unit
val close_mods : unit -> unit
val par : unit -> unit
val forget_par : unit -> unit
val open_block : string -> string -> unit
val close_flow : string -> unit
val close_block : string -> unit
val force_block : string -> string -> unit

val open_display : string -> unit
val close_display : unit -> unit
val item_display : unit -> unit
val force_item_display : unit -> unit
val end_item_display : unit -> int
val begin_item_display : unit -> unit
val erase_display : unit -> unit

val set_dt : string -> unit
val set_dcount : string -> unit
val item : (string -> unit) -> string -> unit
val change_block : string -> string -> unit
val erase_block : string -> unit
val open_group : string -> unit
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

val finalize : unit -> unit

val verbose : int ref
val set_out : Out.t -> unit


val get_fontsize : unit -> int
val open_mod : Latexmacros.env -> unit
val open_mods : Latexmacros.env list -> unit
val par : unit -> unit
val open_par : unit -> unit
val close_par : unit -> unit
val open_block : string -> string -> unit
val close_flow : string -> unit
val close_block : string -> unit
val force_block : string -> string -> unit

val open_display : string -> unit
val close_display : unit -> unit
val item_display : unit -> unit
val end_item_display : unit -> unit
val begin_item_display : unit -> unit
val erase_display : unit -> unit

val item : (unit -> unit) -> unit
val change_block : string -> string -> unit
val erase_block : string -> unit
val open_group : string -> unit
val close_group : unit -> unit
val put : string -> unit
val put_char : char -> unit
val skip_line : unit -> unit


val delay : (int -> unit) -> unit
val flush : unit -> int
val forget : unit -> unit

val loc_ref: string -> string -> unit
val loc_name: string -> string -> unit

val insert_vdisplay: (unit -> unit) -> Latexmacros.env list
val freeze : (unit -> unit) -> unit

val open_chan: out_channel  -> unit
val close_chan: unit -> unit
val to_string: (unit -> unit) -> string

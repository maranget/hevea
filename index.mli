val verbose : int ref
val set_idx : unit -> unit
val newindex : string -> string -> string -> unit
val treat: string -> Lexing.lexbuf -> unit
val print: (string -> unit) -> string -> unit

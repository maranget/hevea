type 'a t

val create : unit -> 'a t
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a
val top : 'a t -> 'a
val empty : 'a t -> bool
val rev : 'a t -> unit

type 'a r
val save_stack : 'a t -> 'a r
val restore_stack : 'a t -> 'a r -> unit

val prerr_args : unit -> unit
val prerr_stack_string : string -> ('a -> string) -> 'a t -> unit

val scan_arg : (string -> 'a) -> int -> 'a
val scan_body :
    (Latexmacros.action list -> 'a) ->
    Latexmacros.action list -> string array -> 'a

val stack_lexbuf : Lexing.lexbuf t
val eat_space : bool ref
val stack_eat : bool t

val record_lexbuf : Lexing.lexbuf -> bool -> unit
val previous_lexbuf : unit -> Lexing.lexbuf

val save_lexstate : unit -> unit
val restore_lexstate : unit -> unit
val start_lexstate : unit -> unit
val out_file : Out.t ref
val prelude : bool ref
val flushing : bool ref
val stack_in_math : bool t
val stack_display : bool t

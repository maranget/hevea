exception Error of string
exception IfFalse
type 'a t

val create : unit -> 'a t
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a
val top : 'a t -> 'a
val empty : 'a t -> bool
val rev : 'a t -> unit

type 'a r
type closenv
val top_level : unit -> bool
val save_stack : 'a t -> 'a r
val restore_stack : 'a t -> 'a r -> unit

val prerr_args : unit -> unit
val prerr_stack_string : string -> ('a -> string) -> 'a t -> unit

val scan_arg : (string -> 'a) -> int -> 'a
val scan_body : ('b -> 'a) -> 'b -> string array -> 'a
val scan_fun :
  (Lexing.lexbuf -> string -> 'a) -> Lexing.lexbuf -> string -> 'a

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

val start_normal: bool ref -> bool ref -> unit
val end_normal : bool ref -> bool ref  -> unit


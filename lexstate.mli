exception Error of string
exception IfFalse

val out_file : Out.t
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
val pretty_lexbuf : Lexing.lexbuf -> unit

val scan_arg : (string -> 'a) -> int -> 'a
val scan_body :
  (Latexmacros.action -> 'a) -> Latexmacros.action -> string array -> 'a
val scan_fun :
  (Lexing.lexbuf -> string -> 'a) -> Lexing.lexbuf -> string -> 'a

val stack_lexbuf : Lexing.lexbuf t
val eat_space : bool ref
val stack_eat : bool t
val tab_val : int ref

val record_lexbuf : Lexing.lexbuf -> bool -> unit
val previous_lexbuf : unit -> Lexing.lexbuf

val save_lexstate : unit -> unit
val restore_lexstate : unit -> unit
val start_lexstate : unit -> unit
val prelude : bool ref
val flushing : bool ref
val stack_in_math : bool t
val stack_display : bool t

val start_normal: bool ref -> bool ref -> unit
val end_normal : bool ref -> bool ref  -> unit

val save_arg : Lexing.lexbuf -> string
type ok = | No of string | Yes of string
val from_ok : ok -> string
val pretty_ok : ok -> string
val parse_quote_arg_opt : string -> Lexing.lexbuf -> ok
val parse_args_norm : 'a list -> Lexing.lexbuf -> string list
val parse_arg_opt : string -> Lexing.lexbuf -> ok
val parse_args_opt : string list -> Lexing.lexbuf -> ok list
val skip_opt : Lexing.lexbuf -> unit
val check_opt : Lexing.lexbuf -> bool
val save_opt : string -> Lexing.lexbuf -> string
val parse_args :
  string list * 'a list -> Lexing.lexbuf -> ok list * string list
val make_stack : string -> Latexmacros.pat -> Lexing.lexbuf -> string array



val scan_this : (Lexing.lexbuf -> 'a ) -> string -> 'a
val scan_this_may_cont :
    bool -> (Lexing.lexbuf -> 'a ) -> Lexing.lexbuf ->  string -> 'a

val input_file : int -> (Lexing.lexbuf -> unit) -> string -> unit

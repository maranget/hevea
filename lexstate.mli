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

type action =
  | Subst of string
  | CamlCode of (Lexing.lexbuf -> unit)
val pretty_action : action -> unit


type pat = string list * string list
val pretty_pat : pat -> unit

type subst
val get_subst : unit -> subst

exception Error of string

val display : bool ref
val in_math : bool ref
val alltt : bool ref
val french : bool ref
val optarg : bool ref
val styleloaded : bool ref
val activebrace : bool ref
val html : bool ref
val text : bool ref

val is_plain : char -> bool
val set_plain : char -> unit
val unset_plain : char -> unit
val plain_back : bool -> char -> unit

val withinLispComment : bool ref
val afterLispCommentNewlines : int ref

type closenv
val top_level : unit -> bool


val prerr_args : unit -> unit
val pretty_lexbuf : Lexing.lexbuf -> unit

val scan_arg : (string -> 'a) -> int -> 'a
val scan_body :
  (action -> 'a) -> action -> subst -> 'a

val stack_lexbuf : Lexing.lexbuf Stack.t
val tab_val : int ref

val previous_lexbuf : unit -> Lexing.lexbuf

val save_lexstate : unit -> unit
val restore_lexstate : unit -> unit
val start_lexstate : unit -> unit
val prelude : bool ref
val flushing : bool ref
val stack_in_math : bool Stack.t
val stack_display : bool Stack.t
val stack_alltt : bool Stack.t
val stack_closed : string Stack.t

val start_normal: subst -> unit
val end_normal : unit -> unit


type ok = | No of string | Yes of string

val from_ok : ok * subst -> string * subst

val save_arg : Lexing.lexbuf -> string * subst
val save_filename : Lexing.lexbuf -> string * subst
val save_verbatim : Lexing.lexbuf -> string * subst
val save_opt : string -> Lexing.lexbuf -> string * subst
val save_opts : string list -> Lexing.lexbuf -> (ok * subst) list

val pretty_ok : ok -> string
val skip_opt : Lexing.lexbuf -> unit
val skip_csname : Lexing.lexbuf -> unit
(* val parse_args :
  string list * string list -> Lexing.lexbuf -> ok list * string list
*)
val make_stack : string -> pat -> Lexing.lexbuf -> subst



val scan_this : (Lexing.lexbuf -> 'a ) -> string -> 'a
val scan_this_arg : (Lexing.lexbuf -> 'a ) -> (string * subst) -> 'a
val scan_this_may_cont :
    (Lexing.lexbuf -> 'a ) -> Lexing.lexbuf -> subst ->  string * subst -> 'a

val input_file : int -> (Lexing.lexbuf -> unit) -> string -> unit

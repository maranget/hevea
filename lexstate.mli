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
  | Subst of string list
  | Toks of string list
  | CamlCode of (Lexing.lexbuf -> unit)

val pretty_body : out_channel -> string list -> unit
val body_to_string : string list -> string
val pretty_action : action -> unit
val is_empty_list : string list -> bool

type pat = string list * string list
val pretty_pat : pat -> unit
val is_subst : action -> bool
val latex_pat: string list -> int -> pat
val zero_pat : pat
val one_pat : pat

type subst
type 'a arg = {arg : 'a ; subst : subst }
val mkarg : 'a -> subst -> 'a arg
val string_to_arg : 'a -> 'a arg

val top_subst : subst
val get_subst : unit -> subst

exception Error of string
exception SubstTop
type jax = JaxOut | JaxInline | JaxDisplay
  
type alltt = Not | Inside | Macro
val effective : alltt -> bool

val raw_chars : bool ref
val display : bool ref
val in_math : bool ref
val jaxauto : bool ref
val injaxauto : jax ref
val alltt : alltt ref
val whitepre: bool ref
val optarg : bool ref
val styleloaded : bool ref
val activebrace : bool ref
val html : bool ref
val text : bool ref
val alltt_loaded : bool ref
val is_plain : char -> bool
val set_plain : char -> unit
val unset_plain : char -> unit
val plain_back : bool -> char -> unit

val withinLispComment : bool ref
val afterLispCommentNewlines : int ref

type case = Upper | Lower | Neutral

val case : case ref

type closenv
val top_level : unit -> bool
val is_top : subst -> bool

val prerr_args : unit -> unit
val full_pretty_subst : subst -> unit

val pretty_lexbuf : Lexing.lexbuf -> unit


val scan_arg : (string list arg -> 'a) -> int -> 'a
val scan_body : (action -> 'a) -> action -> subst -> 'a

val stack_lexbuf : Lexing.lexbuf MyStack.t
val previous_lexbuf : unit -> Lexing.lexbuf
val record_lexbuf : Lexing.lexbuf -> subst -> unit
val top_lexstate : unit -> bool

(* Saving and restoring lexstates on a stack *)
val protect_save_string : (Lexing.lexbuf -> string) -> Lexing.lexbuf -> string
val save_lexstate : unit -> unit
val restore_lexstate : unit -> unit
val start_lexstate : unit -> unit
val start_lexstate_subst : subst -> unit

(* Total checkpoint of lexstate *)
type saved_lexstate
val check_lexstate : unit -> saved_lexstate
val hot_lexstate : saved_lexstate -> unit

val flushing : bool ref
val stack_in_math : bool MyStack.t
val stack_display : bool MyStack.t
val stack_alltt : alltt MyStack.t

val start_normal: subst -> unit
val end_normal : unit -> unit

(* Super/Sub-script parsing *)
  type sup_sub = {
    limits : Misc.limits option;
    sup : string arg;
    sub : string arg;
  } 

val unoption : string arg option -> string arg
val save_sup_sub : Lexing.lexbuf -> sup_sub
val save_sup : Lexing.lexbuf -> string arg option
val save_sub : Lexing.lexbuf -> string arg option

(* Give next char in entry,
   may raise Not_found, if no next char is available *)
val full_peek_char : Lexing.lexbuf -> char

(* Argument parsing *)

type ok = | No of string | Yes of string list
val from_ok : ok arg -> string list arg

val save_arg : Lexing.lexbuf -> string arg
val save_body : Lexing.lexbuf -> string list arg
val save_filename : Lexing.lexbuf -> string arg
val save_verbatim : Lexing.lexbuf -> string arg 
val save_opt : string -> Lexing.lexbuf -> string list arg
val save_opts : string list -> Lexing.lexbuf -> ok arg list
val save_arg_with_delim : string -> Lexing.lexbuf -> string arg
val save_xy_arg : Lexing.lexbuf -> string arg
val save_cite_arg : Lexing.lexbuf -> (string list) arg


val pretty_ok : ok -> string
val skip_opt : Lexing.lexbuf -> unit
val skip_csname : Lexing.lexbuf -> unit

val make_stack : string -> pat -> Lexing.lexbuf -> subst



val scan_this
 : (Lexing.lexbuf -> 'a ) -> string -> 'a
val scan_this_list : (Lexing.lexbuf -> 'a ) -> string list -> 'a
val scan_this_arg : (Lexing.lexbuf -> 'a ) -> string arg -> 'a
val scan_this_arg_list : (Lexing.lexbuf -> 'a ) -> string list arg -> 'a
val scan_this_may_cont :
    (Lexing.lexbuf -> 'a ) -> Lexing.lexbuf -> subst ->  string arg -> 'a
val scan_this_list_may_cont :
    (Lexing.lexbuf -> 'a ) -> Lexing.lexbuf -> subst ->  string list arg -> 'a

val real_input_file :
    int -> (Lexing.lexbuf -> unit) -> string -> in_channel -> unit
val input_file : int -> (Lexing.lexbuf -> unit) -> string -> unit

val register_cell : string -> bool ref -> unit
val unregister_cell : string -> unit
type saved

val checkpoint : unit -> saved
val hot_start : saved -> unit


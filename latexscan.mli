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

module type S =
  sig
    exception Error of string

    val no_prelude : unit -> unit

    val print_env_pos : unit -> unit
    val main : Lexing.lexbuf -> unit

    (* additional resources needed for extension modules. *)
    val save_arg : Lexing.lexbuf -> string
    val save_opt : string -> Lexing.lexbuf -> string
    val subst : Lexing.lexbuf -> unit
    val subst_arg : (Lexing.lexbuf -> unit) -> Lexing.lexbuf -> string
    val subst_this : (Lexing.lexbuf -> unit) -> string -> string
    val cur_env : string ref
    val new_env : string -> unit
    val close_env : string -> unit
    val env_level : int ref
    val macro_register : string -> unit
    val top_open_block : string -> string -> unit
    val top_close_block : string -> unit
    val scan_this : (Lexing.lexbuf -> 'a ) -> string -> 'a
    val get_this : (Lexing.lexbuf -> unit) -> string -> string
    val get_int : (Lexing.lexbuf -> unit) -> string -> int
    val tab_val : int ref

    val withinLispComment : bool ref
    val afterLispCommentNewlines : int ref
    val withinSnippet : bool ref
end

module Make (Html : OutManager.S) : S


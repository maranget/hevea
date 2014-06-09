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

module type S =
  sig
    (* external entry points *)
    val no_prelude : unit -> unit
    val translate_put_unicode : char -> (unit -> int) -> unit
    val translate_put_unicode_string : string -> unit
    val main : Lexing.lexbuf -> unit
    val expand_command : string -> Lexing.lexbuf -> unit
    val expand_command_no_skip : string -> Lexing.lexbuf -> unit
    val print_env_pos : unit -> unit

    (* additional resources needed for extension modules. *)
    val cur_env : string ref
    val new_env : string -> unit
    val close_env : string -> unit
    val echo_toimage : unit -> bool
    val echo_global_toimage : unit -> bool

    val fun_register : (unit -> unit) -> unit
    val newif_ref : string -> bool ref -> unit
    val top_open_block : string -> string -> unit
    val top_close_block : string -> unit
    val top_open_group : unit -> unit
    val top_close_group : unit -> unit
    val check_alltt_skip : Lexing.lexbuf -> unit
    val skip_pop : Lexing.lexbuf -> unit
(* 'def' functions for initialisation only *)
    val def_code : string -> (Lexing.lexbuf -> unit) -> unit
    val def_name_code : string -> (string -> Lexing.lexbuf -> unit) -> unit
    val def_fun : string -> (string -> string) -> unit
(* various calls of main scanner, should tidy that a bit *)
    val get_this_main : string -> string
    val get_this_arg_mbox : string arg -> string
    val get_prim_onarg : string Lexstate.arg -> string
    val check_this_main : string -> bool
    val get_prim : string -> string
    val get_prim_arg : Lexing.lexbuf -> string
    val get_prim_opt : string -> Lexing.lexbuf -> string
    val get_csname : Lexing.lexbuf -> string
end

module Make (Dest : OutManager.S) (Image : ImageManager.S) : S


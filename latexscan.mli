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
    val main : Lexing.lexbuf -> unit
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
    val check_alltt_skip : Lexing.lexbuf -> unit
    val skip_pop : Lexing.lexbuf -> unit
(* ``def'' functions for initialisation only *)
    val def_code : string -> (Lexing.lexbuf -> unit) -> unit
    val def_name_code : string -> (string -> Lexing.lexbuf -> unit) -> unit
    val def_fun : string -> (string -> string) -> unit
    val get_this_main : string -> string
    val check_this_main : string -> bool
    val get_prim : string -> string
    val get_prim_arg : Lexing.lexbuf -> string
    val get_prim_opt : string -> Lexing.lexbuf -> string
    val get_csname : Lexing.lexbuf -> string
end

module Make (Dest : OutManager.S) (Image : ImageManager.S) : S


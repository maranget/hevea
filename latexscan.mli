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
    val no_prelude : unit -> unit

    val print_env_pos : unit -> unit
    val main : Lexing.lexbuf -> unit

    (* additional resources needed for extension modules. *)
    val cur_env : string ref
    val new_env : string -> unit
    val close_env : string -> unit
    val env_level : int ref
    val macro_register : string -> unit
    val top_open_block : string -> string -> unit
    val top_close_block : string -> unit
end

module Make (Dest : OutManager.S) (Image : ImageManager.S)
    (Lexget : Lexget.S): S


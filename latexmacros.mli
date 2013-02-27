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

exception Failed

type saved
val checkpoint : unit -> saved
val hot_start : saved -> unit
val pretty_macro : Lexstate.pat -> Lexstate.action -> unit
val pretty_table : unit -> unit
val set_saved_macros : unit -> unit
val get_saved_macro : string -> bool

val register_init : string -> (unit -> unit) -> unit
val exec_init : string -> unit

val open_group : unit -> unit
val close_group : unit -> unit
val get_level : unit -> int

val exists : string -> bool
val find : string -> Lexstate.pat * Lexstate.action
val pretty_command : string -> unit
val def : string -> Lexstate.pat -> Lexstate.action -> unit
val global_def : string -> Lexstate.pat -> Lexstate.action -> unit
(* Undefine a command *)
val global_undef : string -> unit

(******************)
(* For inside use *)
(******************)

(* raises Failed if already defined *)
val def_init : string -> (Lexing.lexbuf -> unit) -> unit
(* raises Failed if not defined *)
val find_fail : string -> Lexstate.pat * Lexstate.action

(* 
  replace name new,
     Send back the Some (old definition for name) or None

  - if new is Some (def)
        then def replaces the old definition, or a definition is created
  - if new is None, then undefine the last local binding for name.
*)
val replace : string -> (Lexstate.pat * Lexstate.action) option ->
  (Lexstate.pat * Lexstate.action) option

(* Add tokens at the end of subst macros, created with zero args
   if non-existing *)
val addto : string -> string list -> unit


val invisible : string -> bool


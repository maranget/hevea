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

exception Failed
exception Error of string

type env =
  Style of string
| Font of int
| Color of string

val pretty_env : env -> string

val find_macro: string -> pat * action
val exists_macro: string -> bool
val is_subst_noarg : action -> pat -> bool

val make_pat: string list -> int -> pat
val def_coltype: string -> pat  -> action -> unit
val def_macro_pat: string -> pat  -> action -> unit
val redef_macro_pat: string -> pat  -> action -> unit
val provide_macro_pat: string -> pat  -> action -> unit
val def_macro: string -> int -> action -> unit
val def_code: string -> (Lexing.lexbuf -> unit) -> unit
val redef_code: string -> (Lexing.lexbuf -> unit) -> unit
val def_name_code: string -> (string -> Lexing.lexbuf -> unit) -> unit
val redef_macro: string -> int -> action -> unit
val def_env_pat: string -> pat -> action -> action -> unit
val redef_env_pat: string -> pat -> action -> action -> unit
val silent_def: string -> int -> action -> unit
val unregister : string -> unit

val invisible : string -> bool
val limit : string -> bool
val int : string -> bool
val big : string -> bool

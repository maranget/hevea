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
exception Error of string

type env =
  Style of string
| Font of int
| Color of string

val pretty_env : env -> string


type action =
    Print of string
  | ItemDisplay
  | Print_arg of int
  | Print_fun of ((string -> string) * int)
  | Subst of string
  | Print_count of ((int -> string)  * int)
  | Test of bool ref
  | SetTest of (bool ref * bool)
  | IfCond of bool ref * action list * action list
  | Br
;;



type pat =  string list * string list
;;

val pretty_pat : pat -> unit

val find_macro: string -> pat * action list

val make_pat: string list -> int -> pat
val def_macro_pat: string -> pat  -> action list -> unit
val redef_macro_pat: string -> pat  -> action list -> unit
val provide_macro_pat: string -> pat  -> action list -> unit
val def_macro: string -> int -> action list -> unit
val redef_macro: string -> int -> action list -> unit
val def_env_pat: string -> pat -> action list -> action list -> unit
val redef_env_pat: string -> pat -> action list -> action list -> unit
val unregister : string -> unit
val newif : string -> string

val display :  bool ref
val in_math :  bool ref
val alltt :  bool ref
val optarg : bool ref
val styleloaded : bool ref
val activebrace : bool ref

val invisible : string -> bool
val limit : string -> bool
val int : string -> bool
val big : string -> bool


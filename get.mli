(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: get.mli,v 1.11 2001-05-25 09:20:44 maranget Exp $"            *)
(***********************************************************************)
open Lexstate

exception Error of string

val init :
  (string arg -> string) ->
  ((Lexing.lexbuf -> unit) -> Lexing.lexbuf -> string) ->
  (string -> unit) -> (string -> unit) ->
  (Lexing.lexbuf -> string) ->
  (Lexing.lexbuf -> unit) -> unit

type saved
val check : unit -> saved
val hot : saved -> unit

val get_int : string arg -> int
val get_bool : string arg -> bool
val get_length : string -> Length.t


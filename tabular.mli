(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: tabular.mli,v 1.11 2001-05-25 12:37:29 maranget Exp $         *)
(***********************************************************************)
exception Error of string

type align =
    {hor : string ; mutable vert : string ; wrap : bool ;
      mutable pre : string ; mutable post : string ; width : Length.t}
type format =
    Align of align
  | Inside of string
  | Border of string

val border : bool ref

val pretty_format : format -> string
val pretty_formats : format array -> unit


val main : string  Lexstate.arg -> format array

(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: tree.mli,v 1.4 2001-05-28 17:28:56 maranget Exp $             *)
(***********************************************************************)
open Lexeme



type 'a t =
  | Text of string
  | Blanks of string
  | Node of 'a * ('a t) list
  | ONode of string * string * ('a t) list

(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: tree.mli,v 1.2 2001-05-25 09:20:50 maranget Exp $"            *)
(***********************************************************************)
open Lexeme

type style =
 {tag : tag ; attrs : attrs ; txt : string ; ctxt : string}


type 'a t =
  | Text of string
  | Blanks of string
  | Node of 'a * ('a t) list
  | ONode of string * string * ('a t) list

(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: tree.mli,v 1.5 2005-02-25 17:49:18 maranget Exp $             *)
(***********************************************************************)

type 'a t =
  | Text of string
  | Blanks of string
  | Node of 'a * ('a t) list
  | ONode of string * string * ('a t) list

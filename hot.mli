(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: hot.mli,v 1.3 2001-05-25 12:37:23 maranget Exp $              *)
(***********************************************************************)
type saved

val checkpoint : unit -> saved
val start : saved -> unit

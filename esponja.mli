(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: esponja.mli,v 1.1 2001-05-25 12:37:21 maranget Exp $           *)
(***********************************************************************)
val pess : bool ref
val move : bool ref

val process : string -> in_channel -> out_channel -> bool
val file : string -> bool

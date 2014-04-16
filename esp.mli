(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: esp.mli,v 1.1 2007-02-09 14:44:50 maranget Exp $           *)
(***********************************************************************)

exception Failed

module type Config = sig
  val pess : bool
  val move : bool
  val small_length : int
end

module Make(C:Config) : sig
  val file : string -> unit
end


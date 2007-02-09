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
val pess : bool ref
val move : bool ref

val process :
    Emisc.Strings.t option -> string -> in_channel -> out_channel -> bool
val file : string -> bool

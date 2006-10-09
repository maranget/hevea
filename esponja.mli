(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: esponja.mli,v 1.2 2006-10-09 08:25:16 maranget Exp $           *)
(***********************************************************************)
val pess : bool ref
val move : bool ref

val process :
    Emisc.Strings.t option -> string -> in_channel -> out_channel -> bool
val file : string -> bool

(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: ultra.mli,v 1.4 2001-05-28 17:28:56 maranget Exp $            *)
(***********************************************************************)

val verbose : int ref
val main : out_channel -> Lexeme.style Tree.t list -> unit

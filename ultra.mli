(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: ultra.mli,v 1.5 2006-10-09 08:25:16 maranget Exp $            *)
(***********************************************************************)

val main : out_channel -> Lexeme.style Tree.t list -> unit

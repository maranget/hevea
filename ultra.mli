(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: ultra.mli,v 1.3 2001-05-25 12:37:31 maranget Exp $            *)
(***********************************************************************)

val verbose : int ref
val main : Tree.style Tree.t list -> Htmltext.style Tree.t list

(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: ultra.mli,v 1.2 2001-05-25 09:20:51 maranget Exp $"            *)
(***********************************************************************)
val verbose : int ref
val main : Tree.style Tree.t list -> Htmltext.style Tree.t list

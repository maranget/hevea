(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: pp.mli,v 1.4 2001-05-28 17:28:56 maranget Exp $               *)
(***********************************************************************)
val ptree : out_channel -> Lexeme.style Tree.t -> unit
val ptrees : out_channel ->  Lexeme.style Tree.t list -> unit

val tree : out_channel -> Htmltext.style Tree.t -> unit
val trees : out_channel ->  Htmltext.style Tree.t list -> unit

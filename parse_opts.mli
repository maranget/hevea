(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

val verbose : int ref
val readverb : int ref
val silent : bool ref
val symbols : bool ref
type language = Francais | English
val language : language  ref
val read_idx : bool ref
val files : string list ref
val except : string list ref
val path : string list ref
val outname : string ref
val warning : string -> unit

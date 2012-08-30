(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Extract/abstract style attributes *)

val get : Lexing.lexbuf -> Emisc.StringCount.count
val set : string Emisc.StringMap.t -> out_channel -> Lexing.lexbuf -> bool

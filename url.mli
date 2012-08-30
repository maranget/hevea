(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

(* URL encoding and decoding *)

val encode_fragment : (char -> unit) -> (string -> unit) -> string -> unit
val decode_fragment : (char -> unit) -> (string -> unit) -> string -> unit

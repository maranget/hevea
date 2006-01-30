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

(* To store an association anchor -> filename *)
val add : string -> string -> unit

(*
   To retrieve associations,
   the filename retrieved can be changed at the very last moment
   by the change function given as argument
*)

(* fullname change file name: format reference from file 'file' to 'name' *)
val fullname : (string -> string) -> string -> string -> string

(* Dump the whole cross-reference table *)
val dump : string -> (string -> string) -> unit

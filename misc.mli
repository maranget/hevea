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

exception Fatal of string
exception Purposly of string
exception ScanError of string
exception EndInput
exception EndDocument
exception Close of string
exception EndOfLispComment of int (* QNC *)

val verbose : int ref
val readverb : int ref
val silent : bool ref
val column_to_command : string -> string
val warning : string -> unit
val print_verb : int -> string -> unit
val message : string -> unit
val fatal : string -> 'a
val copy_hashtbl : (string, 'a) Hashtbl.t -> (string, 'a) Hashtbl.t -> unit
val clone_hashtbl : (string, 'a) Hashtbl.t -> (string, 'a) Hashtbl.t

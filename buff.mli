(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: buff.mli,v 1.4 2001-05-28 17:28:55 maranget Exp $             *)
(***********************************************************************)
type t

val create : unit -> t
val put_char : t -> char -> unit
val put : t -> string -> unit
val to_string : t -> string
val reset : t -> unit

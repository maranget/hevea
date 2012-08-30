(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: emisc.mli,v 1.2 2006-10-09 08:25:16 maranget Exp $          *)
(***********************************************************************)

val verbose : int ref
val basefont : int ref
val reset : unit -> unit
val dump_class : out_channel -> string -> string -> unit
module Strings : Set.S with type elt = string
module StringMap : Map.S with type key = string
module StringCount : Count.S with type key = string
exception LexError of string

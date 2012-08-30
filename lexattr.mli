(************************************************************************)
(*                                                                      *)
(*                          HEVEA                                       *)
(*                                                                      *)
(*  Luc Maranget, Thibault Suzanne, projet MOSCOVA, INRIA Rocquencourt  *)
(*                                                                      *)
(*  Copyright 1998 Institut National de Recherche en Informatique et    *)
(*  Automatique.  Distributed only by permission.                       *)
(*                                                                      *)
(************************************************************************)


(* 
   add_style "name:val" attributes return the attributes with the
   "name" style having the value "val". The "style=" attribute is
   added if needed
*)

val add_style : string -> string -> string

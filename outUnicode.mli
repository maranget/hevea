(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet MOSCOVA, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2006 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Parse unicode chars given the HTML way *)
val parse : string -> int

(* Set output translator by key *)
val set_translate : string -> unit
(* Set output translator as the page translator *)
val set_translate_page : int -> unit

(* Translate for output *)
exception CannotTranslate
val translate : int -> char

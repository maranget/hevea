(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: length.mli,v 1.8 2012-06-05 14:55:39 maranget Exp $           *)
(***********************************************************************)

val base_font_size : int
val pixel_to_char : int -> int
val pixel_to_char_float : int -> float
val char_to_pixel : int -> int

type t =
  | Char of int
  | Pixel of int
  | Percent of int
  | NotALength of string
  | Default

val pretty : t -> string
val is_zero : t -> bool
val as_number_of_chars : t -> int

val main : Lexing.lexbuf -> t

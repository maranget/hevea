(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: length.mli,v 1.5 2001-05-25 09:20:47 maranget Exp $"            *)
(***********************************************************************)
val font : int

type t = Char of int | Pixel of int | Percent of int | No of string | Default
val pretty : t -> string

val font : int
val pixel_to_char : int -> int
val char_to_pixel : int -> int
val main: Lexing.lexbuf -> t

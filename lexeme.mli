(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: lexeme.mli,v 1.3 2001-05-25 12:37:26 maranget Exp $           *)
(***********************************************************************)
type tag =
  | TT |I |B |BIG |SMALL
  | STRIKE | S |U |FONT
  | EM |STRONG |DFN |CODE |SAMP
  | KBD |VAR |CITE |ABBR |ACRONYM 
  | Q |SUB |SUP | A

type atag =
  | SIZE of string | COLOR of string | FACE of string | OTHER

type attr = atag * string

type attrs = attr list

type token =
  | Open of tag * attrs * string
  | Close of tag * string
  | Text of string
  | Blanks of string
  | Eof



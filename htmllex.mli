(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: htmllex.mli,v 1.6 2006-10-09 08:25:16 maranget Exp $          *)
(***********************************************************************)
exception Error of string

val ptop : unit -> unit
val to_string : Lexeme.token -> string
val cost : Lexeme.style -> int * int
val reset : unit -> unit
val next_token : Lexing.lexbuf -> Lexeme.token
val styles : Lexing.lexbuf -> Css.id list
val classes : Lexing.lexbuf -> Emisc.Strings.t

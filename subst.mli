(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: subst.mli,v 1.5 2001-05-25 09:20:49 maranget Exp $"            *)
(***********************************************************************)
open Lexstate
val do_subst_this : string arg -> string
val subst_this : string -> string
val subst_arg : Lexing.lexbuf -> string
val subst_opt : string -> Lexing.lexbuf -> string
val subst_body : Lexing.lexbuf -> string

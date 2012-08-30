(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: subst.mli,v 1.8 2007-06-06 18:24:19 maranget Exp $            *)
(***********************************************************************)
open Lexstate

val do_subst_this : string arg -> string
val do_subst_this_list : string list arg -> string

val subst_list : string list arg -> string list
val subst_this : string -> string
val subst_arg : Lexing.lexbuf -> string
val subst_expn_arg : Lexing.lexbuf -> string
val subst_opt : string -> Lexing.lexbuf -> string list
val subst_body : Lexing.lexbuf -> string list
val subst_expn_body : Lexing.lexbuf -> string list
val subst_arg_list : Lexing.lexbuf -> string list

val uppercase : string -> string
val lowercase : string -> string

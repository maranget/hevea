(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* "$Id: cut.mli,v 1.4 2006-11-10 08:28:46 maranget Exp $" *)

type toc_style = Normal | Both | Special

exception Error of string

module type Config = sig
  val verbose : int
  val name_in : string
  val name_out : string
  val toc_style : toc_style
  val svg_arrows: bool
  val cross_links : bool
  val small_length : int
end

module Make (Config:Config) :
sig
  val dir : string option
  val base : string
  val real_name : string -> string
  val check_changed : string -> string
  val start_phase : unit -> unit
  val do_lex : Lexing.lexbuf -> bool
end

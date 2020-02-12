(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: zyva.mli,v 1.1 2007-02-16 19:22:52 maranget Exp $             *)
(***********************************************************************)

module type S =
  functor (Dest : OutManager.S) ->
    functor (Image : ImageManager.S) ->
      functor (Scan : Latexscan.S) ->
        Misc.Rien

module Make :
  functor (Dest : OutManager.S) ->
    functor (Image : ImageManager.S) ->
      functor (Scan : Latexscan.S) ->
        functor (ToMake : S) -> sig module Rien : sig end val rien : unit end

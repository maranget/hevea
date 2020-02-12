(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: zyva.ml,v 1.4 2007-02-16 19:22:52 maranget Exp $              *)
(***********************************************************************)
module type S =
 functor (Dest : OutManager.S) ->
   functor (Image : ImageManager.S) ->
     functor (Scan : Latexscan.S) ->
       Misc.Rien


module
    Make
    (Dest: OutManager.S) (Image : ImageManager.S) (Scan : Latexscan.S)
    (ToMake : S) =
  struct
    module Rien =  ToMake (Dest) (Image) (Scan)
    let rien = Rien.rien
  end

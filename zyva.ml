(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: zyva.ml,v 1.2 2001-05-25 09:20:56 maranget Exp $"            *)
(***********************************************************************)
module type S =
 functor (Dest : OutManager.S) ->
   functor (Image : ImageManager.S) ->
     functor (Scan : Latexscan.S) ->
       sig end


module
    Make
    (Dest: OutManager.S) (Image : ImageManager.S) (Scan : Latexscan.S)
    (ToMake : S) =
struct
  module Rien = ToMake (Dest) (Image) (Scan)
  
end

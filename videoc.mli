(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: videoc.mli,v 1.6 2001-05-25 09:20:55 maranget Exp $"            *)
(***********************************************************************)

(* <Christian.Queinnec@lip6.fr>
 The plugin for HeVeA that implements the VideoC style.
 $Id: videoc.mli,v 1.6 2001-05-25 09:20:55 maranget Exp $
*)

module type T =
  sig
  end;;

module Make
    (Dest : OutManager.S) (Image : ImageManager.S)
    (Scan : Latexscan.S) : T

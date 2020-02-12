(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*  Christian Queinnec, Universite Paris IV                            *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* <Christian.Queinnec@lip6.fr>
 The plugin for HeVeA that implements the VideoC style.
 $Id: videoc.mli,v 1.7 2001-05-25 12:37:34 maranget Exp $
*)

module Make
    (Dest : OutManager.S) (Image : ImageManager.S)
    (Scan : Latexscan.S) : Misc.Rien

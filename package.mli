(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

(*  $Id: package.mli,v 1.2 1999-10-13 08:21:26 maranget Exp $    *)

module Make
    (Dest : OutManager.S) (Image : ImageManager.S)
    (Scan : Latexscan.S) : Misc.Rien

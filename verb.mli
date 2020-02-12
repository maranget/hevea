(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: verb.mli,v 1.9 2001-05-25 12:37:32 maranget Exp $             *)
(***********************************************************************)
exception VError of string

module Make
    (Dest : OutManager.S) (Image : ImageManager.S)
    (Scan : Latexscan.S) : Misc.Rien


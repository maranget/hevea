(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: verb.mli,v 1.8 2001-05-25 09:20:52 maranget Exp $"            *)
(***********************************************************************)
exception VError of string

module type S = sig  end


module Make
    (Dest : OutManager.S) (Image : ImageManager.S)
    (Scan : Latexscan.S) : S


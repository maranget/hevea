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

(*  $Id: package.mli,v 1.1 1999-10-08 17:58:16 maranget Exp $    *)

module type S = sig  end

module MakeAlso
    (Dest : OutManager.S) (Image : ImageManager.S)
    (Scan : Latexscan.S) (Lexget : Lexget.S) : S

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

let header = "$Id: section.ml,v 1.2 1998-07-21 11:18:42 maranget Exp $" 
let value s = match String.uppercase s with
  "DOCUMENT"|"" -> 0
| "PART" -> 1
| "CHAPTER" -> 2
| "SECTION" -> 3
| "SUBSECTION" -> 4
| "SUBSUBSECTION" -> 5
| _         -> 6
;;

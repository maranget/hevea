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

let header = "$Id: section.ml,v 1.4 2006-09-06 13:52:05 maranget Exp $" 
let value s = match String.uppercase s with
  "DOCUMENT"|""|"NOW" -> 0
| "PART" -> 1
| "CHAPTER" -> 2
| "SECTION" -> 3
| "SUBSECTION" -> 4
| "SUBSUBSECTION" -> 5
| "PARAGRAPH" -> 6
| "SUBPARAGRAPH" -> 7
| _         ->
    Misc.warning (Printf.sprintf "argument '%s' as section level" s) ;
    8
;;

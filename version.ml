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

let header = "$Id: version.ml,v 1.197 2010-03-16 14:02:50 maranget Exp $" 
let real_version = "1.10+12"
let release_date = "2010-03-16"

let version =
  try
   let _ = String.index real_version '+' in
   real_version^" of "^release_date
  with
  | Not_found -> real_version

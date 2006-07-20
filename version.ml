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

let header = "$Id: version.ml,v 1.140 2006-07-20 12:52:48 maranget Exp $" 
let real_version = "1.08+14"
let release_date = "2006-07-20"

let version =
  try
   let _ = String.index real_version '+' in
   real_version^" of "^release_date
  with
  | Not_found -> real_version

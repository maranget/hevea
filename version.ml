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

let header = "$Id: version.ml,v 1.34 2000-05-22 12:19:15 maranget Exp $" 
let real_version = "1.06-1"
let release_date = "2000-05-04"

let version =
  try
   let _ = String.index real_version '-' in
   real_version^" of "^release_date
  with
  | Not_found -> real_version

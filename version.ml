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

let header = "$Id: version.ml,v 1.21 1999-12-13 16:01:17 maranget Exp $" 
let real_version = "1.05-6"
let release_date = "1999-12-13"

let version =
  try
   let _ = String.index real_version '-' in
   real_version^" of "^release_date
  with
  | Not_found -> real_version

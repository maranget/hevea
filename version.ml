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

let real_version = "2.04"
let release_date = "2013-03-12"


let version =
  try
   let _ = String.index real_version '+' in
   real_version^" of "^release_date
  with
  | Not_found -> real_version

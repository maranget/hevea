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

let header = "$Id: mylib.ml,v 1.7 2001-05-25 09:07:26 maranget Exp $" 
exception Error of string
;;

let static_libdir  = LIBDIR
;;

let libdir =
  try Sys.getenv "HEVEADIR" with Not_found -> LIBDIR
;;


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

let _header = "$Id: mylib.ml,v 1.9 2012-06-05 14:55:39 maranget Exp $" 

let static_libdir  = "LIBDIR"
;;

let libdir = try Sys.getenv "HEVEADIR" with Not_found -> static_libdir
;;


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

let header = "$Id: mylib.ml,v 1.8 2004-11-23 14:32:37 maranget Exp $" 
exception Error of string
;;

let static_libdir  = "LIBDIR"
;;

let libdir = try Sys.getenv "HEVEADIR" with Not_found -> static_libdir
;;


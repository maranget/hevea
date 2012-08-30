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

let _header = "$Id: noimage.ml,v 1.7 1999-12-01 19:04:50 maranget Exp $" 
let start () = ()
and stop () = ()
and restart () = ()
;;

let put _ = ()
and put_char _ = ()
;;

let dump _ image lexbuf  = image lexbuf
let page () = ()
;;
let finalize _ = false
;;

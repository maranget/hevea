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

let header = "$Id: noimage.ml,v 1.5 1999-11-05 19:02:17 maranget Exp $" 
let start () = ()
and stop () = ()
and restart () = ()
;;

let put _ = ()
and put_char _ = ()
;;

let dump _ image lexbuf  = image lexbuf
let page () =
  Misc.warning ("No image dumped") ;
  "XXX"
;;
let finalize _ = ()
;;

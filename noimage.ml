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

let header = "$Id: noimage.ml,v 1.3 1999-08-17 13:26:46 maranget Exp $" 
let start () = ()
;;
let put _ = ()
and put_char _ = ()
;;

let dump _ image lexbuf  = image lexbuf
let page () =
  Misc.warning ("No image dumped") ;
  "XXX"
;;
let finalize () = ()
;;

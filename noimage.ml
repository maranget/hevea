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

let header = "$Id: noimage.ml,v 1.1 1999-06-03 13:13:33 maranget Exp $" 
let start _ = ()
;;
let put _ = ()
and put_char _ = ()
;;

let dump _ image lexbuf  = image lexbuf
let page () =
  Parse_opts.warning ("No image dumped") ;
  "XXX"
;;
let finalize () = ()
;;

(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: emisc.ml,v 1.2 2005-02-25 17:49:18 maranget Exp $          *)
(***********************************************************************)

let basefont = ref 3

let reset () =  basefont := 3

let warning s =
  Location.print_pos () ;
  prerr_string "Warning: " ;
  prerr_endline s


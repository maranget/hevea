(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: emisc.ml,v 1.3 2006-10-09 08:25:16 maranget Exp $          *)
(***********************************************************************)

let verbose = ref 0

let basefont = ref 3

let reset () =  basefont := 3

module OString =
  struct
    type t = string
    let compare = String.compare
  end

module Strings = Set.Make (OString)

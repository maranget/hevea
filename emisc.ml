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

open Printf

let verbose = ref 0
let basefont = ref 3
let reset () =  basefont := 3
let dump_class chan cl st = fprintf chan ".%s{%s}\n" cl st

module Strings = Set.Make(String)
module StringMap = Map.Make(String)
module StringCount = Count.Make(String)
exception LexError of string

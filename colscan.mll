(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: colscan.mll,v 1.6 2001-11-14 12:57:04 maranget Exp $          *)
(***********************************************************************)
{
open Lexing

exception Error of string
;;

let buf = Out.create_buff ()
;;
} 
rule one = parse
| ' '+ {one lexbuf}
| ('0'|'1')?'.'?['0'-'9']*
  {let lxm = lexeme lexbuf in
  try float_of_string lxm with _ -> assert false}
| "" {raise (Error "Syntax error in color argument")}

and other = parse
  ' '* ',' {one lexbuf}
|  ""      {raise (Error "Syntax error in color argument")}

and three = parse
  ""
  {let fst = one lexbuf in
  let snd = other lexbuf in
  let thrd = other lexbuf in
  fst,snd,thrd}
and four = parse
  ""
  {let fst = one lexbuf in
  let snd = other lexbuf in
  let thrd = other lexbuf in
  let fourth = other lexbuf in
  fst,snd,thrd,fourth}


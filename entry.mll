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

{
open Lexing

let header = "$Id: entry.mll,v 1.8 1998-10-09 16:32:56 maranget Exp $" 

let buff = Out.create_buff ()
;;

let put s =
  Out.put buff s
and put_char c =
  Out.put_char buff c
;;


type res =
  Bang of string * string
| Bar of string * string
| Eof of string * string
;;

let extend r i = match r with
  Bang (p,_) -> Bang (i,p)
| Bar (p,_) -> Bar (i,p)
| Eof (p,_) -> Eof (i,p)
;;

exception Fini
;;

}
rule entry = parse
  "\\\""
    {put "\\\"" ; entry lexbuf}
| "\"!"
    {put_char '!' ; entry lexbuf}
| "\"@"
    {put_char '@' ; entry lexbuf}
| "\"|"
    {put_char '|' ; entry lexbuf}
| '!' {Bang   (Out.to_string buff,"")}
| '@' {let s = Out.to_string buff in
      let r = entry lexbuf in
      extend r s}
| '|' {Bar (Out.to_string buff,"")}
| eof {Eof (Out.to_string buff,"")}
| _
   {let lxm = lexeme_char lexbuf 0 in put_char lxm ; entry lexbuf}      


and idx = parse
  "\\indexentry"
     {let x = Save.arg lexbuf in
     let _ = Save.arg lexbuf in
     x}
| eof {raise Fini}
| _   {idx lexbuf}



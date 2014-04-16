(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Erase tags (for title in hacha) *)

{

 exception Error
 let buff = Buffer.create 32

}
let blank = [' ''\t''\n''\r']
let tag = ['a'-'z''A'-'Z''0'-'9']+
let class_name = ['a'-'z''A'-'Z''0'-'9''-']+
let attr_name = ['a'-'z''A'-'Z''-''0'-'9']+ 

rule tagout = parse
| ('<' | "</") tag { skiptag lexbuf ; tagout lexbuf }
| [^'<']+ as lxm
    { Buffer.add_string buff lxm ; tagout lexbuf }
| eof { Buffer.contents buff }
| "" { raise Error }

and skiptag = parse
| '>' { () }
| blank+
| attr_name ( blank* "=" blank*
  ('\'' ([^'\'']*) '\''
  | '"' ([^'"']*) '"'
  | '#'?['a'-'z''A'-'Z''0'-'9''-''+''_'':''.']+))?
(* '"' *)
  { skiptag lexbuf }
| "" { raise Error }

{
let tagout s =
  Buffer.reset buff ;
  tagout (MyLexing.from_string s)
}

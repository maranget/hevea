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
open Parse_opts
let header = "$Id: auxx.mll,v 1.2 1998-10-09 16:32:55 maranget Exp $" 

let rtable = Hashtbl.create 17
;;

let rset name value = Hashtbl.add rtable name value
;;

let rget name =
  try Hashtbl.find rtable name with Not_found -> "X"
;;

let btable = Hashtbl.create 17
;;

let bset name value = Hashtbl.add btable name value
;;

let bget name =
  try Hashtbl.find btable name with Not_found -> name
;;

}

rule main = parse
  "\\newlabel"
    {let name = Save.arg lexbuf in
    let value = Save.arg lexbuf in
    let value = Save.arg (from_string value) in
    rset name value ; main lexbuf}
| "\\bibcite"
    {let name = Save.arg lexbuf in
    let value = Save.arg lexbuf in
    bset name value ;
    main lexbuf}
| "\\@input"
    {let filename = Save.arg lexbuf in
    begin try
      let filename,chan = Myfiles.open_tex filename in
      let newbuf = from_channel chan in
      main newbuf
    with Myfiles.Except -> begin
      if !verbose > 0 then
        prerr_endline ("Not opening file: "^filename) ;
      end
    | Myfiles.Error m ->
        if not !silent || !verbose > 0 then begin
          Location.print_pos () ;
          prerr_endline ("Warning: "^m) ;
        end
    end ;
    main lexbuf}
| _   {main lexbuf}
| eof {()}

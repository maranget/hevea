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

let header = "$Id: symb.ml,v 1.20 2003-12-05 10:44:46 maranget Exp $" 
open Parse_opts

let tr = function
| "\\{" -> "{"
| "\\}" -> "}"
| "\\|" -> "<FONT FACE=symbol>½½</FONT>"
| "\\lfloor" -> "<FONT FACE=symbol>ë</FONT>"   
| "\\rfloor" -> "<FONT FACE=symbol>û</FONT>"
| "\\lceil"  -> "<FONT FACE=symbol>é</FONT>"
| "\\rceil"  -> "<FONT FACE=symbol>ù</FONT>"
| s   -> s
;;

let put_delim skip put d n =

  let  put_skip s = put s ; skip () ; in

  let rec do_rec s i =
    if i >= 1 then begin
      put_skip s;
      do_rec s (i-1)
    end

  and do_bis s i =
    if i>= 2 then begin
      put_skip s ;
      do_bis s (i-1)
    end else
      put s in

  if not !symbols || n=1 then
    let d = tr d in
    do_bis d n
  else if !entities then begin
    if d = "(" then begin
      put_skip "&#9115;" ;
      do_rec "&#9116;" (n-2) ;
      put "&#9117;"
    end else if d=")" then begin
      put_skip "&#9118;" ;
      do_rec "&#9119;" (n-2) ;
      put "&#9120;"
    end else if d = "[" then begin
      put_skip "&#9121;" ; 
      do_rec "&#9122;" (n-2) ;
      put "&#9123;"
    end else if d="]" then begin
      put_skip "&#9124;" ; 
      do_rec "&#9125;" (n-2) ;
      put "&#9126;"
   end else if d = "\\lfloor" then begin
      do_rec "&#9122;" (n-1) ;
      put "&#9123;"
    end else if d="\\rfloor" then begin
      do_rec "&#9125;" (n-1) ;
      put "&#9126;"
    end else if d = "\\lceil" then begin
      put_skip "&#9121" ; 
      do_bis "&#9122" (n-1)
    end else if d="\\rceil" then begin
      put_skip "&#9124;" ; 
      do_bis "&#9125;" (n-1)
    end else if d="|" then begin
      do_bis "|" n
    end else if d="\\|" then begin
      do_bis "||" n
    end else if d = "\\{" then begin
      put_skip "&#9127;" ; 
      do_rec "&#9130;" ((n-3)/2) ;
      put_skip "&#9128;" ;  
      do_rec "&#9130;" ((n-3)/2) ;
      put "&#9129;"     
    end else if d = "\\}" then begin
      put_skip "&#9131;" ; 
      do_rec "&#9130;" ((n-3)/2) ;
      put_skip "&#9132;" ; 
      do_rec "&#9130;" ((n-3)/2) ;
      put "&#9133;"     
    end
  end else begin
    put "<FONT FACE=symbol>\n" ;
    if d = "(" then begin
      put_skip "æ" ;
      do_rec "ç" (n-2) ;
      put "è"
    end else if d=")" then begin
      put_skip "ö" ;
      do_rec "÷" (n-2) ;
      put "ø"
    end else if d = "[" then begin
      put_skip "é" ; 
      do_rec "ê" (n-2) ;
      put "ë"
    end else if d="]" then begin
      put_skip "ù" ; 
      do_rec "ú" (n-2) ;
      put "û"
   end else if d = "\\lfloor" then begin
      do_rec "ê" (n-1) ;
      put "ë"
    end else if d="\\rfloor" then begin
      do_rec "ú" (n-1) ;
      put "û"
    end else if d = "\\lceil" then begin
      put_skip "é" ; 
      do_bis "ê" (n-1)
    end else if d="\\rceil" then begin
      put_skip "ù" ; 
      do_bis "ú" (n-1)
    end else if d="|" then begin
      do_bis "½" n
    end else if d="\\|" then begin
      do_bis "½½" n
    end else if d = "\\{" then begin
      put_skip "ì" ; 
      do_rec "ï" ((n-3)/2) ;
      put_skip "í" ;  
      do_rec "ï" ((n-3)/2) ;
      put "î"     
    end else if d = "\\}" then begin
      put_skip "ü" ; 
      do_rec "ï" ((n-3)/2) ;
      put_skip "ı" ; 
      do_rec "ï" ((n-3)/2) ;
      put "ş"     
    end ;
    put "</FONT>"
  end
;;

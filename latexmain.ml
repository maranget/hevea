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

let header = "$Id: latexmain.ml,v 1.25 1999-02-19 18:00:04 maranget Exp $" 

open Parse_opts

module Scan = Latexscan.Make (Html)
;;

let finalize check =
  Image.finalize () ;
  Html.finalize check
;;

let read_style name =
  let oldverb = !verbose in
  if !verbose > 0 then verbose := 1;
  begin try
    let name,chan =  Myfiles.open_tex name in
    if !verbose > 0 then begin
       prerr_endline ("read_style: "^name)
    end ;
    let buf = Lexing.from_channel chan in
    Location.set name buf;
    Scan.main buf ;
    Location.restore ()
  with Myfiles.Except-> ()  end ;
  verbose := oldverb
;;
 
let main () = 

    verbose := !readverb ;
    read_style "hevea.hva" ;

    begin match !files with
     [] -> files := ["article.hva"]
    | _ -> () end;

    let texfile = match !files with
      [] -> ""
    | x::rest ->
       if Filename.check_suffix x ".tex" then begin
         files := rest ;
         x
       end else "" in

    Image.base :=
     (if !outname <> ""  then  Filename.chop_suffix !outname ".html"
     else begin match texfile with
        "" -> !Image.base
     | _   -> Filename.chop_suffix texfile ".tex" end) ;

    let rec do_rec = function
      [] -> ()
    | x::rest ->
       do_rec rest ;
       read_style x in

    do_rec !files ;

    let basename = match texfile with "" -> "zorglub"
      | _ -> Filename.chop_suffix texfile ".tex" in

    Location.set_base basename ;

    Scan.out_file :=
      if !outname <> "" then
         Out.create_chan (open_out !outname)
      else begin match texfile with
        "" ->  Out.create_chan stdout
      | s  ->
         Out.create_chan
           (open_out (Filename.basename basename^".html")) end ;
        
    begin match texfile with
      "" -> Scan.no_prelude ()
    | _  ->
       let auxname = Filename.basename basename^".aux" in
       try
         let _,auxchan = Myfiles.open_tex auxname in
         let buf = Lexing.from_channel auxchan in
         Auxx.main buf
       with Myfiles.Error _ -> begin
         if !verbose > 0 then
           prerr_endline ("Cannot open aux file: "^auxname)
       end
    end ;
    let chan = match texfile with "" -> stdin | _ -> open_in texfile in
    let buf = Lexing.from_channel chan in
    Location.set texfile buf ;
    Save.set_verbose !silent !verbose ;
    Scan.main buf ;
    Location.restore () ;
    finalize true
;;   


begin try
  main ()
with
| Html.Close s ->
    Location.print_pos () ;
    prerr_endline s;
    Scan.print_env_pos () ;
    prerr_endline "Adios" ;
    exit 2
| Html.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while writing HTML: "^s) ;
    prerr_endline "Adios" ;
    exit 2
| Scan.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading LaTeX: "^s) ;
    prerr_endline "Adios" ;
    exit 2
| Colscan.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading LaTeX style colors: "^s) ;
    prerr_endline "Adios" ;
    exit 2
| Save.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading LaTeX macros arguments: "^s) ;
    prerr_endline "Adios" ;
    exit 2
|  Misc.Fatal s ->
    Location.print_pos () ;
    prerr_endline
      ("Fatal error: "^s^" (please report to Luc.Maranget@inria.fr") ;
    prerr_endline "Adios" ;
    exit 2    
|  x ->
    Location.print_pos () ;
    prerr_endline
      ("Fatal error: spurious exception "^Printexc.to_string x^
       " (please report to Luc.Maranget@inria.fr") ;
    prerr_endline "Adios" ;
    exit 2
end ;
exit 0;;


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

let header = "$Id: latexmain.ml,v 1.21 1998-10-26 16:23:17 maranget Exp $" 

open Parse_opts

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
    Location.set name buf ;
    Latexscan.main buf ;
    Location.restore ()
  with Myfiles.Except-> ()  end ;
  verbose := oldverb
;;
 
let main () = 

    verbose := !readverb ;
    read_style "hevea.sty" ;

    begin match !files with
     [] -> files := ["article.sty"]
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

    Latexscan.out_file :=
      if !outname <> "" then
         Out.create_chan (open_out !outname)
      else begin match texfile with
        "" ->  Out.create_chan stdout
      | s  ->
         Out.create_chan
           (open_out (Filename.basename basename^".html")) end ;
        
    begin match texfile with
      "" -> Latexscan.no_prelude ()
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
    Latexscan.main buf ;
    Location.restore () ;
    finalize true
;;   


begin try
  main ()
with x -> begin
  Location.print_pos () ;
  prerr_endline "Adios" ;
  raise x
  end
end ;
exit 0;;


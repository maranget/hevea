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

let header = "$Id: latexmain.ml,v 1.43 1999-06-03 13:13:22 maranget Exp $" 

open Misc
open Parse_opts


let
  subst_this,subst,get_this,scan_main,
  no_prelude,print_env_pos,
  dest_finalize,image_finalize = 

  match !Parse_opts.destination with
  | Html when name_in <> "" ->
      let module Scan = Latexscan.Make (Html) (Image) in
      let module Otherscan = Videoc.Makealso (Html) (Image) (Scan) in
      let module Verbscan = Verb.MakeAlso  (Html) (Image) (Scan) in
      Otherscan.init () ; Verbscan.init () ;
      Scan.subst_this, Scan.subst,
      Scan.get_this, Scan.main, Scan.no_prelude, Scan.print_env_pos,
      Html.finalize, Image.finalize
  | Html  ->
      let module Scan = Latexscan.Make (Html) (Noimage) in
      let module Otherscan = Videoc.Makealso (Html) (Noimage) (Scan) in
      let module Verbscan = Verb.MakeAlso  (Html) (Noimage) (Scan) in
      Otherscan.init () ; Verbscan.init () ;
      Scan.subst_this, Scan.subst,
      Scan.get_this, Scan.main, Scan.no_prelude, Scan.print_env_pos,
      Html.finalize, Noimage.finalize
  | Text ->
      let module Scan = Latexscan.Make (Text) (Noimage)  in
      let module Verbscan = Verb.MakeAlso  (Text) (Noimage) (Scan) in
      Verbscan.init () ;
      Scan.subst_this, Scan.subst,
      Scan.get_this, Scan.main, Scan.no_prelude, Scan.print_env_pos,
      Text.finalize,Noimage.finalize
  | Info ->
      let module Scan = Latexscan.Make (Info) (Noimage) in
      let module Verbscan = Verb.MakeAlso  (Info) (Noimage) (Scan) in
      Verbscan.init () ;
      Scan.subst_this, Scan.subst,
      Scan.get_this, Scan.main, Scan.no_prelude, Scan.print_env_pos,
      Info.finalize, Noimage.finalize
;;

let finalize check =
  image_finalize () ;
  dest_finalize check
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
    begin try scan_main buf with Misc.EndInput -> () end ;
    close_in chan ;
    Location.restore ()
  with
  | Myfiles.Except-> ()
  end ;
  verbose := oldverb
;;

let open_tex name =
  let name,chan =  Myfiles.open_tex name in
  if !verbose > 0 then
    prerr_endline ("Main input_file: "^name) ;
  name,chan

let read_tex name_in =
    let input_name,chan =
      match name_in with "" -> "",stdin | _ -> open_tex name_in in
    let buf = Lexing.from_channel chan in
    Location.set input_name buf ;
    Save.set_verbose !silent !verbose ;
    begin try scan_main buf with Misc.EndInput -> () end ;
    close_in chan ;
    Location.restore ()

let main () = 

    verbose := !readverb ;
    read_style "hevea.hva" ;

    let rec do_rec = function
      [] -> ()
    | x::rest ->
       do_rec rest ;
       read_style x in

    let styles = match Parse_opts.styles with
    | [] -> if base_in = "" then ["article.hva"] else []
    | _  -> Parse_opts.styles in

    do_rec styles ;

    Location.set_base base_in ;

    begin match base_in with
      "" -> no_prelude ()
    | _  ->
       let auxname = base_in^".aux" in
       try
         let _,auxchan = Myfiles.open_tex auxname in
         let buf = Lexing.from_channel auxchan in
         Auxx.main buf ;
         close_in auxchan
       with Myfiles.Error _ -> begin
         if !verbose > 0 then
           prerr_endline ("Cannot open aux file: "^auxname)
       end
    end ;

    read_tex name_in ;
    finalize true
;;   


begin try
  main ()
with
| Html.Close s ->
    Location.print_pos () ;
    prerr_endline s;
    print_env_pos () ;
    prerr_endline "Adios" ;
    exit 2
| Html.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while writing HTML:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Text.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while writing Text:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Info.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while writing Info:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| InfoRef.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while writing Info:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Misc.ScanError s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading LaTeX:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Lexstate.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading LaTeX:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Verb.VError s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading verbatim LaTeX:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Colscan.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading LaTeX style colors:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Save.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading LaTeX macros arguments:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Tabular.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading table format:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Get.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while getting a value:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Latexmacros.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error in macro definition:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Myfiles.Error s ->
    Location.print_pos () ;
    prerr_endline ("File error:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
|  Misc.Fatal s ->
    Location.print_pos () ;
    prerr_endline
      ("Fatal error: "^s^"\n (if input is plain LaTeX, please report to Luc.Maranget@inria.fr)") ;
    prerr_endline "Adios" ;
    exit 2 
|  x ->
    Location.print_pos () ;
    prerr_endline
      ("Fatal error, spurious exception:\n\t"^Printexc.to_string x^
       "\n(if input is plain LaTeX, please report to Luc.Maranget@inria.fr)") ;
    prerr_endline "Adios" ;
    exit 2
end ;
exit 0;;


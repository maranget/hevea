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

let header = "$Id: latexmain.ml,v 1.49 1999-09-08 15:11:21 maranget Exp $" 

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

let prerr_error msg  =
  Location.print_pos () ;
  if msg <> "" then prerr_endline msg
;;

let prerr_bug msg =
  prerr_error msg ;
  prerr_endline
  "    (if input is plain LaTeX, please report to Luc.Maranget@inria.fr)"
;;


let finalize check =
  try
    image_finalize () ;
    Auxx.finalize () ;
    dest_finalize check
  with e ->
    if check then raise e
    else begin
      prerr_bug ("Uncaught exception in finalyze: "^Printexc.to_string e) ;
      prerr_endline "Adios" ;
      exit 2
    end
      
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

    let styles =  Parse_opts.styles in

    do_rec styles ;

    Location.set_base base_in ;

    begin match base_in with
      "" -> no_prelude ()
    | _  ->
       try
         let is_haux,auxchan =
           try
             let name,chan = Myfiles.open_tex (base_in^".aux") in
             if !verbose > 0 then
               prerr_endline ("Input aux file: "^name) ;
             false,chan
           with
             Myfiles.Error _ ->
               let auxname =  base_out^".haux" in
               let chan = open_in auxname in
               if !verbose > 0 then
                 prerr_endline ("Input aux file: "^auxname) ;
               true,chan in
         let buf = Lexing.from_channel auxchan in
         Auxx.main buf ;
         close_in auxchan ;
         if is_haux then
           Auxx.init base_out
       with Sys_error s -> begin
         Auxx.init base_out ;
         if !verbose > 0 then
           prerr_endline ("I found no aux file, going on")
       end
    end ;

    read_tex name_in ;
    finalize true
;;   


let _ = 
  begin try
    main ()
  with e -> begin
    match e with
    | Misc.Close s -> prerr_error s
    | Html.Error s ->
        prerr_error ("Error while writing HTML:\n\t"^s)
    | Text.Error s ->
        prerr_error ("Error while writing Text:\n\t"^s)
    | Info.Error s ->
        prerr_error ("Error while writing Info:\n\t"^s)
    | InfoRef.Error s ->
        prerr_error ("Error while writing Info:\n\t"^s)
    | Misc.ScanError s ->
        prerr_error ("Error while reading LaTeX:\n\t"^s)
    | Lexstate.Error s ->
        prerr_error ("Error while reading LaTeX:\n\t"^s)
    | Verb.VError s ->
        prerr_error ("Error while reading verbatim LaTeX:\n\t"^s)
    | Colscan.Error s ->
        prerr_error ("Error while reading LaTeX style colors:\n\t"^s)
    | Save.Error s ->
        prerr_error ("Error while reading LaTeX macros arguments:\n\t"^s)
    | Tabular.Error s ->
        prerr_error ("Error while reading table format:\n\t"^s)
    | Get.Error s ->
        prerr_error ("Error while getting a value:\n\t"^s)
    | Latexmacros.Error s ->
        prerr_error ("Error in macro definition:\n\t"^s)
    | Myfiles.Error s ->
        prerr_error ("File error:\n\t"^s)
    |  Misc.Fatal s ->
        prerr_bug ("Fatal error: "^s)
    |  Location.Fatal s ->
        prerr_bug ("Fatal location error: "^s)
    |  x ->
        prerr_bug
          ("Fatal error, spurious exception:\n\t"^Printexc.to_string x)
  end ;
    finalize false ;
    prerr_endline "Adios" ;
    exit 2
  end
;;




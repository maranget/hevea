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

let header = "$Id: latexmain.ml,v 1.58 1999-11-08 12:58:11 maranget Exp $" 

open Misc
open Parse_opts


let
  scan_main, no_prelude,
  dest_finalize,image_finalize = 

  match !Parse_opts.destination with
  | Html when name_in <> "" ->
      let module Scan = Latexscan.Make (Html) (Image) in
      let module MakeIt = Zyva.Make (Html) (Image) (Scan) in
      let module Rien = MakeIt (Videoc.Make) in
      let module RienBis  = MakeIt (Package.Make) in
      let module RienTer = MakeIt (Verb.Make) in
      Scan.main, Scan.no_prelude,
      Html.finalize, Image.finalize
  | Html  ->
      let module Scan = Latexscan.Make (Html) (Noimage) in
      let module Otherscan = Videoc.Make (Html) (Noimage) (Scan) in
      let module Verbscan = Verb.Make  (Html) (Noimage) (Scan) in
      let module OptScan = Package.Make  (Html) (Image) (Scan) in
      Scan.main, Scan.no_prelude,
      Html.finalize, Noimage.finalize
  | Text ->
      let module Scan = Latexscan.Make (Text) (Noimage) in
      let module Verbscan = Verb.Make  (Text) (Noimage) (Scan) in
      let module OptScan = Package.Make (Text) (Image) (Scan)  in
      Scan.main, Scan.no_prelude,
      Text.finalize,Noimage.finalize
  | Info ->
      let module Scan = Latexscan.Make (Info) (Noimage) in
      let module Verbscan = Verb.Make  (Info) (Noimage) (Scan) in
      let module OptScan = Package.Make (Info) (Image) (Scan) in
      Scan.main, Scan.no_prelude,
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
    let changed = Auxx.finalize check in
    let changed = Index.finalize check || changed  in
    image_finalize check ;
    dest_finalize check ;
    if !verbose > 0 && Parse_opts.name_out <> "" then begin
      prerr_endline ("Output is in file: "^Parse_opts.name_out)
    end ;
    changed
  with e ->
    if check then raise e
    else begin
      prerr_bug ("Uncaught exception in finalize: "^Printexc.to_string e) ;
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
  Save.set_verbose !silent !verbose ;
  try
    match name_in with
    | "" -> Lexstate.real_input_file !verbose scan_main "" stdin
    | _  -> Lexstate.input_file !verbose scan_main name_in
  with
  | Misc.EndDocument -> ()

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

    if Parse_opts.filter then  no_prelude () ;

    if !Parse_opts.fixpoint then begin
      Lexstate.checkpoint () ;
      Latexmacros.checkpoint () ;
      Counter.checkpoint () ;
      Color.checkpoint () ;
      let rec do_rec i =
        read_tex name_in ;
        if finalize true then begin
          Lexstate.hot_start () ;
          Latexmacros.hot_start () ;
          Counter.hot_start () ;
          Color.hot_start () ;
          Foot.hot_start () ;
          begin match !Parse_opts.destination with
          | Info -> InfoRef.hot_start ()
          | _ -> ()
          end ;
          Auxx.hot_start () ;
          Misc.message ("Run, run, again...") ;
          do_rec (i+1)
        end else
          Misc.message
            ("Fixpoint reached in "^string_of_int i^" step(s)") in
      do_rec 1
    end else begin
      read_tex name_in ;
      let _ = finalize true in ()      
    end
;;   
(*
let _ =
  Dynlink.init () ;
  begin try
    Dynlink.add_interfaces ["Pervasives"] ["/usr/local/lib/ocaml"] ;
    Dynlink.loadfile "a.cmo" ;
  with Dynlink.Error e -> prerr_endline (Dynlink.error_message e)
  end
*)
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
    let _ = finalize false in
    prerr_endline "Adios" ;
    exit 2
  end
;;




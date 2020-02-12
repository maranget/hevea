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

let header = "$Id: hevea.ml,v 1.4 2011-12-07 13:05:57 maranget Exp $" 

open Misc
open Parse_opts



let
    scan_get_prim,
    scan_main, no_prelude, scan_print_env_pos,
    dest_finalize,image_finalize = 

  match !Parse_opts.destination with
    | Html when name_in <> "" ->
	let module Scan = Latexscan.Make (Html) (Image) in
	let module MakeIt = Zyva.Make (Html) (Image) (Scan) in
	let module Rien = MakeIt (Videoc.Make) in
        Rien.rien ;
	let module RienBis  = MakeIt (Package.Make) in
        RienBis.rien ;
	let module RienTer = MakeIt (Verb.Make) in
        RienTer.rien ;
	Scan.get_prim, Scan.main, Scan.no_prelude, Scan.print_env_pos,
	  Html.finalize, Image.finalize
    | Html  ->
	let module Scan = Latexscan.Make (Html) (Noimage) in
	let module Otherscan = Videoc.Make (Html) (Noimage) (Scan) in
	let module Verbscan = Verb.Make  (Html) (Noimage) (Scan) in
	let module OptScan = Package.Make  (Html) (Image) (Scan) in
        Otherscan.rien ; Verbscan.rien ; OptScan.rien ;
	Scan.get_prim, Scan.main, Scan.no_prelude, Scan.print_env_pos,
	  Html.finalize, Noimage.finalize
    | Text ->
	let module Scan = Latexscan.Make (Text) (Noimage) in
	let module Verbscan = Verb.Make  (Text) (Noimage) (Scan) in
	let module OptScan = Package.Make (Text) (Image) (Scan)  in
        Verbscan.rien ; OptScan.rien ;
	  Scan.get_prim, Scan.main, Scan.no_prelude, Scan.print_env_pos,
	  Text.finalize,Noimage.finalize
    | Info ->
	let module Scan = Latexscan.Make (Info) (Noimage) in
	let module Verbscan = Verb.Make  (Info) (Noimage) (Scan) in
	let module OptScan = Package.Make (Info) (Image) (Scan) in
        Verbscan.rien ; OptScan.rien ;
	  Scan.get_prim, Scan.main, Scan.no_prelude, Scan.print_env_pos,
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

and prerr_not_supported msg =
  prerr_error msg ;
  prerr_endline "You ran into hevea limitations, sorrry"
;;


let finalize check =
  try
    let changed = Auxx.finalize check in
    let changed = Index.finalize check || changed  in
    let image_changed = image_finalize check in
    dest_finalize check ;
    if !verbose > 0 && Parse_opts.name_out <> "" then begin
      prerr_endline ("Output is in file: "^Parse_opts.name_out)
    end ;
    changed,image_changed
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
  if !verbose > 0 then verbose := oldverb ;
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

let read_prog prog =
  try
    let real_prog = Myfiles.find prog        
    and name = Filename.temp_file "hevea" ".hva" in
    begin match Sys.command (real_prog^" >"^name) with
    | 0 -> read_style name
    | _ ->
        warning ("Could not exec program file: "^real_prog)
    end ;
    Mysys.remove name
  with
  | Not_found ->
      warning ("Could not find program file: "^prog)

let read_tex name_in =
  Save.set_verbose !silent !verbose ;
  begin try
    match name_in with
    | "" -> Lexstate.real_input_file !verbose scan_main "" stdin
    | _  ->
        Lexstate.input_file !verbose scan_main name_in ;
        let ok = scan_get_prim "\\@end@document@seen" in
        begin match ok with
        | "OK" -> ()
        | _ -> prerr_endline "Warning: \\end{document} is missing"
        end
  with
  | Misc.EndDocument -> () 
  end

let main () = 

    verbose := !readverb ;
    read_style "hevea.hva" ;

    let rec do_rec = function
      [] -> ()
    | File x::rest ->
       do_rec rest ;
       read_style x
    | Prog x::rest ->
       do_rec rest ;
       read_prog x in

    let styles =  Parse_opts.styles in

    do_rec styles ;

    if Parse_opts.filter then  no_prelude () ;

    if !Parse_opts.fixpoint then begin
      let image_changed = ref false in
      let saved = Hot.checkpoint () in
      let rec do_rec i =
        read_tex name_in ;
        let changed,image_changed_now = finalize true in
        image_changed := !image_changed || image_changed_now ;
        if changed then begin
          Hot.start saved ;
          Auxx.hot_start () ;
          Misc.message ("Run, run, again...") ;
          do_rec (i+1)
        end else begin
          Misc.message
            ("Fixpoint reached in "^string_of_int i^" step(s)") ;
          if !image_changed then begin
            Misc.message
              ("Now, I am running imagen for you") ;
            let _ = Sys.command
                (Filename.concat Mylib.libdir "imagen"^
                 " "^Misc.get_image_opt ()^" "^base_out) in ()
          end
        end in
      do_rec 1
    end else begin
      read_tex name_in ;
      let _ = finalize true in ()      
    end ;

(* Optimisation *)
    if !optimize then begin
      match !destination with
      | Html ->
          if name_in <> "" then begin
	    Emisc.verbose := !Misc.verbose ;
            let module E =
              Esp.Make
                (struct
                  let pess = false
                  let move = true
                  let small_length = !small_length
                end) in
            begin try E.file name_out
            with Esp.Failed ->
              warning "Esponja failed, optimisation not performed"
            end
          end
      | Text|Info -> ()        
    end
;;   

let _ = 
  begin try
    main () ;
    exit 0
  with
    | Misc.Close s ->
        prerr_error ("Environment nesting error: "^s) ;
        scan_print_env_pos ()
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
    | Misc.UserError s ->
        prerr_error ("User error:\n\t"^s)
    | Myfiles.Error s ->
        prerr_error ("File error:\n\t"^s)
    |  Misc.NoSupport s ->
        prerr_not_supported s
    |  Misc.Fatal s ->
        prerr_bug ("Fatal error: "^s)
    |  MyStack.Fatal s ->
        prerr_bug ("Fatal stack error, "^s)
(*
    |  x ->
        prerr_bug
          ("Fatal error, spurious exception:\n\t"^Printexc.to_string x)
*)
  end ;
  let _ = finalize false in
  if !verbose = 0 then Mysys.remove Parse_opts.name_out ;
  prerr_endline "Adios" ;
  exit 2
;;




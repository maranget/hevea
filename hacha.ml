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

open Printf

exception Error of string
;;

let filename = ref None
let outname = ref "index.html"
let log = ref false
let toc_style = ref Cut.Normal
let svg_arrows = ref true
let cross_links = ref true
let verbose = ref 0
let small_length = ref 1024

  
let main () =
  let spec =
     [("-o", Arg.String (fun s -> outname := s),
       "filename, make hacha output go into file 'filename' (defaults to index.html)");
     ("-tocbis", Arg.Unit (fun () -> toc_style := Cut.Both),
       ", Duplicate table of contents at the begining of files");      
     ("-tocter", Arg.Unit (fun () -> toc_style := Cut.Special),
       ", Insert most of table of contents at the beginning of files");
     ("-no-svg-arrows", Arg.Unit (fun () ->  svg_arrows := false ),
       ", Use gif arrows for the previous/up/next links in generated pages");
     ("-nolinks", Arg.Unit (fun () -> cross_links := false),
       ", Suppress the prevous/up/next links in generated pages");
     ("-hrf", Arg.Unit (fun () -> log := true),
        ", output a log file showing the association from local anchors to files");
  ("-rsz", Arg.Int (fun i -> small_length := i),
   (sprintf
     "size of leaves in rope implementation (default %i)"
      !small_length));
  ("-version", Arg.Unit
     (fun () ->
       print_endline ("hacha "^Version.version) ;
       print_endline ("library directory: "^Mylib.static_libdir) ;
       exit 0),
   "show hacha version and library directory") ;
     ("-v", Arg.Unit (fun () -> incr verbose),
        ", verbose flag") ]

  and usage = "Usage: hacha [options] htmlfile" in

  Arg.parse spec (fun s -> filename := Some s) usage ;
  let filename =
    match !filename with
    | None -> raise (Error "No argument given")
    | Some f -> f  in

  let chan =
    try open_in filename
    with Sys_error s -> raise (Error ("File error: "^s)) in

  let module Config = struct
    let verbose = !verbose
    let name_in = filename
    let name_out = !outname
    let toc_style = !toc_style
    let svg_arrows = !svg_arrows
    let cross_links = !cross_links 
    let small_length = !small_length
  end in
  let module C = Cut.Make(Config) in
  let buf = Lexing.from_channel chan in
  Location.set filename buf ;
  C.start_phase () ;
  ignore (C.do_lex buf) ;  
  close_in chan ;
  Location.restore () ;  
  let chan = try open_in filename with Sys_error s -> raise (Error ("File error: "^s)) in
  let buf = Lexing.from_channel chan in
  Location.set filename buf ;
  C.start_phase () ;
  let some_links = C.do_lex buf in
  close_in chan ;
  if !log then Cross.dump (C.real_name (C.base^".hrf")) C.check_changed ;
  if some_links && !svg_arrows then begin
    Mysys.copy_from_lib_to_dir Mylib.libdir C.dir "previous_motif.svg" ;
    Mysys.copy_from_lib_to_dir Mylib.libdir C.dir "next_motif.svg" ;
    Mysys.copy_from_lib_to_dir Mylib.libdir C.dir "contents_motif.svg"
  end
  else if some_links then begin
    Mysys.copy_from_lib_to_dir Mylib.libdir C.dir "previous_motif.gif" ;  
    Mysys.copy_from_lib_to_dir Mylib.libdir C.dir "next_motif.gif" ;  
    Mysys.copy_from_lib_to_dir Mylib.libdir C.dir "contents_motif.gif"
  end
;;


let _ = try
  main () ;
with
| Error s  ->
    prerr_endline s ;
    prerr_endline "Adios" ;
    exit 2
| Cut.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading HTML: "^s) ;
    prerr_endline "Adios" ;
    exit 2
| Misc.Fatal s ->
    Location.print_pos () ;
    prerr_endline
      ("Fatal error: "^s^" (please report to Luc.Maranget@inria.fr)") ;
    prerr_endline "Adios" ;
    exit 2
(*
|  x ->
    Location.print_pos () ;
    prerr_endline
      ("Fatal error: spurious exception "^Printexc.to_string x^
       " (please report to Luc.Maranget@inria.fr") ;
    prerr_endline "Adios" ;
    exit 2
*)
;;

exit 0;;

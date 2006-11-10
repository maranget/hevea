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

let header = "$Id: cutmain.ml,v 1.23 2006-11-10 08:28:46 maranget Exp $" 

exception Error of string
;;

let filename = ref ""
let outname = ref "index.html"
let log = ref false
let toc_style = ref Cut.Normal
let cross_links = ref true
let verbose = ref 0

  
let main () =
  Arg.parse
    [("-o", Arg.String (fun s -> outname := s),
       "filename, make hacha output go into file 'filename' (defaults to index.html)");
     ("-tocbis", Arg.Unit (fun () -> toc_style := Cut.Both),
       ", duplicate table of contents at the begining of files");      
     ("-tocter", Arg.Unit (fun () -> toc_style := Cut.Special),
       ", Insert most of table of contents at the beginning of files");
     ("-nolinks", Arg.Unit (fun () -> cross_links := false),
       ", Suppress the prevous/up/next links in generated pages");
     ("-hrf", Arg.Unit (fun () -> log := true),
        ", output a log file showing the association from local anchors to files"); 
     ("-v", Arg.Unit (fun () -> incr verbose),
        ", verbose flag")    ]
     (fun s -> filename := s) ("hacha "^Version.version);
  let chan = try open_in !filename with Sys_error s -> raise (Error ("File error: "^s)) in

  let module Config = struct
    let verbose = !verbose
    let name_in = !filename
    let name_out = !outname
    let toc_style = !toc_style
    let cross_links = !cross_links 
  end in
  let module C = Cut.Make(Config) in
  let buf = Lexing.from_channel chan in
  Location.set !filename buf ;
  C.start_phase () ;
  ignore (C.do_lex buf) ;  
  close_in chan ;
  Location.restore () ;  
  let chan = try open_in !filename with Sys_error s -> raise (Error ("File error: "^s)) in
  let buf = Lexing.from_channel chan in
  Location.set !filename buf ;
  C.start_phase () ;
  let some_links = C.do_lex buf in
  close_in chan ;
  if !log then Cross.dump (C.real_name (C.base^".hrf")) C.check_changed ;
  if some_links then begin
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
      ("Fatal error: "^s^" (please report to Luc.Maranget@inria.fr") ;
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

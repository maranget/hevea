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

let header = "$Id: cutmain.ml,v 1.22 2006-11-09 21:36:45 maranget Exp $" 

exception Error of string
;;

let filename = ref ""

let outname = ref "index.html"

let log = ref false

  
let main () =
  Arg.parse
    [("-o", Arg.String (fun s -> outname := s),
       "filename, make hacha output go into file 'filename' (defaults to index.html)");
     ("-tocbis", Arg.Unit (fun () -> Cut.toc_style := Cut.Both),
       ", duplicate table of contents at the begining of files");      
     ("-tocter", Arg.Unit (fun () -> Cut.toc_style := Cut.Special),
       ", Insert most of table of contents at the beginning of files");
     ("-nolinks", Arg.Unit (fun () -> Cut.cross_links := false),
       ", Suppress the prevous/up/next links in generated pages");
     ("-hrf", Arg.Unit (fun () -> log := true),
        ", output a log file showing the association from local anchors to files"); 
     ("-v", Arg.Unit (fun () -> incr Cut.verbose),
        ", verbose flag")    ]
     (fun s -> filename := s) ("hacha "^Version.version);
  let chan = try open_in !filename with Sys_error s -> raise (Error ("File error: "^s)) in
  let buf = Lexing.from_channel chan in
  Location.set !filename buf ;
  Cut.start_phase !filename !outname ;
  Cut.main buf ;
  Location.restore () ;
  let chan = try open_in !filename with Sys_error s -> raise (Error ("File error: "^s)) in
  let buf = Lexing.from_channel chan in
  Location.set !filename buf ;
  Cut.start_phase !filename !outname ;
  Cut.main buf ;
  if !log then Cross.dump (!Cut.name^".hrf") Cut.check_changed
;;


let copy_gifs () =
  try
    Mysys.copy_from_lib_to_dir Mylib.libdir !Cut.base "previous_motif.gif" ;  
    Mysys.copy_from_lib_to_dir Mylib.libdir !Cut.base "next_motif.gif" ;  
    Mysys.copy_from_lib_to_dir Mylib.libdir !Cut.base "contents_motif.gif"
  with
  | Mysys.Error s ->
      Location.print_pos () ;
      prerr_endline s

let _ = try
  main () ;
  if !Cut.some_links then
    copy_gifs ()
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

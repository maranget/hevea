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

let header = "$Id: cutmain.ml,v 1.16 2001-10-19 18:35:47 maranget Exp $" 

exception Error of string
;;

let filename = ref ""

let outname = ref "index.html"

let log = ref false

  
let main () =
  Arg.parse
    [("-o", Arg.String (fun s -> outname := s),
       "filename, make htmlcut output go into file ``filename'' (defaults to index.html)");
     ("-francais", Arg.Unit (fun () -> Cut.language := "fra"),
       ", French mode");      
     ("-tocbis", Arg.Unit (fun () -> Cut.tocbis := true),
       ", Add small table of contents at the begining of files");      
     ("-hrf", Arg.Unit (fun () -> log := true),
        ", output a log file showing the association from local anchors to files"); 
     ("-v", Arg.Unit (fun () -> incr Cut.verbose),
        ", verbose flag")    ]
     (fun s -> filename := s) ("hacha "^Version.version);
  let base = Filename.basename !filename in
  Cut.name :=
     (try Filename.chop_extension base with Invalid_argument _ -> base) ;
  let chan = try open_in !filename with Sys_error s -> raise (Error ("File error: "^s)) in
  let buf = Lexing.from_channel chan in
  Location.set !filename buf ;
  Cut.start_phase !outname ;
  Cut.main buf ;
  Location.restore () ;
  let chan = try open_in !filename with Sys_error s -> raise (Error ("File error: "^s)) in
  let buf = Lexing.from_channel chan in
  Location.set !filename buf ;
  Cut.start_phase !outname ;
  Cut.main buf ;
  if !log then Cross.dump (!Cut.name^".hrf")
;;


let copy_gifs () =
  try
    Mysys.copy_from_lib Mylib.libdir "previous_motif.gif" ;  
    Mysys.copy_from_lib Mylib.libdir "next_motif.gif" ;  
    Mysys.copy_from_lib Mylib.libdir "contents_motif.gif"
  with
  | Mysys.Error s ->
      Location.print_pos () ;
      prerr_endline s

let _ = try
  main () ;
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
|  x ->
    Location.print_pos () ;
    prerr_endline
      ("Fatal error: spurious exception "^Printexc.to_string x^
       " (please report to Luc.Maranget@inria.fr") ;
    prerr_endline "Adios" ;
    exit 2
;;

exit 0;;

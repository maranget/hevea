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


exception Error of string


let default_outname = "index.html"
and default_small_length = 1024

let filename = ref None
and outname = ref default_outname
and log = ref false
and toc_style = ref Cut.Normal
and svg_arrows = ref true
and cross_links = ref true
and verbose = ref 0
and small_length = ref default_small_length


let main () =
  let usage =
    "Usage: hacha [OPTION...] HTML-FILE\n\
     \n\
     Split the Hevea-generated HTML-FILE into several HTML files at\n\
     its logical boundaries (chapters or sections) while keeping all\n\
     cross-references intact.  Propagate headers, footers, footnotes,\n\
     and the like into the split HTML files.\n\
     \n\
     Options:"
  and spec =
    ["-o", Arg.Set_string outname,
     "FILENAME redirect Hacha output into FILENAME (default: \"" ^ default_outname ^ "\")";
     "-tocbis", Arg.Unit (fun () -> toc_style := Cut.Both),
     " duplicate table of contents at the beginning of files";
     "-tocter", Arg.Unit (fun () -> toc_style := Cut.Special),
     " insert most of the table of contents at the beginning of files";
     "-no-svg-arrows", Arg.Clear svg_arrows,
     " use GIF arrows for the previous/up/next links in generated pages";
     "-nolinks", Arg.Clear cross_links,
     " suppress the previous/up/next links in generated pages";
     "-hrf", Arg.Set log,
     " write a log file that shows the association of local anchors to files";
     "-rsz", Arg.Set_int small_length,
     (Printf.sprintf "SIZE set SIZE (default: %i) of leaves in rope implementation" default_small_length);
     "-v", Arg.Unit (fun () -> incr verbose),
     " report progress";
     "-version",
     Arg.Unit
       (fun () ->
         print_endline ("hacha " ^ Version.version);
         print_endline ("library directory: " ^ Mylib.static_libdir);
         exit 0),
     " output version information, library directory and exit"]
  in
  Arg.parse (Arg.align spec) (fun s -> filename := Some s) usage;
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

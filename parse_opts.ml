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

open Misc

let header = "$Id: parse_opts.ml,v 1.9 1999-03-03 18:08:49 maranget Exp $" 


let files = ref []
;;

let add_input s =
  files := s :: !files
;;

type language = Francais | English
;;

let language = ref English
and symbols = ref true
and iso = ref true
;;

let read_idx = ref false
;;

let except = ref []
;;

let path = ref []
;;

let outname = ref ""
;;

let _ = Arg.parse
    [("-v", Arg.Unit (fun () -> readverb := !readverb + 1),
       ", verbose flag, can be repeated to increase verbosity") ;
     ("-s", Arg.Unit (fun () -> silent := true),
       ", suppress warnings") ;
     ("-e", Arg.String (fun s -> except := s :: !except),
       "filename, prevent file ``filename'' from being read") ;
     ("-idx",Arg.Unit (fun () -> read_idx := true),
       ", attempt to read .idx file (useful if indexing is non-standard)") ;
     ("-francais",Arg.Unit (fun () -> language := Francais),
       ", french mode") ;
     ("-nosymb",Arg.Unit (fun () -> symbols := false),
       ", do not output symbol fonts") ;
     ("-noiso",Arg.Unit (fun () -> iso := false),
       ", do not output iso characters above 127") ;
     ("-I", Arg.String (fun s -> path := s :: !path),
       "dir, add directory ``dir'' to search path") ;
     ("-o", Arg.String (fun s -> outname := s),
       "filename, make hevea output go into file ``filename''")
    ]
    (add_input)
   ("hevea "^Version.version)
;;

let warning s =
  if not !silent || !verbose > 0 then begin
    Location.print_pos () ;
    prerr_string "Warning: " ;
    prerr_endline s
  end
;;

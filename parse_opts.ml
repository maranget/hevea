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

let header = "$Id: parse_opts.ml,v 1.18 1999-09-11 18:02:51 maranget Exp $" 


let files = ref []
;;

let add_input s =
  files := s :: !files
;;

type language = Francais | English
;;

type destination = Html | Text | Info
;;
let mathml = ref false
and entities = ref false
;;

let language = ref English
and symbols = ref true
and iso = ref true
and pedantic = ref false
and destination = ref Html
;;

let width = ref 72
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
       ", use HTML entities in place of isolatin1 non-ascii characters") ;
     ("-pedantic",Arg.Unit (fun () -> pedantic := true),
       ", be pedantic in interpreting HTML 4.0 transitional definition") ;
     ("-I", Arg.String (fun s -> path := s :: !path),
       "dir, add directory ``dir'' to search path") ;
     ("-mathml",Arg.Unit (fun() -> mathml := true),
       ", produces MathML output for equations, very experimental");
     ("-entities",Arg.Unit (fun() -> entities := true),
       ", produces HTML 4.0 entities and unicode characters references for symbols, very experimental");
     ("-text",Arg.Unit (fun () -> symbols := false; destination := Text),
       ", output plain text");
     ("-info",Arg.Unit (fun () -> symbols := false; destination := Info),
       ", outputinfo file(s)");
     ("-w", Arg.String (fun s -> width := int_of_string s),
      "width, set the output width for text or info output");
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

let base_in,name_in,styles = match !files with
| [] -> "","",[]
| x :: rest ->
    if Filename.check_suffix x ".hva" then
      "","", !files
    else
      let base_file = Filename.basename x in
      try
        let base =
          if Filename.check_suffix base_file ".tex" then
            Filename.chop_extension base_file
          else
            base_file in
        base,x,rest
    with Invalid_argument _ -> base_file, x,rest


let base_out = match !outname with
| "" -> begin match base_in with
  | "" -> ""
  | _  -> Filename.basename base_in
end      
| name ->
    let suff = match !destination with
    | Html -> ".html"
    | Text -> ".txt"
    | Info -> ".info"
    in
    if Filename.check_suffix name suff then
      Filename.chop_suffix name suff
    else
      try
        Filename.chop_extension name
      with Invalid_argument _ -> name

let name_out = match !outname with
| "" -> begin match base_in with
  | "" -> ""
  | x  -> begin
      match !destination with
      |	Html ->x^".html"
      |	Text ->x^".txt"
      |	Info ->x^".info"
  end
end    
| x  -> x


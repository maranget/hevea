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

let header = "$Id: myfiles.ml,v 1.18 1999-09-08 20:26:46 maranget Exp $" 
open Misc

exception Error of string
;;
exception Except
;;

let etable = Hashtbl.create 17
;;


List.iter (fun name -> Hashtbl.add etable name ()) !Parse_opts.except
;;

let is_except name =
  try Hashtbl.find etable name ; true with Not_found -> false
;;

let tex_path = "." :: !Parse_opts.path @
  [Filename.concat
    Mylib.libdir 
     (match !Parse_opts.destination with
     | Parse_opts.Html -> "html"
     | Parse_opts.Text -> "text"
     | Parse_opts.Info -> "info") ;
    Mylib.libdir]
;;

exception Found of (string * in_channel)
;;

let do_open_tex filename =
  try
    List.iter (fun dir ->
      try
        let full_name = Filename.concat dir filename in
        if !verbose > 1 then prerr_endline ("Trying: "^full_name) ;
        let r = open_in full_name in
        if !verbose > 1 then prerr_endline ("Opening: "^full_name) ;
        raise (Found (full_name,r))
      with Sys_error s ->
        if !verbose > 1 then prerr_endline ("Failed: "^s))
    tex_path ;
    raise (Error ("Cannot open file: "^filename))
  with Found r -> r
;;



let open_tex filename =
  if !verbose > 1 then
    prerr_endline ("Searching file: "^filename) ;
  if is_except filename then raise Except ;
  if Filename.is_implicit filename then
    if
      Filename.check_suffix filename ".tex" ||
      Filename.check_suffix filename ".hva"
    then do_open_tex filename
      else
        try
            let name = filename^".tex" in
            if is_except name then raise Except ;
            do_open_tex name
        with Error _ -> do_open_tex filename
   else
    try
      if Filename.check_suffix filename ".tex" then filename,open_in filename
      else
        try (filename^".tex"),open_in (filename^".tex") with
        Sys_error _ -> filename,open_in filename
    with Sys_error _ -> raise (Error ("Cannot open: "^filename))


exception Return of bool

let diff_chan chan1 chan2 =
  try
    while true do
      let c1 =
        try input_char chan1 with End_of_file -> begin
          try
            let _ = input_char chan2 in
            raise (Return true)
          with End_of_file -> raise (Return false)
        end in
      let c2 =
        try input_char chan2 with End_of_file -> raise (Return true) in
      if c1 <> c2 then
        raise (Return true)
    done ;
    assert false
  with Return r -> r

let changed tmp_name name =
  try
    let true_chan = open_in name in
    let tmp_chan =
      try open_in tmp_name
      with Sys_error _ -> begin
        close_in true_chan ;
        raise
          (Misc.Fatal
             ("Cannot reopen temporary image file: "^tmp_name))
      end in
    let r = diff_chan true_chan tmp_chan in
    close_in true_chan ;
    close_in tmp_chan ;
    r
  with Sys_error _ -> true


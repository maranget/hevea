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

let header = "$Id: myfiles.ml,v 1.15 1999-04-08 09:24:36 maranget Exp $" 
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

let tex_path = "." :: !Parse_opts.path @ [Mylib.libdir]
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
    if Filename.check_suffix filename ".tex" then do_open_tex filename
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

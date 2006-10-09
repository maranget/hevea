(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: esponja.ml,v 1.9 2006-10-09 08:25:16 maranget Exp $           *)
(***********************************************************************)

open Mysys

let pess  = ref false
and move  = ref true
;;


let process cls in_name input output =
  let rec do_rec lexbuf = match Htmlparse.main cls lexbuf with
  | [] -> ()
  | ts ->
      if !pess then
        Pp.trees output (Explode.trees ts)
      else
        Ultra.main output ts ;
      do_rec lexbuf in
  try
    let lexbuf = Lexing.from_channel input in
    Location.set in_name lexbuf ;
    Emisc.reset () ;
    do_rec lexbuf ;
    Location.restore () ;
    true
  with
  | Htmllex.Error s ->
      if !Emisc.verbose > 0 then
        output_char stderr '\n' ;
      Location.print_fullpos () ;
      Printf.fprintf stderr "Lexer error: %s\n" s ;
      Location.restore () ;
      false
  | Htmlparse.Error s ->
      if !Emisc.verbose > 0 then
        output_char stderr '\n' ;
      Location.print_fullpos () ;
      Printf.fprintf stderr "Parser error: %s\n" s ;
      Htmllex.ptop () ;
      Htmllex.reset () ;
      Location.restore () ;
      false
  | e ->
      Location.restore () ;
      raise e

let classes in_name input =
  try
    let lexbuf = Lexing.from_channel input in
    Location.set in_name lexbuf ;
    Emisc.reset () ;
    let cls = Htmllex.classes lexbuf in
    Location.restore () ;
    Some cls
  with
  | Htmllex.Error s ->
      if !Emisc.verbose > 0 then
        output_char stderr '\n' ;
      Location.print_fullpos () ;
      Printf.fprintf stderr "Lexer error: %s\n" s ;
      Location.restore () ;
      None
  | e ->
      Location.restore () ;
      raise e

let file in_name =
  if !Emisc.verbose > 0 then begin
    Printf.fprintf stderr "Optimizing file: %s... " in_name ;
    flush stderr
  end ;
  let out_name =
    Filename.concat
      (Filename.dirname in_name)
      (Filename.basename in_name ^ ".esp")
  in
  begin try
    let input = open_in in_name in
    let cls =
      if !pess then None
      else try classes in_name input
      with e -> close_in input ; raise e in
    close_in input ;
    let input = open_in in_name in
    let out =
      try open_out out_name
      with Sys_error _ as e ->
        close_in input ; raise e in
    let size_in = in_channel_length input in
    let ok =
      try process cls in_name input out
      with e -> close_in input ; close_out out ; raise e in
    close_in input ;
    flush out ;
    let size_out = out_channel_length out in
    close_out out ;
    if ok && size_in > size_out then
      begin if !move then rename out_name in_name end
    else
      remove out_name ;
    if !Emisc.verbose > 0  && ok then begin
      Printf.fprintf stderr "saved %d -> %d, %0.2f%%"
        size_in size_out
        ((float (size_in-size_out) *. 100.0) /.
         float size_in) ;
      prerr_endline ""
    end ;
    ok        
  with
  | Sys_error msg ->
      Printf.fprintf stderr "File error: %s\n" msg ;
      false
  | e ->
      remove out_name ;
      raise e
  end

(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: esponja.ml,v 1.4 2001-05-25 09:20:43 maranget Exp $"            *)
(***********************************************************************)
open Mysys

let pess  = ref false
and move  = ref true
;;

let arg = ref []
;;

Arg.parse
  ["-u", Arg.Set pess, "pessimize" ;
  "-v", Arg.Unit (fun () -> incr Ultra.verbose),"be verbose" ;
  "-n", Arg.Unit (fun () -> move := false ; Ultra.verbose := 1),
    "do not change files"]
  (fun s -> arg :=  s :: !arg)
  ("Usage: esponja [option*] < infile > outfile,\n or    esponja [option*] files+
options are:")
;;

let length input  = in_channel_length input

let process in_name input output =
  let rec do_rec lexbuf = match Htmlparse.main lexbuf with
  | [] -> ()
  | ts ->
      if !pess then
        Pp.trees output (Explode.trees ts)
      else
        Pp.trees output (Ultra.main ts) ;
      do_rec lexbuf in
  try
    let lexbuf = Lexing.from_channel input in
    Location.set in_name lexbuf ;
    do_rec lexbuf ;
    Location.restore () ;
    true
  with
  | Htmllex.Error s ->
      if !Ultra.verbose > 0 then
        output_char stderr '\n' ;
      Location.print_fullpos () ;
      Printf.fprintf stderr "Lexer error: %s\n" s ;
      false
  | Htmlparse.Error s ->
      if !Ultra.verbose > 0 then
        output_char stderr '\n' ;
      Location.print_fullpos () ;
      Printf.fprintf stderr "Parser error: %s\n" s ;
      false


let main () =
  try
    begin match !arg with
    | [] ->
        ignore (process "" stdin stdout)
    | files ->
        List.iter
          (fun in_name ->
            if !Ultra.verbose > 0 then begin
              Printf.fprintf stderr "Processing file: %s... " in_name ;
              flush stderr
            end ;
            let out_name =
              Filename.concat
                (Filename.dirname in_name)
                (Filename.basename in_name ^ ".esp")
            in
            begin try
              let input = open_in in_name in
              let out =
                try open_out out_name
                with Sys_error _ as e ->
                  close_in input ; raise e in
              let size_in = in_channel_length input in
              let ok = process in_name input out in
              flush out ;
              let size_out = out_channel_length out in
              close_in input ;
              close_out out ;
              if ok && size_in > size_out then
                begin if !move then rename out_name in_name end
              else
                remove out_name ;
              if !Ultra.verbose > 0  && ok then begin
                Printf.fprintf stderr "saved %d -> %d %0.2f"
                  size_in size_out
                  ((float (size_in-size_out) *. 100.0) /.
                   float size_in) ;
                  prerr_endline ""
              end
              
            with
            | Sys_error msg ->
                Printf.fprintf stderr "File error: %s\n" msg
            | e ->
                remove out_name ;
                raise e
            end)
          files
    end ;
    exit 0
  with
  | e ->
      Printf.fprintf stderr "Unexpected exception: %s\n"
        (Printexc.to_string e) ;
      exit 1
;;

main ()
;;

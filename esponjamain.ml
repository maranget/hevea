(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: esponjamain.ml,v 1.2 2001-05-25 17:23:10 maranget Exp $           *)
(***********************************************************************)

open Mysys
open Esponja

let arg = ref []
;;

Arg.parse
  ["-u", Arg.Set pess, "pessimize" ;
  "-v", Arg.Unit (fun () -> incr Ultra.verbose),"be verbose" ;
  "-n", Arg.Unit (fun () -> move := false ; incr Ultra.verbose),
    "do not change files"]
  (fun s -> arg :=  s :: !arg)
  ("Usage: esponja [option*] < infile > outfile,\n or    esponja [option*] files+
options are:")
;;

let main () =
  try
    begin match !arg with
    | [] ->
        ignore (process "" stdin stdout)
    | files ->
        List.iter (fun f -> ignore (Esponja.file f)) files
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

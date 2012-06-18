(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: esponja.ml,v 1.12 2012-06-18 13:14:41 suzanne Exp $           *)
(***********************************************************************)

open Mysys

let arg = ref []
;;

Arg.parse
  ["-u", Arg.Set Esp.pess, "pessimize" ;
  "-v", Arg.Unit (fun () -> incr Emisc.verbose),"be verbose" ;
  "-n", Arg.Unit (fun () -> Esp.move := false ; incr Emisc.verbose),
    "do not change files"]
  (fun s -> arg :=  s :: !arg)
  ("Usage: esponja [option*] < infile > outfile,\n or esponja [option*] files\noptions are:")
;;

let main () =
  try
    begin match !arg with
    | [] ->
        let ok = Esp.process None "" stdin stdout in
        exit (if ok then 0 else 2)
    | files ->
        List.iter (fun f -> ignore (Esp.file f)) (List.rev files) ;
        exit 0
    end
  with
  | e ->
      Printf.fprintf stderr "Unexpected exception: %s\n"
        (Printexc.to_string e) ;
      exit 2
;;

main ()
;;

(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Printf

let arg = ref []
let pess = ref false
let move = ref true
let small_length = ref 1024

let () =
  Arg.parse
    [
     ("-version", Arg.Unit
     (fun () ->
       print_endline ("esponja "^Version.version) ;
       print_endline ("library directory: "^Mylib.static_libdir) ;
       exit 0),
     "show version and exit") ;
     ("-rsz", Arg.Int (fun i -> small_length := i),
      (sprintf
         "size of leaves in rope implementation (default %i)"
         !small_length)) ;
     "-u", Arg.Set pess, "pessimize" ;
     "-v", Arg.Unit (fun () -> incr Emisc.verbose),"be verbose" ;
     "-n", Arg.Unit (fun () -> move := false ; incr Emisc.verbose),
     "do not change files"; ]
    (fun s -> arg :=  s :: !arg)
  ("Usage: esponja [option*] files\noptions are:")
;;

module E =
  Esp.Make
    (struct
      let pess = !pess
      let move = !move
      let small_length = !small_length
    end)

let process name = try E.file name with Esp.Failed -> ()

let main () =
(*  try *)
    List.iter process (List.rev !arg) ;
    exit 0
(*  with
  | e ->
      Printf.fprintf stderr "Unexpected exception: %s\n"
        (Printexc.to_string e) ;
      exit 2
*)
;;

main ()
;;

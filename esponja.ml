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

let arg = ref []
let pess = ref false
let move = ref true

let () =
  Arg.parse
    ["-u", Arg.Set pess, "pessimize" ;
     "-v", Arg.Unit (fun () -> incr Emisc.verbose),"be verbose" ;
     "-n", Arg.Unit (fun () -> move := false ; incr Emisc.verbose),
     "do not change files"]
    (fun s -> arg :=  s :: !arg)
  ("Usage: esponja [option*] files\noptions are:")
;;

module E =
  Esp.Make
    (struct
      let pess = !pess
      let move = !move
    end)

let process name = try E.file name with Esp.Failed -> ()

let main () =
  try
    List.iter process (List.rev !arg) ;
    exit 0
  with
  | e ->
      Printf.fprintf stderr "Unexpected exception: %s\n"
        (Printexc.to_string e) ;
      exit 2
;;

main ()
;;

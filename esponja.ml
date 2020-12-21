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


let default_small_length = 1024

let arg = ref []
and pess = ref false
and move = ref true
and small_length = ref default_small_length

let () =
  let usage =
    "Usage: esponja [OPTION...] HTML-FILE...\n\
     \n\
     Optimize HTML-FILE by factoring common CSS-styles and removing\n\
     unused CSS-class definitions.\n\
     \n\
     Options:"
  and spec =
    ["-u", Arg.Set pess,
     " pessimize optimizer";
     "-n", Arg.Unit (fun () -> move := false ; incr Emisc.verbose),
     " dry run - do not change files";
     "-rsz", Arg.Set_int small_length,
     (Printf.sprintf "SIZE set SIZE (default: %i) of leaves in rope implementation" default_small_length);
     "-v", Arg.Unit (fun () -> incr Emisc.verbose),
     " report progress";
     "-version",
     Arg.Unit
       (fun () ->
         print_endline ("esponja " ^ Version.version);
         print_endline ("library directory: " ^ Mylib.static_libdir);
         exit 0),
     " output version information, library directory and exit"]
  in
    Arg.parse
      (Arg.align spec)
      (fun s -> arg :=  s :: !arg)
      usage;

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

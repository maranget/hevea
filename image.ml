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

let header = "$Id: image.ml,v 1.7 1998-07-21 11:18:30 maranget Exp $" 
open Parse_opts

let base = ref "image"
;;

let count = ref 0
;;

let buff = ref (Out.create_chan (open_out "/dev/null"))
;;

let start () =
  buff := Out.create_buff ()
;;


let put s = Out.put !buff s
and put_char c = Out.put_char !buff c
;;

let open_chan () =
  let chan = open_out (!base^".image.tex") in
  Out.to_chan chan !buff ;
  buff := Out.create_chan chan ;
  Out.put !buff "\\pagestyle{empty}\n";

and close_chan () =
  Out.put !buff "\\end{document}\n" ;
  Out.close !buff
;;


let my_string_of_int n =
  let r0 = n mod 10 and q0 = n / 10 in
  let r1 = q0 mod 10 and q1 = q0 / 10 in
  let r2 = q1 mod 10 in
  string_of_int r2^string_of_int r1^string_of_int r0
;;


let page () =
  let n = !count in
  if !verbose > 0 then begin
    Location.print_pos ();
    Printf.fprintf stderr "dump image number %d" (n+1) ;
    prerr_endline ""
  end ;
  (if n = 0 then
    open_chan()) ;
  incr count ;
  !base^my_string_of_int !count^".gif"
;;

let open_image () = ()
and close_image () = ()
;;

let dump s_open image  lexbuf =
  open_image ();
  Out.put !buff s_open ;
  image lexbuf
;;

let finalize () =  close_chan()
;;

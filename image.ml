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

let header = "$Id: image.ml,v 1.29 2006-07-26 06:33:19 maranget Exp $" 
open Misc

let base = Parse_opts.base_out
;;

let count = ref 0
;;

let buff = ref (Out.create_null ())
;;

let active = ref false
;;

let start () =
  active := true ;
  count := 0 ;
  buff := Out.create_buff ()
;;

let active_stack = Stack.create "Image.active" 

let stop () =
  Stack.push active_stack !active ;
  active := false

and restart () =
  if Stack.empty active_stack then
    active := true
  else
    active := Stack.pop active_stack

let put s = if !active then Out.put !buff s

and put_char c = if !active then Out.put_char !buff c


let tmp_name =
  if Parse_opts.filter then "" else base ^ ".image.tex.new"

let open_chan () =
  let chan = open_out tmp_name in
  Out.to_chan chan !buff ;
  buff := Out.create_chan chan

and close_chan () = Out.close !buff

let my_string_of_int n = Printf.sprintf "%03d" n

let page () =
  let n = !count in
  if !verbose > 0 then begin
    Location.print_pos ();
    Printf.fprintf stderr "dump image number %d" (n+1) ;
    prerr_endline ""
  end ;
  if n = 0 then open_chan () ;
  incr count ;
  put ("\n\\clearpage% page: "^string_of_int n^"\n")
;;

let dump s_open image  lexbuf =
  Out.put !buff s_open ;
  image lexbuf
;;

let finalize check = 
  active := false ;
  if !count > 0 then begin
    close_chan() ;
    if check then begin
      let true_name = Filename.chop_suffix tmp_name ".new" in
      if Myfiles.changed tmp_name true_name then begin
        Mysys.rename tmp_name true_name ;
        Misc.message
          ("HeVeA Warning: images may have changed, run ``imagen "^
           Misc.get_image_opt ()^" "^base^"''");
        true
      end else begin
        Mysys.remove tmp_name ;
        false
      end
    end else
      false        
  end else
    false

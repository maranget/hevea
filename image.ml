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

let header = "$Id: image.ml,v 1.14 1999-06-18 13:25:01 maranget Exp $" 
open Misc

let base = Parse_opts.base_out
;;

let count = ref 0
;;

let buff = ref (Out.create_null ())
;;

let start () =
  buff := Out.create_buff ()
;;


let put s = Out.put !buff s
and put_char c = Out.put_char !buff c
;;

let tmp_name = match base with
| "" -> "image.tex.new"
| _ -> base ^ ".image.tex.new"

let open_chan () =
  let chan = open_out tmp_name in
  Out.to_chan chan !buff ;
  buff := Out.create_chan chan


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
  if n = 0 then open_chan () ;
  incr count ;
  base^my_string_of_int !count^".gif"
;;

let dump s_open image  lexbuf =
  Out.put !buff s_open ;
  image lexbuf
;;


exception Return of bool

let diff_chan chan1 chan2 =
  try
    while true do
      let c1 =
        try input_char chan1 with End_of_file -> begin
          try
            let _ = input_char chan2 in
            raise (Return true)
          with End_of_file -> raise (Return false)
        end in
      let c2 =
        try input_char chan2 with End_of_file -> raise (Return true) in
      if c1 <> c2 then
        raise (Return true)
    done ;
    assert false
  with Return r -> r

let changed tmp_name name =
  try
    let true_chan = open_in name in
    let tmp_chan =
      try open_in tmp_name
      with Sys_error _ -> begin
        close_in true_chan ;
        raise
          (Misc.Fatal
             ("Cannot reopen temporary image file: "^tmp_name))
      end in
    let r = diff_chan true_chan tmp_chan in
    close_in true_chan ;
    close_in tmp_chan ;
    r
  with Sys_error _ -> true


let finalize () = 
  if !count > 0 then begin
    close_chan() ;
    let true_name = Filename.chop_suffix tmp_name ".new" in
    if changed tmp_name true_name then
      Sys.rename tmp_name true_name
    else
      Sys.remove tmp_name
  end

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

let header = "$Id: location.ml,v 1.6 1998-07-21 11:18:37 maranget Exp $" 
let base = ref ""
;;

let set_base s = base := s
and get_base () = !base
;;

let stack = ref []
;;
let push s e = s := e:: !s
and pop s = match !s with
  [] -> failwith "Location : Empty stack"
| e::rs -> s := rs ; e
;;


let cur_line = ref (0,1)
;;

let curlexbuf = ref (Lexing.from_string "")
and curlexname = ref ""
;;

let get () = !curlexname
;;

let set name lexbuf =
  push stack (!curlexname,!curlexbuf,!cur_line) ;
  curlexname := name ;
  curlexbuf := lexbuf;
  cur_line := (0,1)
;;

let restore () =
  let name,lexbuf,line = pop stack in
  curlexname := name ;
  curlexbuf := lexbuf;
  cur_line := line
;;


let rec find_line file r = function
  0 -> r
| n ->
   find_line file
    (match input_char file with '\n' -> r+1 | _ -> r)
    (n-1)
;;

   
let print_pos () =
  try
    let file = open_in !curlexname
    and char_pos = Lexing.lexeme_start !curlexbuf
    and last_pos,last_line = !cur_line in
    let last_pos,last_line =
      if char_pos < last_pos then 0,1 else last_pos,last_line in
    seek_in file last_pos ;
    let nline =
       find_line file last_line (char_pos-last_pos) in
    close_in file ;
    cur_line := (char_pos,nline);
    prerr_string (!curlexname^":"^string_of_int nline^": ")
  with Sys_error s -> ()
;;


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
open Stack

let header = "$Id: location.ml,v 1.15 2000-01-21 18:48:58 maranget Exp $" 

type fileOption = No | Yes of in_channel
;;

let stack = Stack.create "location"
;;



let curlexbuf = ref (Lexing.from_string "")
and curlexname = ref ""
and curline = ref (0,1)
and curfile = ref No
;;

let save_state () =
  push stack (!curlexname,!curlexbuf,!curline,!curfile)
and restore_state () =
  let name,lexbuf,line,file = pop stack in
  curlexname := name ;
  curlexbuf := lexbuf;
  curline := line;
  curfile := file

type saved = (string * Lexing.lexbuf * (int * int)  * fileOption) Stack.saved

let close_file = function
  | Yes f -> close_in f
  | _ -> ()

let close_curfile () = close_file !curfile

let check () =
  save_state () ;
  let r = Stack.save stack in
  restore_state () ;
  r

and hot saved =
  save_state () ;
  Stack.finalize stack saved
    (fun (_,_,_,file) -> close_file file) ;
  Stack.restore stack saved ;  
  restore_state ()

let get () = !curlexname
;;

let set name lexbuf =
  save_state () ;
  curlexname := name ;
  curlexbuf := lexbuf;
  curfile :=
     begin match name with "" -> No
     | _ -> try Yes (open_in name) with Sys_error _ -> No
     end ;
  curline := (0,1)
;;

let restore () =
  close_curfile () ;
  restore_state ()
;;


let rec find_line file r = function
  0 -> r
| n ->
   find_line file
    (match input_char file with '\n' -> r+1 | _ -> r)
    (n-1)
;;

type t = string * int

let do_get_pos () =  match !curfile with
  No -> -1
| Yes file ->
    try 
      let  char_pos = Lexing.lexeme_start !curlexbuf
      and last_pos,last_line = !curline in
      let last_pos,last_line =
        if char_pos < last_pos then 0,1 else last_pos,last_line in
      seek_in file last_pos ;
      let nline =
        find_line file last_line (char_pos-last_pos) in
      curline := (char_pos,nline);
      nline
    with Sys_error _ -> -1
;;

let get_pos () =
  let nline = do_get_pos () in
  !curlexname,nline
;;

let do_print_pos (s,nline) =
  if nline >= 0 then
    prerr_string (s^":"^string_of_int nline^": ")
  else
    match s with
    | "" -> ()
    | _  ->  prerr_string (s^": ")

let print_pos () =
  do_print_pos (!curlexname,do_get_pos ())

and print_this_pos p = do_print_pos p

let stack_pos = ref []
;;

let push  x = stack_pos := x :: !stack_pos
and pop () = match !stack_pos with
  [] -> raise (Fatal "Empty stack_pos")
| x::r -> stack_pos := r ; x
;;

let push_pos () = push (!curlexname, do_get_pos ())
and pop_pos () = let _ = pop () in ()
and print_top_pos () =
  match !stack_pos with
    [] -> ()
  | x::_ -> do_print_pos x
;;

(* Deprecated 
let echo_from_start pos buff = match !curfile with
| No -> prerr_endline "No echo"
| Yes file ->
   try
      let save_pos = pos_in file in
      seek_in file 0 ;
      for i = 0 to pos do
        Out.put_char buff (input_char file)
      done ;
     seek_in file save_pos
   with Sys_error _ -> prerr_endline "Echo failed"
;;
*)

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

let header = "$Id: location.ml,v 1.18 2001-05-25 09:07:25 maranget Exp $" 

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
  let to_finalize = stack in
  Stack.restore stack saved ;  
  let _,_,_,file_now = Stack.top stack in
  Stack.finalize to_finalize
    (fun (_,_,_,file) -> file == file_now)
    (fun (_,_,_,file) -> close_file file) ;
  restore_state ()

let get () = !curlexname
;;

let set name lexbuf =
  save_state () ;
  curlexname := name ;
  curlexbuf := lexbuf;
  curfile :=
     begin match name with "" -> No
     | _ ->
         try Yes (open_in name) with Sys_error _ -> No
     end ;
  curline := (0,1)
;;

let restore () =
  close_curfile () ;
  restore_state ()
;;


let rec do_find_line file r c = function
  0 -> r,c
| n ->
   let cur = input_char file in
   do_find_line file
    (match cur with '\n' -> r+1 | _ -> r)
    (match cur with '\n' -> 1 | _ -> c+1)
    (n-1)
;;

let find_line file nline nchars = do_find_line file nline 1 nchars

type t = string * int * int

let do_get_pos () =  match !curfile with
  No -> -1,-1
| Yes file ->
    try 
      let  char_pos = Lexing.lexeme_start !curlexbuf
      and last_pos,last_line = !curline in
      let last_pos,last_line =
        if char_pos < last_pos then 0,1 else last_pos,last_line in
      seek_in file last_pos ;
(*      prerr_endline ("char_pos="^string_of_int char_pos) ; *)
      let nline,nchar =
        find_line file last_line (char_pos-last_pos) in
      curline := (char_pos,nline);
      nline,nchar
    with Sys_error _ -> -1,-1
;;

let get_pos () =
  let nline,nchars = do_get_pos () in
  !curlexname,nline,nchars
;;

let do_print_pos full (s,nline,nchars) =
  if nline >= 0 then
    prerr_string
      (s^":"^string_of_int nline^
       (if full then ":"^string_of_int nchars^": " else ": "))
  else
    match s with
    | "" -> ()
    | _  ->  prerr_string (s^": ")

let print_pos () =
  let nlines,nchars = do_get_pos () in
  do_print_pos false (!curlexname,nlines,nchars)

and print_fullpos () =
  let nlines,nchars = do_get_pos () in
  do_print_pos true (!curlexname,nlines,nchars)

and print_this_pos p = do_print_pos false p



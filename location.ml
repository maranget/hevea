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

let header = "$Id: location.ml,v 1.11 1999-06-18 13:25:10 maranget Exp $" 


type fileOption = No | Yes of in_channel
;;

let base = ref ""
;;

let set_base s = base := s
and get_base () = !base
;;

let stack = ref []
;;

let push s e = s := e:: !s
and pop s = match !s with
  [] -> raise (Misc.Fatal "Location : Empty stack")
| e::rs -> s := rs ; e
;;



let curlexbuf = ref (Lexing.from_string "")
and curlexname = ref ""
and curline = ref (0,1)
and curfile = ref No
;;

let close_curfile () = match !curfile with
| Yes f -> close_in f
| _ -> ()

let get () = !curlexname
;;

let set name lexbuf =
  push stack (!curlexname,!curlexbuf,!curline,!curfile) ;
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
  let name,lexbuf,line,file = pop stack in
  curlexname := name ;
  curlexbuf := lexbuf;
  curline := line;
  curfile := file
;;


let rec find_line file r = function
  0 -> r
| n ->
   find_line file
    (match input_char file with '\n' -> r+1 | _ -> r)
    (n-1)
;;

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

let print_pos () =
  let nline = do_get_pos () in
  if nline >= 0 then
    prerr_string (!curlexname^":"^string_of_int nline^": ")
  else
    prerr_string (!curlexname^": ")
;;

let stack_pos = ref []
;;

let push  x = stack_pos := x :: !stack_pos
and pop () = match !stack_pos with
  [] -> raise (Misc.Fatal "Empty stack_pos")
| x::r -> stack_pos := r ; x
;;

let push_pos () = push (do_get_pos ())
and pop_pos () = let _ = pop () in ()
and print_top_pos () =
  prerr_string (!curlexname^":") ;
  match !stack_pos with
    [] -> prerr_string " "
  | x::_ -> prerr_string (string_of_int x^": ")
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

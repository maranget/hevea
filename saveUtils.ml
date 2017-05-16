(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Save module utilities *)

let verbose = ref 0 and silent = ref false
;;

let set_verbose s v =
  silent := s ; verbose := v
;;

exception Error of string
;;
exception Delim of string
;;

let seen_par = ref false
;;


let brace_nesting = ref 0
and arg_buff = Out.create_buff ()
and echo_buff = Out.create_buff ()
and tag_buff = Out.create_buff ()
;;

  
let echo = ref false
;;

let get_echo () = echo := false ; Out.to_string echo_buff
and start_echo () = echo := true ; Out.reset echo_buff
;;

let empty_buffs () =
  brace_nesting := 0 ; Out.reset arg_buff ;
  echo := false ; Out.reset echo_buff ;
  Out.reset tag_buff 
;;

let error s =
  empty_buffs () ;
  raise (Error s)
;;

let my_int_of_string s =
  try int_of_string s
  with Failure _ ->
    error ("Integer argument expected: ``"^s^"''")

exception Eof
;;
exception LimitEof of Misc.limits option
;;
exception NoOpt
;;

let put_echo s =
  if !echo then Out.put echo_buff s
and put_echo_char c =
  if !echo then Out.put_char echo_buff c
and blit_echo lb =
  if !echo then Out.blit echo_buff lb
;;

let put_both s =
  put_echo s ; Out.put arg_buff s
;;
let blit_both lexbuf =
  blit_echo lexbuf ; Out.blit arg_buff lexbuf

let put_both_char c =
  put_echo_char c ; Out.put_char arg_buff c
;;


(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: myStack.ml,v 1.1 2007-02-08 17:48:28 maranget Exp $             *)
(***********************************************************************)
exception Fatal of string

type 'a t = {mutable l : 'a list ; name : string ; bottom : 'a option}


let create name = {l = [] ; name=name ; bottom = None}
let create_init name x = {l = [] ; name=name ; bottom = Some x}

let reset s = s.l <- []

let bottom msg s = match s.bottom with
| None -> raise (Fatal (msg^": "^s.name))
| Some x -> x

let name {name=name;_} = name

and push s x = s.l <- x :: s.l

and pop s = match s.l with
| [] -> bottom "pop" s
| x :: r ->
    s.l <- r ;
    x

and top s = match s.l with
| [] -> bottom "top" s
| x :: _ -> x

and top2 s = match s.l with
| []|[_] -> bottom "top2" s
| _ :: x :: _ -> x

and length s = List.length s.l

and empty s = match s.l with
| [] -> true
| _  -> false

let pretty f stack =
  prerr_string stack.name ;
  prerr_string ": <<" ;
  let rec do_rec = function
    | [] -> prerr_endline ">>"
    | [x] ->
        prerr_string ("'"^f x^"'") ;
        prerr_endline ">>"
    | x :: r ->
        prerr_string "'" ;
        prerr_string (f x) ;
        prerr_string "'" ;
        do_rec r in
  do_rec stack.l

let rev s = s.l <- List.rev s.l
let map s f =  s.l <- List.map f s.l

type 'a saved = 'a list

let empty_saved = []
and save {l=l;_} = l
and restore s x = s.l <- x

let finalize {l=now;_} p f =
  let rec f_rec = function
    | [] -> ()
    | nx::n -> 
      if p nx  then ()
      else begin
        f nx ;
        f_rec n
      end  in
  f_rec now

  


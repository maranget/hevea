(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

let header = "$Id: misc.ml,v 1.13 2000-05-22 12:19:10 maranget Exp $" 

exception Fatal of string
exception Purposly of string
exception ScanError of string
exception EndInput
exception EndDocument
exception Close of string
exception EndOfLispComment of int (* QNC *)

let verbose = ref 0
and readverb = ref 0

let silent = ref false

let column_to_command s = "\\@"^s^"@"

let warn_max = 100
let n_warns = ref warn_max

let w_table = Hashtbl.create 37
let hot_start () =
  Hashtbl.clear w_table ;
  n_warns := warn_max

let new_warn s =
  try
    Hashtbl.find w_table s ; false
  with
  | Not_found ->
      n_warns := !n_warns - 1 ;
      if !n_warns <= 0 then
        hot_start () ;
      Hashtbl.add w_table s () ;
      true

        
let warning s =
  if not !silent || !verbose > 0 then begin
    if (* !verbose > 0 || new_warn s *) true then begin
      Location.print_pos () ;
      prerr_string "Warning: " ;
      prerr_endline s
    end
  end

let print_verb level s =
  if  !verbose > level then begin
    Location.print_pos () ;
    prerr_endline s
  end

let message s =
  if not !silent || !verbose > 0 then prerr_endline s

let fatal s = raise (Fatal s)


let copy_hashtbl from_table to_table =
  Hashtbl.clear to_table ;
  let module OString =
    struct
      type t = string
      let compare = Pervasives.compare
    end in
  let module Strings = Set.Make (OString) in
  let keys = ref Strings.empty in
  Hashtbl.iter 
    (fun key _ -> keys := Strings.add key !keys)
    from_table ;
  Strings.iter
    (fun key ->
      let vals = Hashtbl.find_all from_table key in
      match vals with
      | [] -> assert false
      | value :: _ -> Hashtbl.add to_table key value)
    !keys

let clone_hashtbl from_table =
  let to_table = Hashtbl.create 17 in
  copy_hashtbl from_table to_table ;
  to_table 

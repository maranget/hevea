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

let header = "$Id: misc.ml,v 1.16 2000-07-05 17:46:35 maranget Exp $" 

exception Fatal of string
exception NoSupport of string
exception Purposly of string
exception ScanError of string
exception UserError of string
exception EndInput
exception EndDocument
exception Close of string
exception EndOfLispComment of int (* QNC *)

let verbose = ref 0
and readverb = ref 0

let silent = ref false

let column_to_command s = "\\@"^s^"@"


let hot_start () = ()

let warning s =
  if not !silent || !verbose > 0 then begin
    Location.print_pos () ;
    prerr_string "Warning: " ;
    prerr_endline s
  end

let print_verb level s =
  if  !verbose > level then begin
    Location.print_pos () ;
    prerr_endline s
  end

let message s =
  if not !silent || !verbose > 0 then prerr_endline s

let fatal s = raise (Fatal s)
let not_supported s = raise (NoSupport s)


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

let start_env env = "\\"^ env
and end_env env = "\\end"^env


type limits = Limits | NoLimits | IntLimits

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

open Printf
open Misc
open Lexstate

exception Failed

module OString = struct
  type t = string
  let compare = Pervasives.compare
end

module Strings = Set.Make (OString)

(* Data structures for TeX macro  model *)
let local_table = Hashtbl.create 97
and global_table = Hashtbl.create 97
and prim_table = Hashtbl.create 5
and known_macros = ref Strings.empty

let purge = ref Strings.empty
and purge_stack = MyStack.create "purge"
and group_level = ref 0

(* Hot start *)
type ctable = (string, pat * action) Hashtbl.t
type ptable = (string, (unit -> unit)) Hashtbl.t
type saved = 
    int * Strings.t * Strings.t MyStack.saved *
    ptable * ctable * ctable * Strings.t


let pretty_macro n acs =
  pretty_pat n ;
  prerr_string " -> " ;
  pretty_action acs

let hidden_pretty_table cmdtable =
  let t = Hashtbl.create 97
  and count = ref 0 in
  let incr k =
    incr count ;
    let r =
      try Hashtbl.find t k with
      | Not_found ->
          let r = ref 0 in
          Hashtbl.add t k r ;
          r in
    incr r in
  Hashtbl.iter (fun k (n,acc) ->
    Printf.fprintf stderr "%s -> " k ;
    pretty_macro n acc ;
    prerr_endline "" ;
    incr k) cmdtable ;
  Printf.fprintf stderr
      "Table size: %d\n" !count ;
  Hashtbl.iter
    (fun k r ->
      if !r > 1 then
        Printf.fprintf stderr "%s: %d\n" k !r)
    t ;
  flush stderr

let pretty_table () =
  Printf.fprintf stderr "Macro tables, level=%d\n" !group_level ;
  prerr_endline "Global table" ;
  hidden_pretty_table global_table ;
  prerr_endline "Local table" ;
  hidden_pretty_table local_table

let checkpoint () =
  !group_level, !purge, MyStack.save purge_stack,
  Hashtbl.copy prim_table,
  Hashtbl.copy global_table, Hashtbl.copy local_table,
  !known_macros

and hot_start (level_checked, purge_checked, purge_stack_checked,
               prim_checked,
               global_checked, local_checked,
               known_checked) =
  group_level := level_checked ;
  purge := purge_checked ;
  MyStack.restore purge_stack purge_stack_checked ;
  Misc.copy_hashtbl prim_checked prim_table ;
  Misc.copy_hashtbl global_checked global_table ;
  Misc.copy_hashtbl local_checked local_table ;
  known_macros := known_checked

(* Controlling scope *)
let open_group () =
  incr group_level ;
  MyStack.push purge_stack !purge ;
  purge := Strings.empty

and close_group () =
  if !group_level > 0 then (* Undo bindings created at the closed level *)
    Strings.iter
      (fun name -> Hashtbl.remove local_table name)
      !purge ;
  decr group_level ;
  purge := MyStack.pop purge_stack

let get_level () = !group_level

(* Remove one local definition in advance ... *)
let pre_purge name purge =
  if Strings.mem name purge then begin
    Hashtbl.remove local_table name ;
    Strings.remove name purge
  end else
    purge

(* Definitions *)
let hidden_global_def name x =
  if !group_level > 0 && Hashtbl.mem local_table name then begin
    (*
      global definition of a localy defined macro,
      undo all local bindings
    *)
    purge := pre_purge name !purge ;
    MyStack.map purge_stack (fun purge -> pre_purge name purge)
  end ;
  Hashtbl.replace global_table name x

let hidden_local_def name x =
  if !group_level > 0 then begin (* indeed local *)
    if Strings.mem name !purge then (* redefinition *)
      Hashtbl.remove local_table name
    else (* creation (at the current level) *)
      purge := Strings.add name !purge ;
    Hashtbl.add local_table name x
  end else begin (* same as global *)
    Hashtbl.replace global_table name x
  end

let hidden_find name =
  if !group_level > 0 then begin
    try Hashtbl.find local_table name with
    | Not_found -> Hashtbl.find global_table name
  end else
    Hashtbl.find global_table name

let set_saved_macros () =
  known_macros :=
    Hashtbl.fold
      (fun name _ -> Strings.add name)
      global_table Strings.empty

let get_saved_macro name = Strings.mem name !known_macros
  
(* Primitives *)
let register_init name f =
  if !verbose > 1 then
    prerr_endline ("Registering primitives for package: "^name);
  try
    let _f = Hashtbl.find prim_table name in
    fatal
      ("Attempt to initlialize primitives for package "^name^" twice")
  with
  | Not_found ->  Hashtbl.add prim_table name f

and exec_init name =
   if !verbose > 1 then
     prerr_endline ("Initializing primitives for package: "^name) ;
  try
    let f = Hashtbl.find prim_table name in
    Hashtbl.remove prim_table name ; (* accidental double is possible... *)
    try f () with
      Failed ->
        Misc.warning
         ("Bad trip while initializing primitives for package: "^name)
  with Not_found ->
    Misc.warning ("Cannot find primitives for package: "^name)
;;   


(* Interface *)

let exists name = 
  try
    let _ = hidden_find name in true
  with
  | Not_found -> false


let find name =
  try hidden_find name with
  | Not_found ->
      warning ("Command not found: "^name) ;
      ([],[]),Subst []

and find_fail name =
  try hidden_find name with
  | Not_found -> raise Failed

let pretty_command name =
  let n,acc = find name in
  eprintf "%s: " name ;
  pretty_macro n acc ;
  prerr_endline ""

let def name pat action =
  if !verbose > 1 then begin
    Printf.fprintf stderr "def %s = " name;
    pretty_macro pat action ;
    prerr_endline ""
  end ;
  hidden_local_def name (pat,action)

and global_def name pat action =
  if !verbose > 1 then begin
    Printf.fprintf stderr "global def %s = " name;
    pretty_macro pat action ;
    prerr_endline ""
  end ;
  hidden_global_def name (pat,action)

and global_undef name =
  Hashtbl.remove global_table name ;
  Hashtbl.remove local_table name
;;

let def_init name f =
  if exists name then begin
    fatal ("Command: "^name^" defined at initialisation")
  end ;
  def name zero_pat (CamlCode f)

(*
let pretty_arg = function
  | None -> prerr_string "<None>"
  | Some (n,acc) -> pretty_macro n acc

let pretty_replace s name old new_def =
  Printf.fprintf stderr "%s: %s\n\told=" s name ;
  pretty_arg old ;
  Printf.fprintf stderr "\n\tnew=" ;
  pretty_arg new_def ;
  prerr_endline ""
*)
    
let replace name new_def =
  let old_def =
    try Some (hidden_find name) with
    | Not_found -> None in
(*
  pretty_replace "replace" name old_def new_def ;
  Printf.fprintf stderr "level=%d\n" !group_level ;
*)
  begin match new_def with
  | Some d -> hidden_local_def name d
  | None -> match old_def with
    | None -> ()
    | Some _ -> (* what will happen if binding was global ??? *)
        if !group_level > 0 then
          purge := pre_purge name !purge
        else
          Hashtbl.remove global_table name 
  end ;
  old_def
          
(* addto *)
let addto name body =
  let old = try Some (hidden_find name) with Not_found -> None in
  match old with
  | Some (pat,Subst obody) ->
      hidden_local_def name (pat,Subst (obody@("%\n"::body)))
  | Some (_,_) ->
      warning "\\addto on non-subst macro"
  | None ->
      hidden_local_def name (zero_pat,Subst body)
      
      

(* macro static properties *)

let invisible = function
  "\\nofiles"
| "\\pagebreak" | "\\nopagebreak" | "\\linebreak"
| "\\nolinebreak" | "\\label" | "\\index"
| "\\vspace" | "\\glossary" | "\\marginpar"
| "\\figure" | "\\table"
| "\\nostyle" | "\\rm" | "\\tt"
| "\\bf" | "\\em" | "\\it" | "\\sl" 
| "\\tiny" | "\\footnotesize" | "\\scriptsize"
| "\\small" | "\\normalsize" | "\\large" | "\\Large" | "\\LARGE"
| "\\huge" | "\\Huge"
| "\\purple" | "\\silver" | "\\gray" | "\\white"
| "\\maroon" | "\\red" | "\\fuchsia" | "\\green"
| "\\lime" | "\\olive" | "\\yellow" | "\\navy"
| "\\blue" | "\\teal" | "\\aqua" | "\\else" | "\\fi"
| "\\char" -> true
| name ->
    (String.length name >= 3 && String.sub name 0 3 = "\\if")
;;


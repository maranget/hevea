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

open Misc

let header = "$Id: auxx.ml,v 1.2 1999-11-04 23:11:37 maranget Exp $" 

let rtable = Hashtbl.create 17
;;

let rset name value = Hashtbl.add rtable name value
;;

let rget name =
  try Hashtbl.find rtable name with Not_found -> begin
    warning ("Undefined label: "^name) ; "??"
  end
;;

let btable = Hashtbl.create 17
;;

let bset name value =  Hashtbl.add btable name value
;;

let bget name =
  try Hashtbl.find btable name with Not_found ->
  begin
    warning ("Undefined citation: "^name) ; name
  end
;;

let auxfile = ref None
and auxname = ref ""
and something = ref false
and changed = ref false
;;

let init base =
  let filename = base^".haux" in
  try
    let file = open_out filename in
    auxname := filename ;
    auxfile := Some file
  with Sys_error s ->
    warning ("Cannot open out file: "^filename^" : "^s)

and finalize () = match !auxfile with
| None -> ()
| Some file ->
    close_out file ;
    if not !something then
      Sys.remove !auxname;
    if !changed then
        prerr_endline
          "HeVeA Warning: Label(s) may have changed. Rerun to get cross-references right." ;
;;

let write table output_fun key pretty = match !auxfile with
| None -> ()
| Some file ->
    something := true ;
    changed :=
       !changed ||
       (try let olds = Hashtbl.find_all table key in
       match olds with
       | []    -> true
       | [old] -> pretty <> old
       | _     -> false (* In that case, can't tell *)
       with Not_found -> true) ;
    output_fun file
;;

let bseen = Hashtbl.create 17
let bcheck key =
  try
    let _ = Hashtbl.find bseen key in
    warning ("Multiple definitions for citation: "^key)
  with
  | Not_found ->
      Hashtbl.add bseen key ()

let rseen = Hashtbl.create 17
let rcheck key =
  try
    let _ = Hashtbl.find rseen key in
    warning ("Multiple definitions for label: "^key)
  with
  | Not_found ->
      Hashtbl.add rseen key ()


let bwrite key pretty =
  bcheck key ;
  write  btable
    (fun file ->
      output_string file "\\bibcite{" ;
      output_string file key ;
      output_string file "}{" ;
      output_string file pretty ;
      output_string file "}\n") key pretty
and rwrite key pretty =
  rcheck key ;
  write rtable
    (fun file ->
      output_string file "\\newlabel{" ;
      output_string file key ;
      output_string file "}{{" ;
      output_string file pretty ;
      output_string file "}{X}}\n") key pretty
;;

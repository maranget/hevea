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

let header = "$Id: auxx.ml,v 1.1 1999-11-02 20:10:46 maranget Exp $" 

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

let bset name value = Hashtbl.add btable name value
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
       (try let old = Hashtbl.find table key in old <> pretty
         with Not_found -> true) ;
    output_fun file
;;

let bwrite key pretty =
  write btable
    (fun file ->
      output_string file "\\bibcite{" ;
      output_string file key ;
      output_string file "}{" ;
      output_string file pretty ;
      output_string file "}\n") key pretty
and rwrite key pretty =
  write rtable
    (fun file ->
      output_string file "\\newlabel{" ;
      output_string file key ;
      output_string file "}{{" ;
      output_string file pretty ;
      output_string file "}{X}}\n") key pretty
;;

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

let header = "$Id: auxx.ml,v 1.10 2000-05-26 17:05:51 maranget Exp $" 

let rtable = Hashtbl.create 17
;;

let rset name value = Hashtbl.add rtable name value
;;

  
let rget name =
  try Hashtbl.find rtable name with Not_found -> begin
    warning ("Undefined label: ``"^name^"''") ; "??"
  end
;;

let btable = Hashtbl.create 17
;;

let bset name value =  Hashtbl.add btable name value
;;

let bget warn name =
  let r =
    try Hashtbl.find btable name with Not_found ->
      begin
        if warn then warning ("Undefined citation: ``"^name^"''") ;
        "{"^name^"}"
      end in
  r
;;

let auxfile = ref None
and auxname = ref ""
and something = ref false
and changed = ref false
;;

let rseen = Hashtbl.create 17
and bseen = Hashtbl.create 17
;;

let init base =
  let filename = base^".haux" in
  try
    let file = open_out filename in
    auxname := filename ;
    auxfile := Some file
  with Sys_error s ->
    warning ("Cannot open out file: "^filename^" : "^s)

(* result is true when another run is needed *)

and finalize check =
  match !auxfile with
  | None -> false
  | Some file ->
      close_out file ;
      if not !something then
        Myfiles.remove !auxname;
      if check then begin
        let check_disappear table seen =
          Hashtbl.iter
            (fun key _ ->
              try Hashtbl.find seen key
              with Not_found -> changed := true)
            table in
        if not !changed then begin
          check_disappear rtable rseen ;
          check_disappear btable bseen
        end ;
        if !changed then
          Misc.message
            "HeVeA Warning: Label(s) may have changed. Rerun to get cross-references right." ;
        !changed
      end else
        false
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


let bcheck key =
  try
    let _ = Hashtbl.find bseen key in
    warning ("Multiple definitions for citation: "^key) ;
    false
  with
  | Not_found ->
      Hashtbl.add bseen key () ;
      true

let rcheck key =
  try
    let _ = Hashtbl.find rseen key in
    warning ("Multiple definitions for label: "^key) ;
    false
  with
  | Not_found ->
      Hashtbl.add rseen key () ;
      true


let bwrite key pretty =
  if bcheck key then
    write  btable
      (fun file ->
        output_string file "\\bibcite{" ;
        output_string file key ;
        output_string file "}{" ;
        output_string file pretty ;
        output_string file "}\n") key pretty

and rwrite key pretty =
  if rcheck key then
    write rtable
      (fun file ->
        output_string file "\\newlabel{" ;
        output_string file key ;
        output_string file "}{{" ;
        output_string file pretty ;
        output_string file "}{X}}\n") key pretty
;;

type saved =
(string, string) Hashtbl.t * (string, unit) Hashtbl.t *
  (string, string) Hashtbl.t * (string, unit) Hashtbl.t *
  out_channel option * string * bool * bool

let check () =
  Misc.clone_hashtbl rtable,  Misc.clone_hashtbl rseen,
  Misc.clone_hashtbl btable,  Misc.clone_hashtbl  bseen,
  !auxfile, !auxname, !something, !changed

let hot
 (srtable, srseen, sbtable, sbseen,
  sauxfile, sauxname, ssomething, schanged) =
  Misc.copy_hashtbl srtable rtable ; Misc.copy_hashtbl srseen rseen ;
  Misc.copy_hashtbl sbtable btable ; Misc.copy_hashtbl sbseen bseen ;
  auxfile := sauxfile ;
  auxname := sauxname ;
  something := ssomething ;
  changed := schanged

(* Valid only juste before reading main input file *)
let hot_start () =
  Hashtbl.clear rtable ; Hashtbl.clear rseen ;
  Hashtbl.clear btable ; Hashtbl.clear bseen ;
  auxfile :=  None ;
  auxname := "" ;
  something := false ;
  changed := false

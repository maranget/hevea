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

let header = "$Id: auxx.ml,v 1.16 2001-10-22 18:03:55 maranget Exp $" 

let rtable = Hashtbl.create 17
;;

let rset name value =
  Hashtbl.add rtable name value
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
        "\\@verbarg{"^name^"}"
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

(* result is true when another run is needed *)

let finalize check =
  match !auxfile with
  | None -> false
  | Some file ->
      close_out file ;
      if not !something then
        Mysys.remove !auxname;
      if check then begin
        let check_disappear table seen =
          Hashtbl.iter
            (fun key _ ->
              try Hashtbl.find seen key
              with Not_found ->
                Misc.warning ("Disappear: "^key) ;
                changed := true)
            table in
        if not !changed then begin
          check_disappear rtable rseen ;
          check_disappear btable bseen
        end ;
        if !changed then
          Misc.message
            "HeVeA Warning: Label(s) may have changed. Rerun me to get cross-references right." ;
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

let swrite msg = match !auxfile with
| None -> ()
| Some file ->
    something := true ;
    output_string file msg
  
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

type toc_t = {mutable level : int ; mutable depth : int ; chan : out_channel}

let toctable = Hashtbl.create 5
;;


let do_addtoc toc level what =
  (* First adjust nesting of tocenv *)
  if level > toc.level then begin
    for i = toc.level to level-1 do
      output_string toc.chan "\\begin{tocenv}\n"
    done ;
    toc.depth <- toc.depth + level - toc.level ;
    toc.level <- level
  end else if level < toc.level then begin
    let nclose = min toc.depth (toc.level - level) in
    for i = 1 to nclose do
      output_string toc.chan "\\end{tocenv}\n"
    done ;
    toc.depth <- toc.depth - nclose ;
    if toc.depth=0 then begin
      output_string toc.chan "\\begin{tocenv}\n" ;
      toc.depth <- 1 ;          
    end ;
    toc.level <- level
  end ;

 (* Then ouput toc item *)
  Printf.fprintf toc.chan "\\tocitem %s\n" what



let  addtoc suf level what = 
  try
    try
      let toc = Hashtbl.find toctable suf in
      do_addtoc toc level what

  with
    | Not_found ->
        let name = Parse_opts.base_out^"."^suf in
        let chan = open_out name in
        output_string chan "\\begin{tocenv}\n" ;
        let toc = {level=level ; depth=1 ; chan=chan} in
        Hashtbl.add toctable suf toc ;
        do_addtoc toc level what
  with
  | Sys_error msg ->
      Misc.warning
        ("Problem with toc file "^Parse_opts.base_out^"."^suf^": "^msg)

let final base =
  Hashtbl.iter
    (fun suf toc ->
      for i=1 to toc.depth do
        output_string toc.chan "\\end{tocenv}\n" ;
      done ;
      close_out toc.chan) 
    toctable ;
  Hashtbl.clear toctable ;
  let filename = base^".haux" in
  try
    let file = open_out filename in
    auxname := filename ;
    auxfile := Some file
  with Sys_error s ->
    warning ("Cannot open out file: "^filename^" : "^s)


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

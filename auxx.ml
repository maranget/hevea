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

let rtable = Hashtbl.create 17
;;

let rset name value = Hashtbl.add rtable name value
;;
  
let rget name =
  try Hashtbl.find rtable name with Not_found -> begin
    warning ("Undefined label: '"^name^"'") ; "??"
  end
;;

let btable = Hashtbl.create 17
;;

let bset name value =  Hashtbl.add btable name value
;;

let bget warn name =
  let r =
    try Some (Hashtbl.find btable name) with Not_found ->
      begin
        if warn then warning ("Undefined citation: '"^name^"'") ;
        None
      end in
  r
;;

let auxfile = ref None
and auxname = ref ""
and something = ref false
and digest = ref None
;;

let read_digest name =
  try
    Some (Digest.file name)
  with
  | Sys_error _ -> None

let labelcount = ref 0
let rseen = Hashtbl.create 17
and bseen = Hashtbl.create 17
;;

(* result is true when another run is needed *)

let finalize check =
  match !auxfile with
  | None -> false
  | Some file ->
      close_out file ;
      if not !something then Mysys.remove !auxname;
      if check then begin
        let changed = !digest <> read_digest !auxname in          
        if changed then
          Misc.message
            "HeVeA Warning: Label(s) may have changed. Rerun me to get cross-references right." ;
        changed
      end else
        false
;;

let write output_fun = match !auxfile with
| None -> ()
| Some file ->
    something := true ;
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
    -1
  with
  | Not_found ->
      let x = !labelcount in
      incr labelcount ;
      Hashtbl.add rseen key x ;
      x

let swrite msg = match !auxfile with
| None -> ()
| Some file ->
    something := true ;
    output_string file msg
  
let bwrite key pretty =
  if bcheck key then
    write
      (fun file ->
        output_string file "\\bibcite{" ;
        output_string file key ;
        output_string file "}{" ;
        output_string file pretty ;
        output_string file "}\n")

and rwrite key pretty =
  let idx = rcheck key in
  if idx >= 0 then
    write
      (fun file ->
        output_string file "\\newlabel{" ;
        output_string file key ;
        output_string file "}{{" ;
        output_string file pretty ;
        output_string file "}{X}}\n")
and rwrite2 anchor key pretty =
  let idx =  rcheck key in
  if idx >= 0 then
    write
      (fun file ->
        output_string file "\\new@anchor@label{" ;
        output_string file anchor ;
        output_string file "}{" ;
        output_string file key ;
        output_string file "}{{" ;
        output_string file pretty ;
        output_string file "}{X}}\n")
;;

type toc_t =
    {mutable level : int ; mutable depth : int ; chan : out_channel }

let toctable = Hashtbl.create 5
;;

let tocfilename suf = Parse_opts.base_out^"."^suf

let do_addtoc toc level what =
  (* First adjust nesting of tocenv *)
  if level > toc.level then begin
    for _i = toc.level to level-1 do
      output_string toc.chan "\\begin{tocenv}\n"
    done ;
    toc.depth <- toc.depth + level - toc.level ;
    toc.level <- level
  end else if level < toc.level then begin
    let nclose = min toc.depth (toc.level - level) in
    for _i = 1 to nclose do
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

let addtoc suf level what = 
  try
    try
      let toc = Hashtbl.find toctable suf in
      do_addtoc toc level what
  with
    | Not_found ->
        let name = Parse_opts.base_out^"."^suf in
        let chan = open_out name in
        output_string chan "\\begin{tocenv}\n" ;
        let toc = {level=level ; depth=1 ; chan=chan } in
        Hashtbl.add toctable suf toc ;
        do_addtoc toc level what
  with
  | Sys_error msg ->
      Misc.warning
        ("Problem with toc file "^tocfilename suf^": "^msg)


(* To be performed aroound haux file reading *)
let init base =
  digest := read_digest (base^".haux")

let final base =
  Hashtbl.iter
    (fun _ toc ->
      for _i=1 to toc.depth do
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
(string, string) Hashtbl.t *
  int * (string, int) Hashtbl.t *
  (string, string) Hashtbl.t * (string, unit) Hashtbl.t *
  out_channel option * string * bool * Digest.t option

let check () =
  Hashtbl.copy rtable,
  !labelcount,
  Hashtbl.copy rseen,
  Hashtbl.copy btable,  Hashtbl.copy  bseen,
  !auxfile, !auxname, !something, !digest

let hot
 (srtable, slabelcount, srseen, sbtable, sbseen,
  sauxfile, sauxname, ssomething, sdigest) =
  Misc.copy_hashtbl srtable rtable ;
  labelcount := slabelcount  ;
  Misc.copy_hashtbl srseen rseen ;
  Misc.copy_hashtbl sbtable btable ; Misc.copy_hashtbl sbseen bseen ;
  auxfile := sauxfile ;
  auxname := sauxname ;
  something := ssomething ;
  digest := sdigest

(* Valid only juste before reading main input file *)
let hot_start () =
  Hashtbl.clear rtable ; labelcount := 0 ; Hashtbl.clear rseen ;
  Hashtbl.clear btable ; Hashtbl.clear bseen ;
  auxfile :=  None ;
  auxname := "" ;
  something := false ;
  digest := None

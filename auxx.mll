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

{
open Lexing
open Misc

let header = "$Id: auxx.mll,v 1.4 1999-08-30 17:59:16 maranget Exp $" 

let rtable = Hashtbl.create 17
;;

let rset name value = Hashtbl.add rtable name value
;;

let rget name =
  try Hashtbl.find rtable name with Not_found -> "X"
;;

let btable = Hashtbl.create 17
;;

let bset name value = Hashtbl.add btable name value
;;

let bget name =
  try Hashtbl.find btable name with Not_found -> name
;;

let auxfile = ref None
and something = ref false
and changed = ref false
;;

let init base =
  let filename = base^".haux" in
  try
    let file = open_out filename in
    auxfile := Some file
  with Sys_error s ->
    warning ("Cannot open out file: "^filename^" : "^s)

and finalize () = match !auxfile with
| None -> ()
| Some file ->
    close_out file ;
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
}

rule main = parse
  "\\newlabel"
    {let name = Save.arg lexbuf in
    let value = Save.arg lexbuf in
    let value = Save.arg (from_string value) in
    rset name value ; main lexbuf}
| "\\bibcite"
    {let name = Save.arg lexbuf in
    let value = Save.arg lexbuf in
    bset name value ;
    main lexbuf}
| "\\@input"
    {let filename = Save.arg lexbuf in
    begin try
      let filename,chan = Myfiles.open_tex filename in
      let newbuf = from_channel chan in
      main newbuf
    with Myfiles.Except -> begin
      if !verbose > 0 then
        prerr_endline ("Not opening file: "^filename) ;
      end
    | Myfiles.Error m ->
        if not !silent || !verbose > 0 then begin
          Location.print_pos () ;
          prerr_endline ("Warning: "^m) ;
        end
    end ;
    main lexbuf}
| _   {main lexbuf}
| eof {()}

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

let header = "$Id: latexmain.ml,v 1.36 1999-04-16 13:31:39 maranget Exp $" 

open Misc
open Parse_opts

module Scan = Latexscan.Make (Html)
;;

(* Additional modules *)
module Otherscan = Videoc.Makealso (Scan);;
Otherscan.init ();;

module Verbscan = Verb.MakeAlso  (Html) (Scan);;
Verbscan.init ();;


Get.init (Scan.subst_this Scan.subst) (Scan.get_this Scan.main) ;
Tabular.init (Scan.subst_this Scan.subst)
;;


let finalize check =
  Image.finalize () ;
  Html.finalize check
;;

let read_style name =
  let oldverb = !verbose in
  if !verbose > 0 then verbose := 1;
  begin try
    let name,chan =  Myfiles.open_tex name in
    if !verbose > 0 then begin
       prerr_endline ("read_style: "^name)
    end ;
    let buf = Lexing.from_channel chan in
    Location.set name buf;
    Scan.main buf ;
    close_in chan ;
    Location.restore ()
  with
  | Myfiles.Except-> ()
  end ;
  verbose := oldverb
;;

let open_tex name =
  let name,chan =  Myfiles.open_tex name in
  if !verbose > 0 then
    prerr_endline ("Main input_file: "^name) ;
  name,chan

let read_tex name_in =
    let input_name,chan =
      match name_in with "" -> "",stdin | _ -> open_tex name_in in
    let buf = Lexing.from_channel chan in
    Location.set input_name buf ;
    Save.set_verbose !silent !verbose ;
    Scan.main buf ;
    close_in chan ;
    Location.restore ()

let main () = 

    verbose := !readverb ;
    read_style "hevea.hva" ;

    let rec do_rec = function
      [] -> ()
    | x::rest ->
       do_rec rest ;
       read_style x in

    let styles = match Parse_opts.styles with
    | [] -> if base_in = "" then ["article.hva"] else []
    | _  -> Parse_opts.styles in

    do_rec styles ;

    Location.set_base base_in ;

    begin match base_in with
      "" -> Scan.no_prelude ()
    | _  ->
       let auxname = base_in^".aux" in
       try
         let _,auxchan = Myfiles.open_tex auxname in
         let buf = Lexing.from_channel auxchan in
         Auxx.main buf ;
         close_in auxchan
       with Myfiles.Error _ -> begin
         if !verbose > 0 then
           prerr_endline ("Cannot open aux file: "^auxname)
       end
    end ;

    read_tex name_in ;
    finalize true
;;   


begin try
  main ()
with
| Html.Close s ->
    Location.print_pos () ;
    prerr_endline s;
    Scan.print_env_pos () ;
    prerr_endline "Adios" ;
    exit 2
| Html.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while writing HTML:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Scan.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading LaTeX:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Lexstate.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading LaTeX:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Verb.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading verbatim LaTeX:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Colscan.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading LaTeX style colors:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Save.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading LaTeX macros arguments:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Tabular.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while reading table format:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Get.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error while getting a value:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Latexmacros.Error s ->
    Location.print_pos () ;
    prerr_endline ("Error in macro definition:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
| Myfiles.Error s ->
    Location.print_pos () ;
    prerr_endline ("File error:\n\t"^s) ;
    prerr_endline "Adios" ;
    exit 2
|  Misc.Fatal s ->
    Location.print_pos () ;
    prerr_endline
      ("Fatal error: "^s^"\n (if input is plain LaTeX, please report to Luc.Maranget@inria.fr)") ;
    prerr_endline "Adios" ;
    exit 2 
|  x ->
    Location.print_pos () ;
    prerr_endline
      ("Fatal error, spurious exception:\n\t"^Printexc.to_string x^
       "\n(if input is plain LaTeX, please report to Luc.Maranget@inria.fr)") ;
    prerr_endline "Adios" ;
    exit 2
end ;
exit 0;;


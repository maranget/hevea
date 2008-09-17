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

let header = "$Id: parse_opts.ml,v 1.38 2008-09-17 08:12:11 maranget Exp $" 

type input = File of string | Prog of string

let files = ref []
;;

let add_input s = files := File s :: !files
and add_program s = files := Prog s :: !files
;;

(* use this to create your warnings if you wish to *)
let frenchwarning = ref false
;;

type destination = Html | Text | Info
;;
let mathml = ref false
;;

(*to activate advanced entities*) 
let moreentities = ref false
;;

(* NO NEED AFTER BABEL SUPPORT *)
(*let language = ref English*)
type symbol_mode = SText | Symbol | Entity

let symbol_mode = ref Entity
and pedantic = ref false
and destination = ref Html
and fixpoint = ref false
and optimize = ref false
;;

let width = ref 72
;;

let except = ref []
;;

let path = ref []
;;

let outname = ref ""
;;

let check_displayverb n = if n > 1 then displayverb := true

let _ = Arg.parse
    [
  ("-version", Arg.Unit
     (fun () ->
       print_endline ("hevea "^Version.version) ;
       print_endline ("library directory: "^Mylib.static_libdir) ;
       exit 0),
   "show hevea version and library directory") ;
  ("-v",
   Arg.Unit
     (fun () -> readverb := !readverb + 1 ; check_displayverb !readverb),
   "verbose flag, can be repeated to increase verbosity") ;
  ("-dv", Arg.Unit (fun () -> displayverb := true),
   "add borders to some block-level elements, so as to show hevea output structure") ;
  ("-s", Arg.Unit (fun () -> silent := true),
   "suppress warnings") ;
  ("-I", Arg.String (fun s -> path := s :: !path),
   "dir, add directory 'dir' to search path") ;
  ("-e", Arg.String (fun s -> except := s :: !except),
   "filename, prevent file 'filename' from being read") ;
  ("-fix", Arg.Unit (fun () -> fixpoint := true),
   "iterate Hevea until fixpoint") ;
  ("-O", Arg.Unit (fun () -> optimize := true),
   "call esponja to optimize HTML output") ;
  ("-exec", Arg.String add_program,
   "prog , execute external program 'prog', then read its result") ;
  ("-francais",Arg.Unit (fun () -> frenchwarning := true),
   "French mode (deprecated)") ;
  ("-moreentities", Arg.Unit (fun () -> moreentities := true),
   "Enable the output of some rare entities.") ;
  ("-entities", Arg.Unit (fun () -> symbol_mode := Entity),
   "Render symbols by using entities, this is the default") ;
  ("-textsymbols", Arg.Unit (fun () -> symbol_mode := SText),
   "Render symbols by english text") ;

  ("-noiso",Arg.Unit (fun () -> Misc.warning "-noiso is deprecated, by default hevea output is ascii"),
   "deprecated, does nothing") ;
  ("-pedantic",Arg.Unit (fun () -> pedantic := true),
   "be pedantic in interpreting HTML 4.0 transitional definition") ;
  ("-mathml",Arg.Unit (fun() -> mathml := true),
   "produces MathML output for equations, very experimental");

  ("-text",Arg.Unit (fun () -> symbol_mode :=  SText ; destination := Text),
   "output plain text");
  ("-info",Arg.Unit (fun () -> symbol_mode :=  SText ; destination := Info),
   "output info file(s)");
  ("-w", Arg.String (fun s -> width := int_of_string s),
   "width, set the output width for text or info output");
  ("-o", Arg.String (fun s -> outname := s),
   "filename, make hevea output go into file 'filename'")
]
    (add_input)
    ("hevea "^Version.version)
;;

let warning s =
  if not !silent || !verbose > 0 then begin
    Location.print_pos () ;
    prerr_string "Warning: " ;
    prerr_endline s
  end
;;

(* For correcting strange user (-exec prog en dernier) *)
let rec ffirst = function
  | [] -> None,[]
  | Prog _ as arg::rem ->
      let file, rest = ffirst rem in
      file, arg::rest
  | File _ as arg::rem ->
      Some arg,rem
;;

files :=
   match ffirst !files with
   | None,rem -> rem
   | Some arg,rem -> arg::rem


      
let base_in,name_in,styles = match !files with
| File x :: rest ->
    if Filename.check_suffix x ".hva" then
      "","", !files
    else
      let base_file = Filename.basename x in
      begin try
        let base =
          if Filename.check_suffix base_file ".tex" then
            Filename.chop_extension base_file
          else
            base_file in
        base,x,rest
      with Invalid_argument _ -> base_file, x,rest
      end
| _ -> "","",!files

let filter = match base_in with "" -> true | _ ->  false
;;

if filter then begin
  if !fixpoint then
    Misc.warning ("No fixpoint in filter mode");
  fixpoint := false
end
;;

let base_out = match !outname with
| "" -> begin match base_in with
  | "" -> ""
  | _  -> Filename.basename base_in
end      
| name ->
    let suff = match !destination with
    | Html -> ".html"
    | Text -> ".txt"
    | Info -> ".info"
    in
    if Filename.check_suffix name suff then
      Filename.chop_suffix name suff
    else
      try
        Filename.chop_extension name
      with Invalid_argument _ -> name

let name_out = match !outname with
| "" -> begin match base_in with
  | "" -> ""
  | x  -> begin
      match !destination with
      |	Html ->x^".html"
      |	Text ->x^".txt"
      |	Info ->x^".info"
  end
end    
| x  -> x



let _ =
  if !frenchwarning then begin
    warning "-francais option is deprecated, use babel instead"
  end


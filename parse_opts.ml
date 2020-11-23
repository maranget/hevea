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


type input = File of string | Prog of string

let files = ref []


let add_input s = files := File s :: !files
and add_program s = files := Prog s :: !files


(* use this to create your warnings if you wish to *)
let frenchwarning = ref false


type destination = Html | Text | Info

let mathml = ref false


(*to activate advanced entities*)
let moreentities = ref false


(* NO NEED AFTER BABEL SUPPORT *)
(*let language = ref English*)
type symbol_mode = SText | Symbol | Entity

let symbol_mode = ref Entity
and pedantic = ref false
and destination = ref Html
and fixpoint = ref false
and optimize = ref false

let default_width = 72
and default_small_length = 1024

let width = ref default_width
and except = ref []
and path = ref []
and outname = ref ""
and small_length = ref default_small_length


let check_displayverb n =
  if n > 1 then
    Misc.displayverb := true


let () =
  let usage =
    "Usage: hevea [OPTION...] [HVA-FILE...|LATEX-FILE...]\n\
     \n\
     Consult all HVA-FILEs and LATEX-FILEs; designate last LATEX-FILE\n\
     as main-input file and translate it to HTML.  If no file is given\n\
     or the last file is not a LATEX-FILE work in filter mode: read\n\
     LaTeX-source from standard input and write to standard output.\n\
     \n\
     Options `-info' or `-text' switch the output format to GNU Info\n\
     or plain text.\n\
     \n\
     Options:"
  and spec =
    ["-I", Arg.String (fun s -> path := s :: !path),
     "DIR prepend directory DIR to search path";
     "-e", Arg.String (fun s -> except := s :: !except),
     "FILE exclude FILE from being processed";
     "-fix", Arg.Set fixpoint,
     " iterate Hevea until reaching a fixpoint";
     "-info", Arg.Unit (fun () -> symbol_mode := SText; destination := Info),
     " output GNU Info file(s)";
     "-text", Arg.Unit (fun () -> symbol_mode := SText; destination := Text),
     " output plain text";
     "-o", Arg.Set_string outname,
     "FILE redirect Hevea output to FILE";
     "-s", Arg.Set Misc.silent,
     " suppress warnings";
     "-w", Arg.String (fun s -> width := int_of_string s),
     (Printf.sprintf "WIDTH set the output WIDTH (default: %i) for text or Info output" default_width);
     "-O", Arg.Set optimize,
     " call Esponja to optimize the HTML output of Hevea";
     "-exec", Arg.String add_program,
     "PROG execute external program PROG, then read its result";
     "-entities", Arg.Unit (fun () -> symbol_mode := Entity),
     " render symbols by using entities (default)";
     "-francais", Arg.Set frenchwarning,
     " French mode (deprecated)";
     "-noiso", Arg.Unit (fun () -> Misc.warning "-noiso is deprecated, by default hevea output is ASCII"),
     " deprecated, does nothing";
     "-mathml", Arg.Set mathml,
     " generate MathML output for equations; VERY EXPERIMENTAL";
     "-moreentities", Arg.Set moreentities,
     " enable the output of some rare entities";
     "-pedantic", Arg.Set pedantic,
     " be pedantic in interpreting HTML 4.0 transitional";
     "-textsymbols", Arg.Unit (fun () -> symbol_mode := SText),
     " render symbols by English text";
     "-dv", Arg.Set Misc.displayverb,
     " add borders to some block-level elements";
     "-rsz", Arg.Set_int small_length,
     (Printf.sprintf "SIZE set SIZE (default: %i) of leaves in rope implementation" default_small_length);
     "-v", Arg.Unit (fun () -> incr Misc.readverb; check_displayverb !Misc.readverb),
     " verbose flag; repeat to increase verbosity";
     "-version",
     Arg.Unit
       (fun () ->
         print_endline ("hevea " ^ Version.version);
         print_endline ("library directory: " ^ Mylib.static_libdir);
         exit 0),
     " output version information, library directory and exit"]
  in
    Arg.parse (Arg.align spec) add_input usage


let warning s =
  if not !Misc.silent || !Misc.verbose > 0 then
    begin
      Location.print_pos ();
      prerr_string "Warning: ";
      prerr_endline s
    end


(* For correcting strange user (-exec prog en dernier) *)
let rec ffirst = function
  | [] -> None, []
  | Prog _ as arg :: rem ->
     let file, rest = ffirst rem in
       file, arg :: rest
  | File _ as arg :: rem ->
     Some arg, rem
;;

files :=
   match ffirst !files with
   | None, rem -> rem
   | Some arg, rem -> arg :: rem



let base_in, name_in, styles =
  match !files with
  | File x :: rest ->
     if Filename.check_suffix x ".hva" then
       "", "", !files
     else
       let base_file = Filename.basename x in
         begin
           try
             let base =
               if Filename.check_suffix base_file ".tex" then
                 Filename.chop_extension base_file
               else
                 base_file in
               base,x,rest
           with Invalid_argument _ -> base_file, x, rest
         end
  | [] | Prog _ :: _ -> "", "", !files


let filter = match base_in with "" -> true | _ ->  false
;;


if filter then
  begin
    if !fixpoint then
      Misc.warning ("No fixpoint in filter mode");
    fixpoint := false
  end


let base_out =
  match !outname with
  | "" ->
     begin
       match base_in with
       | "" -> ""
       | _  -> Filename.basename base_in
     end
  | name ->
     let suff =
       match !destination with
       | Html -> ".html"
       | Text -> ".txt"
       | Info -> ".info"
     in
       if Filename.check_suffix name suff then
         Filename.chop_suffix name suff
       else
         try Filename.chop_extension name
         with Invalid_argument _ -> name


let name_out =
  match !outname with
  | "" ->
     begin
       match base_in with
       | "" -> ""
       | x  ->
          begin
            match !destination with
            | Html ->x ^ ".html"
            | Text ->x ^ ".txt"
            | Info ->x ^ ".info"
          end
     end
  | x -> x


let () =
  if !frenchwarning then
    warning "-francais option is deprecated, use babel instead"

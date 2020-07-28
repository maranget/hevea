(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*  Christian Queinnec, Universite Paris IV                            *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* <Christian.Queinnec@lip6.fr>
 The plugin for HeVeA that implements the VideoC style.
*)

{

open Printf

module Make
    (Dest : OutManager.S)
    (Image : ImageManager.S)
    (Scan : Latexscan.S) : Misc.Rien =
struct
open Misc
open Lexing
open Lexstate
open Latexmacros
open Subst
open Scan


let _header = 
  "$Id: videoc.mll,v 1.32 2012-06-05 14:55:39 maranget Exp $"
(* So I can synchronize my changes from Luc's ones *)
let _qnc_header =  "30 oct 2000"

exception EndSnippet
;;
exception EndTeXInclusion
;;

(* Re-link with these variables inserted in latexscan. *)

let withinSnippet = ref false;;
let withinTeXInclusion = ref false;;
let endSnippetRead = ref false;;

(* Snippet global defaults *)

let snippetLanguage = ref "";;
let enableLispComment = ref false;;
let enableSchemeCharacters = ref false;;

(* Snippet Environment: run a series of hooks provided they exist as
   user macros. *)

let runHook prefix parsing name =
  let run name = begin
    if !verbose > 2 then prerr_endline ("Trying to run hook " ^ name);
    if Latexmacros.exists name 
    then begin Lexstate.scan_this parsing name; () end
  end in
  let rec iterate name suffix =
    run name;
    if suffix <> ""
    then iterate (name ^ (String.make 1 (String.get suffix 0)))
                 (String.sub suffix 1 ((String.length suffix) - 1))
  in iterate (prefix ^ name ^ "Hook") !snippetLanguage;;

let snippetRunHook parsing name =
  runHook "\\snippet" parsing name;;

let snipRunHook parsing name =
  runHook "\\snip" parsing name;;

(* Hack for mutual recursion between modules: *)

(* Convert a reference to a hint such as "3" "annote.ann" "premier indice"
   into "3_annote_ann". This is needed for the annote tool.  *)

let compute_hint_id number filename _notename =
  let result = number ^ "_" ^ filename in
(*DEPRECATED
  let rec convert i = begin
    if i<String.length(result)
    then let c = String.get result i in
         if true || ('a' <= c && c <= 'z') (* test *)
            || ('A' <= c && c <= 'z') 
            || ('0' <= c && c <= '9') 
         then ()
         else String.set result i '_';
         convert (i+1);
    end in
  convert 0;
*)
  result;;

let increment_internal_counter =
  let counter = ref 99 in
  function () -> 
    begin
      counter := !counter + 1;
      !counter
    end;;

} 

let command_name =
  '\\' ((['@''A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'] | "\\*")

rule snippetenv = parse 
| eof { () }
| command_name
   {let csname = lexeme lexbuf in
    let pat,body = Latexmacros.find csname in
    begin match pat with
    | [],[] ->
      let args =  make_stack csname pat lexbuf in
      let cur_subst = get_subst () in
      let exec = function
        | Subst body ->
            if !verbose > 2 then
              eprintf "user macro in snippet: [%a]\n" pretty_body body ;
            Lexstate.scan_this_list_may_cont Scan.main
              lexbuf cur_subst (string_to_arg body)
        | Toks l ->
            List.iter
              (fun s -> scan_this Scan.main s)
              (List.rev l)
        | CamlCode f -> f lexbuf in
      scan_body exec body args
    |  _ ->
      raise (Misc.ScanError ("Command with arguments inside snippet"))
    end ;
    snippetenv lexbuf}
| '\n'
    {Dest.put_tag "<BR>";
     Dest.put_char '\n';
     snippetRunHook Scan.main "AfterLine";
     snippetRunHook Scan.main "BeforeLine";
     snippetenv lexbuf}
| ' '|'\t'
    {Dest.put_hspace false (Length.Char 1);
     snippetenv lexbuf}
| ';' + 
    {Dest.put (lexeme lexbuf);
     Dest.put_char ' ';
     if !enableLispComment
     then begin
        if !verbose > 1 then 
          prerr_endline "Within snippet: Lisp comment entered";
        Lexstate.withinLispComment := true;
        Scan.top_open_block "SPAN" 
          ("class=\"" ^ !snippetLanguage ^ "Comment\"");
        snippetRunHook Scan.main "BeforeComment";
        try Scan.main lexbuf with (* until a \n is read *)
        | exc -> begin
            snippetRunHook Scan.main "AfterComment";
            Scan.top_close_block "SPAN";
            Lexstate.withinLispComment := false;
            (* re-raise every exception but EndOfLispComment *)
            try raise exc with
            | Misc.EndOfLispComment nlnum -> begin
                let addon = (if !endSnippetRead then "\\endsnippet" else "") in
                if !verbose > 1 then 
                  Printf.fprintf stderr "%d NL after LispComment %s\n" 
                    nlnum ((if !endSnippetRead then "and " else "")^addon);
                let _ = Lexstate.scan_this snippetenv 
                    ((String.make (1+nlnum) '\n')^addon) in
                ()
            end;
        end;
     end;
     snippetenv lexbuf}
| '#'
    {Dest.put_char '#';
     if !enableSchemeCharacters
     then begin
        if !verbose > 1 then 
          prerr_endline "Within snippet: scheme characters enabled";
        schemecharacterenv lexbuf
     end;
     snippetenv lexbuf}
| _ as lxm
    {Scan.translate_put_unicode lxm (fun () -> read_lexbuf lexbuf) ;
     snippetenv lexbuf}

and read_lexbuf = parse
| _ as lxm { Char.code lxm }
| eof      { -1 }

(* Scheme characters are written as #\A or #\Newspace *)

and schemecharacterenv = parse
| command_name
    {let csname = lexeme lexbuf in
     Dest.put csname}
| ""
    { () }

(* Swallow characters until the end of the line. *)

and skip_blanks_till_eol_included = parse
| ' ' + 
    {skip_blanks_till_eol_included lexbuf}
| '\n'
    { () }
| ""
    { () }

(* Parse a succession of things separated by commas. *)

and comma_separated_values = parse
| [ ^ ',' ] * ','
    {let lxm = lexeme lexbuf in
     let s = String.sub lxm 0 (String.length lxm - 1) in
     if !verbose > 2 then prerr_endline ("CSV" ^ s);
     s :: comma_separated_values lexbuf}
| eof
    { [] }

(* Trailer: Register local macros as global. *)

{
let caml_print s = CamlCode (fun _ -> Dest.put s)
let snippet_def name d = Latexmacros.def name zero_pat (CamlCode d)

let rec do_endsnippet _ =
  if !Lexstate.withinLispComment then begin
    endSnippetRead := true;
    raise (Misc.EndOfLispComment 0)
  end;
  if !Scan.cur_env = "snippet" then
    raise EndSnippet
  else 
    raise (Misc.ScanError ("\\endsnippet without opening \\snippet"))
    
and do_texinclusion lexbuf =
  Scan.top_open_block "SPAN" 
    ("class=\"" ^ !snippetLanguage ^ "Inclusion\"");
  snippetRunHook Scan.main "BeforeTeX";
  withinTeXInclusion := true;
  begin (* Until a \] is read *)
    try Scan.main lexbuf with 
    | exc -> begin
        snippetRunHook Scan.main "AfterTeX";
        Scan.top_close_block "SPAN";
        snippetRunHook Scan.main "Restart";
        (* Re-raise every thing but EndTeXInclusion *)
        try raise exc with
        | EndTeXInclusion -> ()
    end;
  end ;  

and do_texexclusion _ =
 if !withinSnippet then begin
   if !verbose > 2 then prerr_endline "\\] caught within TeX escape"; 
   withinTeXInclusion := false;
   raise EndTeXInclusion
 end else
   raise (Misc.ScanError ("\\] without opening \\[ in snippet"))

and do_backslash_newline  _ =
  Dest.put "\\\n";
  Lexstate.scan_this snippetenv "\n"

and do_four_backslashes _ = Dest.put "\\"

(* HACK: Define a macro with a body that is obtained via substitution.
   This is a kind of restricted \edef as in TeX.
   Syntax:    \@EDEF\macroName{#2#1..}                                 *)

and do_edef _lxm lexbuf =
  let name = Scan.get_csname lexbuf in
  let body = subst_arg lexbuf in
  if Scan.echo_toimage () then 
    Image.put ("\\def"^name^"{"^body^"}\n") ;
  Latexmacros.def name zero_pat (caml_print body);
  ()

(* Syntax:  \@MULEDEF{\macroName,\macroName,...}{#1#3...} 
   This is an awful hack extending the \@EDEF command. It locally
   rebinds the (comma-separated) \macronames to the corresponding
   (comma-separated) expansion of second argument. All \macronames
   should be a zero-ary macro. *)

and do_muledef lxm lexbuf =
  let names = subst_arg lexbuf in
  let bodies = subst_arg lexbuf in
  let rec bind lasti lastj =
    try let i = String.index_from names lasti ',' in
    try let j = String.index_from bodies lastj ',' in
    let name = String.sub names lasti (i - lasti) in
    let body = String.sub bodies lastj (j - lastj) in
    if !verbose > 2 then prerr_endline (lxm ^ name ^ ";" ^ body);
    Latexmacros.def name zero_pat (caml_print body);
      bind (i+1) (j+1)
    with Not_found -> failwith "Missing bodies for \\@MULEDEF"
    with Not_found ->
      let name = String.sub names lasti (String.length names - lasti) in
      let body = String.sub bodies lastj (String.length bodies - lastj) in
      if !verbose > 2 then prerr_endline (lxm ^ name ^ ";" ^ body);
      Latexmacros.def name zero_pat (caml_print body) ;
  in bind 0 0;
  ()


(* The command that starts the \snippet inner environment: *)

and do_snippet lexbuf =
  if !withinSnippet
  then raise (Misc.ScanError "No snippet within snippet.")
  else begin
    (* Obtain the current TeX value of \snippetDefaultLanguage *)
    let snippetDefaultLanguage =   "\\snippetDefaultLanguage" in
    let language = get_prim_opt snippetDefaultLanguage lexbuf in
    let language = if language = "" then snippetDefaultLanguage
                                    else language in
    skip_blanks_till_eol_included lexbuf;
    Dest.put "<BR>\n";
    Scan.top_open_block "DIV" ("class=\"div" ^ language ^ "\"");
    Dest.put "\n";
    Scan.new_env "snippet";
    (* Define commands local to \snippet *)
    snippet_def "\\endsnippet"  do_endsnippet;
    snippet_def "\\[" do_texinclusion ;
    snippet_def "\\]" do_texexclusion ;
    snippet_def "\\\\" do_four_backslashes ;
    snippet_def "\\\n" do_backslash_newline ;

    snippetLanguage := language;
    enableLispComment := false;
    enableSchemeCharacters := false;
    withinSnippet := true;
    snippetRunHook Scan.main "Before";
    try snippetenv lexbuf with 
      exc -> begin
        snippetRunHook Scan.main "AfterLine";
        snippetRunHook Scan.main "After";
        withinSnippet := false;
        Scan.close_env "snippet";
        Scan.top_close_block "DIV";
        (* Re-raise all exceptions but EndSnippet *)
        try raise exc with
          EndSnippet -> ()
      end;
  end

and do_enable_backslashed_chars lexbuf =
  let def_echo s = snippet_def s (fun _ -> Dest.put s) in
  let chars = subst_arg lexbuf in begin
  if !verbose > 2 then prerr_endline ("\\enableBackslashedChar "^chars);
  for i=0 to (String.length chars - 1) do
    let charcommandname = "\\" ^ (String.sub chars i 1) in 
    def_echo charcommandname;
  done; 
  end;
  ()

and do_enableLispComment _lexbuf =
  enableLispComment := true;
  ()

and do_disableLispComment _lexbuf =
  enableLispComment := false;
  ()

and do_enableSchemeCharacters _lexbuf =
  enableSchemeCharacters := true;
  ()

and do_disableSchemeCharacters _lexbuf =
  enableSchemeCharacters := false;
  ()

and do_snippet_run_hook lexbuf =
  let name = subst_arg lexbuf in begin
    snippetRunHook Scan.main name;
    ()
  end

and do_snip_run_hook lexbuf =
  let name = subst_arg lexbuf in begin
    snipRunHook Scan.main name;
    ()
  end

(* These macros are defined in Caml since they are not nullary macros.
   They require some arguments but they cannot get them in the snippet
   environment. So I code them by hand. *)

and do_vicanchor lexbuf = begin
  let {arg=style} = Lexstate.save_opt "" lexbuf in
  let {arg=nfn}   = Lexstate.save_opt "0,filename,notename" lexbuf in
  let fields =
    comma_separated_values (MyLexing.from_list (nfn @ [","])) in
  match fields with
  | [number;filename;notename] -> 
      begin
        let uniqueNumber = (* Would be better: truncate(Unix.gettimeofday()) *)
          increment_internal_counter()
        and hintId = compute_hint_id number filename notename in
        let style = String.concat "" style
        and nfn = String.concat "" nfn in
        Dest.put_tag ("<A id=\"a" ^ string_of_int(uniqueNumber)
                      ^ "__" ^ hintId 
                      ^ "\" href=\"javascript: void showMessage('"
                      ^ hintId ^ "')\" class=\"mousable\"><SPAN style=\"" 
                      ^ style ^ "\"><!-- " ^ nfn ^ " -->");
        ()
      end
  | _ -> failwith "Missing comma-separated arguments"
end

and do_vicendanchor lexbuf = begin
  let {arg=nfn} = Lexstate.save_opt "0,filename,notename" lexbuf in
  let nfn = String.concat "" nfn in
  if !verbose > 2 then prerr_endline ("\\vicendanchor"^nfn);
  let fields = 
    comma_separated_values (MyLexing.from_string (nfn ^ ",")) in
  match fields with
  | [_number;_filename;_notename] -> begin
      Dest.put_tag ("</SPAN></A>");
      ()
  end
  | _ -> failwith "Missing comma-separated arguments"
end

and do_vicindex lexbuf = begin
  let _nfn = Lexstate.save_opt "0,filename,notename" lexbuf in
  Dest.put_char ' ';
  ()
end  
;;


(* This is the initialization function of the plugin: *)

let init = function () -> 
  begin
    (* Register global TeX macros: *)
    def_code "\\snippet"                    do_snippet;
    def_name_code "\\@EDEF"                 do_edef;
    def_name_code "\\@MULEDEF"              do_muledef;

    def_code "\\ViCEndAnchor"               do_vicendanchor;
    def_code "\\ViCAnchor"                  do_vicanchor;
    def_code "\\ViCIndex"                   do_vicindex;

    def_code "\\enableLispComment"          do_enableLispComment;
    def_code "\\disableLispComment"         do_disableLispComment;
    def_code "\\enableSchemeCharacters"     do_enableSchemeCharacters;
    def_code "\\disableSchemeCharacters"    do_disableSchemeCharacters;
    def_code "\\enableBackslashedChars"     do_enable_backslashed_chars;
    def_code "\\snippetRunHook"             do_snippet_run_hook;
    def_code "\\snipRunHook"                do_snip_run_hook;
    ()
  end;;

register_init "videoc" init
;;

let rien = ()

end}


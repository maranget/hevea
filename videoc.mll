(* <Christian.Queinnec@lip6.fr>
 The plugin for HeVeA that implements the VideoC style.
 $Id: videoc.mll,v 1.1 1999-03-08 18:37:41 maranget Exp $ 
*)

{
module type T =
  sig
    val init : unit -> unit
  end;;

module Makealso (Scan : Latexscan.S) =
struct
open Misc
open Parse_opts
open Lexing
open Myfiles
open Latexmacros
(* open Html *)

let header = 
  "$Id: videoc.mll,v 1.1 1999-03-08 18:37:41 maranget Exp $"

(* Re-link with these variables inserted in latexscan. *)

let withinSnippet = Scan.withinSnippet;;
let withinLispComment = Scan.withinLispComment;;
let afterLispCommentNewlines = Scan.afterLispCommentNewlines;;

(* Snippet global defaults *)

let snippetLanguage = ref "";;
let enableLispComment = ref false;;
let enableSchemeCharacters = ref false;;
let snippetCSSstyle = 
  "style=\"font: monospace; color: rgb(255,0,0); \"";;
let snippetCSStexInclusion = 
  "style=\"font: serif bold; color: rgb(0,0,255); \"";;
let snippetCSSlispComment = 
  "style=\"font: serif italic; color: rgb(0,255,0); \"";;

(* Snippet Environment: run a series of hooks provided they exist as
   user macros. *)

let snippetRunHook parsing name =
  let run name = begin
    if !verbose > 2 then prerr_endline ("Trying to run hook " ^ name);
    if exists_macro name 
    then begin Scan.scan_this parsing name; () end
  end in
  let rec iterate name suffix =
    run name;
    if suffix <> ""
    then iterate (name ^ (String.make 1 (String.get suffix 0)))
                 (String.sub suffix 1 ((String.length suffix) - 1))
  in iterate ("\\snippet" ^ name ^ "Hook") !snippetLanguage;;

(* Hack for mutual recursion: *)

let handle_command = ref
  ((function lexbuf -> function s -> ()) 
     : (Lexing.lexbuf -> string -> unit));;

} 

let command_name = '\\' (('@' ? ['A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])

rule snippetenv = parse 
| eof { () }
| command_name
    {let lxm = lexeme lexbuf in
     !handle_command lexbuf lxm}
| '\n'
    {Html.put "<BR>\n";
     snippetRunHook Scan.main "AfterLine";
     snippetRunHook Scan.main "BeforeLine";
     snippetenv lexbuf}
| ' '
    {Html.put "&nbsp;";
     snippetenv lexbuf}
| '\t'
    {for i=1 to !Scan.tab_val do
      Html.put "&nbsp;"
     done;
     snippetenv lexbuf}
| ';' + 
    {Html.put (lexeme lexbuf);
     Html.put_char ' ';
     if !enableLispComment
     then begin
        if !verbose > 1 then 
          prerr_endline "Within snippet: Lisp comment enabled";
        withinLispComment := true;
        afterLispCommentNewlines := 0;
        Scan.top_open_block "SPAN" snippetCSSlispComment;
        Scan.top_open_block "SPAN" 
          ("class=\"" ^ !snippetLanguage ^ "Comment\"");
        snippetRunHook Scan.main "BeforeComment";
        Scan.main lexbuf; (* until a \n is read *)
        snippetRunHook Scan.main "AfterComment";
        Scan.top_close_block "SPAN";
        Scan.top_close_block "SPAN";
        withinLispComment := false;
        let nlnum = !afterLispCommentNewlines in
        afterLispCommentNewlines := 0;
        let _ = Scan.scan_this snippetenv (String.make nlnum '\n') in
        ()
     end;
     snippetenv lexbuf}
| '#'
    {Html.put_char '#';
     if !enableSchemeCharacters
     then begin
        if !verbose > 1 then 
          prerr_endline "Within snippet: scheme characters enabled";
        schemecharacterenv lexbuf
     end;
     snippetenv lexbuf}
| _ 
    {Html.put (Html.iso (lexeme_char lexbuf 0));
     snippetenv lexbuf}

(* Scheme characters are written as #\A or #\Newspace *)

and schemecharacterenv = parse
| command_name
    {let csname = lexeme lexbuf in
     Html.put csname}
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

let rec do_handle_command lexbuf csname =
  if !verbose > 1 then
    prerr_endline ("Within snippet: found csname:" ^ csname ^ ".");

  (* End the snippet environment and return back to the previous mode: *)
  if csname = "\\endsnippet" then begin
    if !Scan.cur_env = "snippet" then () else 
    failwith "\\endsnippet without matching \\snippet"
  (* Start a TeX inclusion within a snippet: *)
  end else if csname = "\\[" then begin
    Scan.top_open_block "SPAN" snippetCSStexInclusion;
    Scan.top_open_block "SPAN" 
      ("class=\"" ^ !snippetLanguage ^ "Inclusion\"");
    snippetRunHook Scan.main "BeforeTeX";
    Scan.main lexbuf; (* Until a \] is read *)
    snippetRunHook Scan.main "AfterTeX";
    Scan.top_close_block "SPAN";
    Scan.top_close_block "SPAN";
    snippetRunHook Scan.main "Restart";
    snippetenv lexbuf

 (* Handle a backslash newline to let it appear as it is: *)
  end else if csname = "\\\n" then begin
    Html.put csname;
    Scan.scan_this snippetenv "\n";
    snippetenv lexbuf

  end else if csname = "\\ViCIndex" then begin
    let lxm = lexeme lexbuf in
    do_vicindex lexbuf lxm;
    snippetenv lexbuf
  end else if csname = "\\ViCAnchor" then begin
    let lxm = lexeme lexbuf in
    do_vicanchor lexbuf lxm;
    snippetenv lexbuf
  end else if csname = "\\ViCEndAnchor" then begin
    let lxm = lexeme lexbuf in
    do_vicendanchor lexbuf lxm;
    snippetenv lexbuf

  end else if csname = "\\\\" then begin
      let lxm = lexeme lexbuf in
      do_four_backslashes lexbuf lxm;
      snippetenv lexbuf

  (* Execute the nullary macro with Scan.main then return to the snippetenv: *)
  end else begin
    Scan.scan_this Scan.main csname;
    snippetenv lexbuf
  end

(* These are individual functions associated to TeX macros.
   They process the macro and return nil. They do not start or 
   end inner modes. *)

and do_four_backslashes lexbuf name = begin
  Html.put "\\";
  ()
end

(*************************
and do_reference_url lexbuf lxm =
  let txt = Save.arg lexbuf in
  let url = Save.arg_verbatim lexbuf in
  Html.put ("<A href=\"" ^ url ^ "\">" ^ txt ^ "</A>");
  ()

and do_single_url lexbuf lxm =
  let url = Save.arg_verbatim lexbuf in
  Html.put ("<A href=\"" ^ url ^ "\">" ^ url ^ "</A>");
  ()
***************************)

and do_define_url lexbuf lxm =
  let name = Save.csname lexbuf in
  let body = Save.arg_verbatim lexbuf in
  if !Scan.env_level = 0 then 
    Image.put (lxm^name^"{"^body^"}\n")
  else 
    Scan.macro_register name;
  begin try
    def_macro name 0 (Print body);
  with Latexmacros.Failed -> () end ;
  ()

(* HACK: Define a macro with a body that is obtained via substitution.
   This is a kind of restricted \edef as in TeX.
   Syntax:    \@EDEF\macroName{#2#1..}                                 *)

and do_edef lexbuf lxm =
  let name = Scan.subst_arg Scan.subst lexbuf in
  let body = Scan.subst_arg Scan.subst lexbuf in
  if !Scan.env_level = 0 then 
    Image.put (lxm^name^"{"^body^"}\n")
  else 
    Scan.macro_register name;
  begin try
    def_macro name 0 (Print body);
  with Latexmacros.Failed -> () end ;
  ()

(* Syntax:  \@MULEDEF{\macroName,\macroName,...}{#1#3...} 
   This is an awful hack extending the \@EDEF command. It locally
   rebinds the (comma-separated) \macronames to the corresponding
   (comma-separated) expansion of second argument. All \macronames
   should be a zero-ary macro. *)

and do_muledef lexbuf lxm =
  let names = Scan.subst_arg Scan.subst lexbuf in
  let bodies = Scan.subst_arg Scan.subst lexbuf in
  let rec bind lasti lastj =
    try let i = String.index_from names lasti ',' in
    try let j = String.index_from bodies lastj ',' in
    let name = String.sub names lasti (i - lasti) in
    let body = String.sub bodies lastj (j - lastj) in
    if !verbose > 2 then prerr_endline (lxm ^ name ^ ";" ^ body);
    try (if exists_macro name then redef_macro else def_macro)
        name 0 (Print body);
      Scan.macro_register name;
    with Failed -> ();
      bind (i+1) (j+1)
    with Not_found -> failwith "Missing bodies for \\@MULEDEF"
    with Not_found ->
      let name = String.sub names lasti (String.length names - lasti) in
      let body = String.sub bodies lastj (String.length bodies - lastj) in
      if !verbose > 2 then prerr_endline (lxm ^ name ^ ";" ^ body);
      try (if exists_macro name then redef_macro else def_macro)
          name 0 (Print body);
        Scan.macro_register name
      with Failed -> ();
  in bind 0 0;
  ()

(* The command that starts the \snippet inner environment: *)

and do_snippet lexbuf lxm =
  if !withinSnippet
  then failwith "No snippet within snippet."
  else begin
    let snippetDefaultLanguage = 
      Scan.get_this Scan.main "\\snippetDefaultLanguage" in
    let language = Scan.save_opt snippetDefaultLanguage lexbuf in
    let language = if language = "" then snippetDefaultLanguage
                                    else language in
    skip_blanks_till_eol_included lexbuf;
    Html.put "\n";
    Scan.top_open_block "TT" "";
    Scan.top_open_block "DIV" snippetCSSstyle;
    Scan.top_open_block "SPAN" ("class=\"" ^ language ^ "\"");
    Html.put "\n";
    Scan.new_env "snippet";
    (* Register local commands *)
    def_macro "\\endsnippet" 0 (CamlCode !handle_command);
    Scan.macro_register "\\endsnippet";
    def_macro "\\[" 0 (CamlCode !handle_command);
    Scan.macro_register "\\[";
    def_macro "\\\\" 0 (CamlCode do_four_backslashes);
    Scan.macro_register "\\\\";
    redef_macro "\\\n" 0 (CamlCode !handle_command);
    Scan.macro_register "\\n";
    snippetLanguage := language;
    enableLispComment := false;
    enableSchemeCharacters := false;
    withinSnippet := true;
    snippetRunHook Scan.main "Before";
    snippetenv lexbuf;
    snippetRunHook Scan.main "AfterLine";
    snippetRunHook Scan.main "After";
    withinSnippet := false;
    Scan.close_env !Scan.cur_env;
    Scan.top_close_block "SPAN";
    Scan.top_close_block "DIV";
    Scan.top_close_block "TT";
  end

and do_enable_some_backslashed_chars lexbuf lxm =
  def_macro "\\n" 0 (Print "\\n"); Scan.macro_register "\\n";
  def_macro "\\r" 0 (Print "\\r"); Scan.macro_register "\\r";
  def_macro "\\0" 0 (Print "\\0"); Scan.macro_register "\\0";
  def_macro "\\t" 0 (Print "\\t"); Scan.macro_register "\\t";
  def_macro "\\f" 0 (Print "\\f"); Scan.macro_register "\\f";
  ()  

and do_enableLispComment lexbuf lxm =
  enableLispComment := true;
  ()

and do_disableLispComment lexbuf lxm =
  enableLispComment := false;
  ()

and do_enableSchemeCharacters lexbuf lxm =
  enableSchemeCharacters := true;
  ()

and do_disableSchemeCharacters lexbuf lxm =
  enableSchemeCharacters := false;
  ()

(* These macros are defined in Caml since they are not nullary macros.
   They require some arguments but they cannot get them in the snippet
   environment. So I code them by hand. *)

and do_vicanchor lexbuf lxm = begin
  let style = Scan.save_opt "" lexbuf in
  if !verbose > 2 then prerr_endline ("\\vicanchor"^style);
  let nfn   = Scan.save_opt "0,filename,notename" lexbuf in
  if !verbose > 2 then prerr_endline ("\\vicanchor"^style^nfn);
  let fields =
    comma_separated_values (Lexing.from_string (nfn ^ ",")) in
  match fields with 
  | [number;filename;notename] -> begin
      Html.put ("<script language=\"JavaScript\"><!-- 
 last_links_length = document.links.length; // --></script><A href='#' class='mo
usable'><SPAN style=\"" ^ style ^ "\"><!-- " 
  ^ nfn ^ " -->");
      ()
    end
  | _ -> failwith "Missing comma-separated arguments"
end

and do_vicendanchor lexbuf lxm = begin
  let nfn = Scan.save_opt "0,filename,notename" lexbuf in
  if !verbose > 2 then prerr_endline ("\\vicendanchor"^nfn);
  let fields = 
    comma_separated_values (Lexing.from_string (nfn ^ ",")) in
  match fields with
  | [number;filename;notename] -> begin
      Html.put ("</SPAN></A><script language=\"JavaScript\"><!-- \n hint_push("
                ^ number ^ "); // --></script>");
      ()
  end
  | _ -> failwith "Missing comma-separated arguments"
end  

and do_vicindex lexbuf lxm = begin
  let nfn = Scan.save_opt "0,filename,notename" lexbuf in
  Html.put_char ' ';
  ()
end
;;

handle_command := do_handle_command;;

(* This is the initialization function of the plugin: *)

let init = function () -> 
  begin
    (* Register global TeX macros: *)
    def_macro "\\defineURL" 0 (CamlCode do_define_url);
    (********
    def_macro "\\referenceURL" 0 (CamlCode do_reference_url);
    def_macro "\\singleURL" 0 (CamlCode do_single_url);
    **********)
    def_macro "\\@EDEF" 0 (CamlCode do_edef);
    def_macro "\\@MULEDEF" 0 (CamlCode do_muledef);
    def_macro "\\snippet" 0 (CamlCode do_snippet);
    def_macro "\\ViCIndex" 0 (CamlCode do_vicindex);
    def_macro "\\ViCEndAnchor" 0 (CamlCode do_vicendanchor);
    def_macro "\\ViCAnchor" 0 (CamlCode do_vicanchor);
    def_macro "\\enableLispComment" 0 (CamlCode do_enableLispComment);
    def_macro "\\disableLispComment" 0 (CamlCode do_disableLispComment);
    def_macro "\\enableSchemeCharacters" 0 
      (CamlCode do_enableSchemeCharacters);
    def_macro "\\disableSchemeCharacters" 0 
      (CamlCode do_disableSchemeCharacters);
    def_macro "\\enableSomeBackslashedChars" 0
      (CamlCode do_enable_some_backslashed_chars);
    ()
  end;;

end}

(* end of videoc.mll *)

(* <Christian.Queinnec@lip6.fr>
 The plugin for HeVeA that implements the VideoC style.
 $Id: videoc.mll,v 1.6 1999-05-21 12:54:19 maranget Exp $ 
*)

{
module type T =
  sig
    val init : unit -> unit
  end;;

module Makealso (Dest : OutManager.S) (Scan : Latexscan.S) =
struct
open Misc
open Parse_opts
open Lexing
open Myfiles
open Lexstate
open Latexmacros


let header = 
  "$Id: videoc.mll,v 1.6 1999-05-21 12:54:19 maranget Exp $"

exception EndSnippet
;;
exception EndTeXInclusion
;;
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
    then begin Lexstate.scan_this parsing name; () end
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
    {let csname = lexeme lexbuf in
    let pat,body = find_macro csname in
    begin match pat with
    | [],[] ->
      let args =  make_stack csname pat lexbuf in
      let exec = function
        | Print str -> Dest.put str
        | Subst body ->
            if !verbose > 2 then
              prerr_endline ("user macro in snippet: "^body) ;            
            scan_this snippetenv body
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
| ' '
    {Dest.put_nbsp ();
     snippetenv lexbuf}
| '\t'
    {for i=1 to !Lexstate.tab_val do
      Dest.put_nbsp ()
     done;
     snippetenv lexbuf}
| ';' + 
    {Dest.put (lexeme lexbuf);
     Dest.put_char ' ';
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
        let _ = Lexstate.scan_this snippetenv (String.make nlnum '\n') in
        ()
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
| _ 
    {Dest.put (Dest.iso (lexeme_char lexbuf 0));
     snippetenv lexbuf}

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

let rec do_endsnippet _ =
  if !Scan.cur_env = "snippet" then
    raise EndSnippet
  else 
    failwith "\\endsnippet without matching \\snippet"
    
and do_texinclusion lexbuf =
  Scan.top_open_block "SPAN" snippetCSStexInclusion;
  Scan.top_open_block "SPAN" 
    ("class=\"" ^ !snippetLanguage ^ "Inclusion\"");
  snippetRunHook Scan.main "BeforeTeX";
  begin
    try Scan.main lexbuf with EndTeXInclusion -> ()
  end ; (* Until a \] is read *)
  snippetRunHook Scan.main "AfterTeX";
  Scan.top_close_block "SPAN";
  Scan.top_close_block "SPAN";
  snippetRunHook Scan.main "Restart"

and do_texexclusion _ =
 if !withinSnippet then begin
   if !verbose > 2 then prerr_endline "\\] caught within TeX escape"; 
   raise EndTeXInclusion
 end else
   raise (Misc.ScanError ("\\] without opening \\[ in snippet"))

and do_backslash_newline  _ =
  Dest.put "\\\n";
  Lexstate.scan_this snippetenv "\n";

and do_four_backslashes _ = Dest.put "\\"


(*************************
and do_reference_url lexbuf  =
  let txt = Save.arg lexbuf in
  let url = Save.arg_verbatim lexbuf in
  Dest.put ("<A href=\"" ^ url ^ "\">" ^ txt ^ "</A>");
  ()

and do_single_url lexbuf =
  let url = Save.arg_verbatim lexbuf in
  Dest.put ("<A href=\"" ^ url ^ "\">" ^ url ^ "</A>");
  ()
***************************)

and do_define_url lxm lexbuf =
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

and do_edef lxm lexbuf =
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

and do_muledef lxm lexbuf =
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

and do_snippet lexbuf =
  if !withinSnippet
  then raise (Misc.ScanError "No snippet within snippet.")
  else begin
    let snippetDefaultLanguage = 
      Scan.get_this Scan.main "\\snippetDefaultLanguage" in
    let language = Lexstate.save_opt snippetDefaultLanguage lexbuf in
    let language = if language = "" then snippetDefaultLanguage
                                    else language in
    skip_blanks_till_eol_included lexbuf;
    Dest.put "\n";
    Scan.top_open_block "DIV" snippetCSSstyle;
    Dest.open_mod (Latexmacros.Style "TT") ;
    Scan.top_open_block "SPAN" ("class=\"" ^ language ^ "\"");
    Dest.put "\n";
    Scan.new_env "snippet";
    (* Register local commands *)
    def_code "\\endsnippet" do_endsnippet;
    Scan.macro_register "\\endsnippet";
    redef_code "\\[" do_texinclusion ;
    Scan.macro_register "\\[";
    redef_code "\\]" do_texexclusion ;
    Scan.macro_register "\\[";
    redef_code "\\\\" do_four_backslashes;
    Scan.macro_register "\\\\";
    redef_code "\\\n" do_backslash_newline ;
    Scan.macro_register "\\n";
    snippetLanguage := language;
    enableLispComment := false;
    enableSchemeCharacters := false;
    withinSnippet := true;
    snippetRunHook Scan.main "Before";
    begin try snippetenv lexbuf with EndSnippet -> () end ;
    snippetRunHook Scan.main "AfterLine";
    snippetRunHook Scan.main "After";
    withinSnippet := false;
    Scan.close_env !Scan.cur_env;
    Scan.top_close_block "SPAN";
    Scan.top_close_block "DIV";
  end

and do_enable_some_backslashed_chars lexbuf =
  def_macro "\\n" 0 (Print "\\n"); Scan.macro_register "\\n";
  def_macro "\\r" 0 (Print "\\r"); Scan.macro_register "\\r";
  def_macro "\\0" 0 (Print "\\0"); Scan.macro_register "\\0";
  def_macro "\\t" 0 (Print "\\t"); Scan.macro_register "\\t";
  def_macro "\\f" 0 (Print "\\f"); Scan.macro_register "\\f";
  ()  

and do_enableLispComment lexbuf =
  enableLispComment := true;
  ()

and do_disableLispComment lexbuf =
  enableLispComment := false;
  ()

and do_enableSchemeCharacters lexbuf =
  enableSchemeCharacters := true;
  ()

and do_disableSchemeCharacters lexbuf =
  enableSchemeCharacters := false;
  ()

(* These macros are defined in Caml since they are not nullary macros.
   They require some arguments but they cannot get them in the snippet
   environment. So I code them by hand. *)

and do_vicanchor lexbuf = begin
  let style = Lexstate.save_opt "" lexbuf in
  if !verbose > 2 then prerr_endline ("\\vicanchor"^style);
  let nfn   = Lexstate.save_opt "0,filename,notename" lexbuf in
  if !verbose > 2 then prerr_endline ("\\vicanchor"^style^nfn);
  let fields =
    comma_separated_values (Lexing.from_string (nfn ^ ",")) in
  match fields with 
  | [number;filename;notename] -> begin
      Dest.put_tag ("<script language=\"JavaScript\"><!-- 
 last_links_length = document.links.length; // --></script><A href='#' class='mo
usable'><SPAN style=\"" ^ style ^ "\"><!-- " 
  ^ nfn ^ " -->");
      ()
    end
  | _ -> failwith "Missing comma-separated arguments"
end

and do_vicendanchor lexbuf = begin
  let nfn = Lexstate.save_opt "0,filename,notename" lexbuf in
  if !verbose > 2 then prerr_endline ("\\vicendanchor"^nfn);
  let fields = 
    comma_separated_values (Lexing.from_string (nfn ^ ",")) in
  match fields with
  | [number;filename;notename] -> begin
      Dest.put_tag ("</SPAN></A><script language=\"JavaScript\"><!-- \n hint_push("
                ^ number ^ "); // --></script>");
      ()
  end
  | _ -> failwith "Missing comma-separated arguments"
end  

and do_vicindex lexbuf = begin
  let nfn = Lexstate.save_opt "0,filename,notename" lexbuf in
  Dest.put_char ' ';
  ()
end
;;


(* This is the initialization function of the plugin: *)

let init = function () -> 
  begin
    (* Register global TeX macros: *)
    def_name_code "\\defineURL" do_define_url;
    (********
    def_code "\\referenceURL" do_reference_url ;
    def_code "\\singleURL" do_single_url;
    **********)
    def_name_code "\\@EDEF" do_edef;
    def_name_code "\\@MULEDEF" do_muledef;
    def_code "\\snippet" do_snippet;
    def_code "\\ViCIndex" do_vicindex;
    def_code "\\ViCEndAnchor" do_vicendanchor;
    def_code "\\ViCAnchor" do_vicanchor;
    def_code "\\enableLispComment" do_enableLispComment;
    def_code "\\disableLispComment" do_disableLispComment;
    def_code "\\enableSchemeCharacters" do_enableSchemeCharacters;
    def_code "\\disableSchemeCharacters" do_disableSchemeCharacters;
    def_code "\\enableSomeBackslashedChars"
      do_enable_some_backslashed_chars;
    ()
  end;;

end}

(* end of videoc.mll *)

(* <Christian.Queinnec@lip6.fr>
 The plugin for HeVeA that implements the VideoC style.
 $Id: videoc.mll,v 1.18 2000-01-28 15:40:19 maranget Exp $ 
*)

{
module type T =
  sig
  end;;

module Make
    (Dest : OutManager.S)
    (Image : ImageManager.S)
    (Scan : Latexscan.S) =
struct
open Misc
open Parse_opts
open Lexing
open Myfiles
open Lexstate
open Latexmacros
open Subst
open Scan


let header = 
  "$Id: videoc.mll,v 1.18 2000-01-28 15:40:19 maranget Exp $"
(* So I can synchronize my changes from Luc's ones *)
let qnc_header = 
  "17 aout 99"

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

(* Hack for mutual recursion between modules: *)

let handle_command = ref
  ((function lexbuf -> function s -> ()) 
     : (Lexing.lexbuf -> string -> unit));;

(* Convert a reference to a hint such as "3" "annote.ann" "premier indice"
   into "3_annote_ann". This is needed for the annote tool.  *)

let compute_hint_id number filename notename =
  let result = number ^ "_" ^ filename in
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
  result;;

let increment_internal_counter =
  let counter = ref 99 in
  function () -> 
    begin
      counter := !counter + 1;
      !counter
    end;;

} 

let command_name = '\\' ((['@''A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])

rule snippetenv = parse 
| eof { () }
| command_name
   {let csname = lexeme lexbuf in
    let pat,body = find_macro csname in
    begin match pat with
    | [],[] ->
      let args =  make_stack csname pat lexbuf in
      let cur_subst = get_subst () in
      let exec = function
        | Subst body ->
            if !verbose > 2 then
              prerr_endline ("user macro in snippet: "^body) ;
            Lexstate.scan_this_may_cont Scan.main
              lexbuf cur_subst (body,get_subst ())
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
    {Dest.put_nbsp ();
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
            | Misc.EndOfLispComment i -> begin
                let nlnum = i in
                let addon = (if !endSnippetRead then "\\endsnippet" else "") in
                if !verbose > 1 then 
                  Printf.fprintf stderr "%d NL after LispComment %s\n" 
                    nlnum ((if !endSnippetRead then "and " else "")^addon);
                let _ = Lexstate.scan_this snippetenv 
                    ((String.make nlnum '\n')^addon) in
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
let caml_print s = CamlCode (fun _ -> Dest.put s)

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

(* Parse an url that may contain a defined url macro.
   Url macros are defined as in \defineURL\QNC{http://h.o.st:80/~queinnec}
   and used as in \referenceURL{bla bla}{\QNC/index.html}. *)

and expand_url_macros lexbuf =
   let url = Save.arg_verbatim lexbuf in
   let url = get_this_main url in
   url

and do_reference_url lexbuf  =
  let txt = save_arg lexbuf in
  let url = expand_url_macros lexbuf in
  Dest.put ("<A href=\"" ^ url ^ "\" class=\"referenceURL\">");
  scan_this_arg main txt;
  Dest.put "</A>";
  ()

and do_single_url lexbuf =
  let url = expand_url_macros lexbuf in
  Dest.put ("<A href=\"" ^ url ^ "\" class=\"referenceURL\">" ^ url ^ "</A>");
  ()

and do_define_url lxm lexbuf =
  let name = Scan.get_csname lexbuf in
  let body = Save.arg_verbatim lexbuf in
  if !Scan.env_level = 0 then 
    Image.put (lxm^name^"{"^body^"}\n")
  else 
    Scan.macro_register name;
  begin try
    def_code name (make_do_defined_macro_url body);
  with Latexmacros.Failed -> () end ;
  ()

and make_do_defined_macro_url body lexbuf =
  Dest.put body;
  () 

(* HACK: Define a macro with a body that is obtained via substitution.
   This is a kind of restricted \edef as in TeX.
   Syntax:    \@EDEF\macroName{#2#1..}                                 *)

and do_edef lxm lexbuf =
  let name = subst_arg lexbuf in
  let body = subst_arg lexbuf in
  if !Scan.env_level = 0 then 
    Image.put ("\\def"^name^"{"^body^"}\n")
  else 
    Scan.macro_register name;
  begin try
    def_macro name 0 (caml_print body);
  with Latexmacros.Failed -> () end ;
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
    silent_def name 0 (caml_print body);
    Scan.macro_register name;
      bind (i+1) (j+1)
    with Not_found -> failwith "Missing bodies for \\@MULEDEF"
    with Not_found ->
      let name = String.sub names lasti (String.length names - lasti) in
      let body = String.sub bodies lastj (String.length bodies - lastj) in
      if !verbose > 2 then prerr_endline (lxm ^ name ^ ";" ^ body);
      silent_def name 0 (caml_print body) ;
      Scan.macro_register name ;
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
    Scan.top_open_block "DIV" ("class=\"" ^ language ^ "\"");
    Dest.put "\n";
    Scan.new_env "snippet";
    (* Register commands local to \snippet *)
    def_code "\\endsnippet" do_endsnippet;
    Scan.macro_register "\\endsnippet";
    (* Use silent_def since I don't whether they are defined in the outer
       LaTeX environment. *)
    silent_def "\\[" 0 (CamlCode do_texinclusion);
    Scan.macro_register "\\["; 
    silent_def "\\]" 0 (CamlCode do_texexclusion);
    Scan.macro_register "\\[";
    silent_def "\\\\" 0 (CamlCode do_four_backslashes);
    Scan.macro_register "\\\\";
    silent_def "\\\n" 0 (CamlCode do_backslash_newline);
    Scan.macro_register "\\n";
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

and do_enable_some_backslashed_chars lexbuf =
  def_macro "\\n" 0 (caml_print "\\n"); Scan.macro_register "\\n";
  def_macro "\\r" 0 (caml_print "\\r"); Scan.macro_register "\\r";
  def_macro "\\0" 0 (caml_print "\\0"); Scan.macro_register "\\0";
  def_macro "\\t" 0 (caml_print "\\t"); Scan.macro_register "\\t";
  def_macro "\\f" 0 (caml_print "\\f"); Scan.macro_register "\\f";
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
  let style,_ = Lexstate.save_opt "" lexbuf in
  if !verbose > 2 then prerr_endline ("\\vicanchor"^style);
  let nfn,_   = Lexstate.save_opt "0,filename,notename" lexbuf in
  if !verbose > 2 then prerr_endline ("\\vicanchor"^style^nfn);
  let fields =
    comma_separated_values (Lexing.from_string (nfn ^ ",")) in
  match fields with
  | [number;filename;notename] -> 
      begin
        let uniqueNumber = (* Would be better: truncate(Unix.gettimeofday()) *)
          increment_internal_counter()
        and hintId = compute_hint_id number filename notename in
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
  let nfn,_ = Lexstate.save_opt "0,filename,notename" lexbuf in
  if !verbose > 2 then prerr_endline ("\\vicendanchor"^nfn);
  let fields = 
    comma_separated_values (Lexing.from_string (nfn ^ ",")) in
  match fields with
  | [number;filename;notename] -> begin
      Dest.put_tag ("</SPAN></A>");
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
    def_name_code "\\defineURL"             do_define_url;
    def_code "\\referenceURL"               do_reference_url ;
    def_code "\\singleURL"                  do_single_url;
    def_name_code "\\@EDEF"                 do_edef;
    def_name_code "\\@MULEDEF"              do_muledef;
    def_code "\\snippet"                    do_snippet;
    def_code "\\ViCEndAnchor"               do_vicendanchor;
    def_code "\\ViCAnchor"                  do_vicanchor;
    def_code "\\ViCIndex"                   do_vicindex;
    def_code "\\enableLispComment"          do_enableLispComment;
    def_code "\\disableLispComment"         do_disableLispComment;
    def_code "\\enableSchemeCharacters"     do_enableSchemeCharacters;
    def_code "\\disableSchemeCharacters"    do_disableSchemeCharacters;
    def_code "\\enableSomeBackslashedChars" do_enable_some_backslashed_chars;
    ()
  end;;

register_init "videoc" init
;;

end}

(* end of videoc.mll *)


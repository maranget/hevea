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
let header = "$Id: cut.mll,v 1.25 2000-01-26 17:08:38 maranget Exp $" 

let verbose = ref 0
;;

let language = ref "eng"
;;

let tocbis = ref false
;;

exception Error of string


(* Accumulate all META, LINK and similar tags that appear in the preamble
   in order to output them in the preamble of every generated page. *)

let header_buff = Out.create_buff ()
let common_headers = ref "";;
 
let adjoin_to_header s = Out.put header_buff s

and adjoin_to_header_char c = Out.put_char header_buff c

and finalize_header () =
  common_headers := Out.to_string header_buff

let html_buff = Out.create_buff ()
let html_head = ref ""
and html_foot = ref ""

let push s e = s := e:: !s
and pop s = match !s with
  [] -> raise (Misc.Fatal "Empty stack in Cut")
| e::rs -> s := rs ; e
;;

let phase = ref (-1)
;;

let name = ref "main"
and count = ref 0
;;

let body = ref "<BODY>"
and doctype = ref ""
and html = ref "<HTML>"
;;

let new_filename () =
  incr count ;
  Printf.sprintf "%s%0.3d.html" !name !count
;;

let out = ref (Out.create_null ())
and out_prefix = ref (Out.create_null ())
and outname = ref ""
and lastclosed = ref ""
and otheroutname = ref ""
;;

let toc = ref !out
and tocname = ref !outname
and otherout = ref !out
;;

let start_phase name =
  incr phase ;
  if !verbose > 0 then
    prerr_endline ("Starting phase number: "^string_of_int !phase);
  outname := name ;
  tocname := name ;
  otheroutname := "" ;
  count := 0 ;
  if !phase > 0 then begin
    out := (Out.create_chan (open_out name))
  end ;
  toc := !out
;;

let openlist out = Out.put out "<UL>\n"
and closelist out = Out.put out "</UL>\n"
and itemref filename s out =
  Out.put out "<LI>" ;
  Out.put out "<A HREF=\"" ;
  Out.put out filename ;
  Out.put out "\">" ;
  Out.put out s ;
  Out.put out "</A>\n"

and itemanchor filename label s out =
  Out.put out "<LI>" ;
  Out.put out "<A HREF=\"" ;
  Out.put out filename ;
  Out.put_char out '#' ;
  Out.put out label ;
  Out.put out "\">" ;
  Out.put out s ;
  Out.put out "</A>\n"

and putanchor label out =
  Out.put out "<A NAME=\"" ;
  Out.put out label ;
  Out.put out "\"></A>"

and itemlist s out =
  Out.put out "<LI>" ;
  Out.put out s
;;

let putlink out name img alt =
  Out.put out "<A HREF=\"" ;
  Out.put out name ;
  Out.put out "\"><IMG SRC =\"" ;
  Out.put out img ;
  Out.put out "\" ALT=\"" ;
  Out.put out alt ;
  Out.put out "\"></A>\n"
;;

let putlinks out name =
  if !verbose > 0 then
    prerr_endline ("putlinks: "^name) ;
  begin try
    putlink out (Thread.prev name) "previous_motif.gif" 
      (if !language = "fra" then "Précédent"
       else "Previous")
  with Not_found -> () end ;
  begin try
    putlink out (Thread.up name) "contents_motif.gif" 
      (if !language = "fra" then "Index"
       else "Contents")
  with Not_found -> () end ;
  begin try
    putlink out (Thread.next name) "next_motif.gif" 
      (if !language = "fra" then "Suivant"
       else "Next")
  with Not_found -> () end
;;

let openhtml title out outname =
  Out.put out !doctype ; Out.put_char out '\n' ;
  Out.put out !html ; Out.put_char out '\n' ;
  Out.put out "<HEAD>\n" ;
  Out.put out !common_headers;
  Out.put out "<TITLE>\n" ;
  let title = Save.tagout (Lexing.from_string title) in
  Out.put out title ;
  Out.put out "\n</TITLE>\n" ;
  Out.put out "</HEAD>\n" ;
  Out.put out !body;
  Out.put out "\n" ;
  putlinks out outname ;
  Out.put out "<HR>\n" ;
  Out.put out !html_head


and closehtml name out =
  Out.put out !html_foot ;
  Out.put out "<HR>\n" ;
  putlinks out name ;
  Out.put out "</BODY>\n" ;
  Out.put out "</HTML>\n" ;
  Out.close out
;;

let put_sec hd title hde out =
  Out.put out hd ;
  Out.put_char out '\n' ;
  Out.put out title ;
  Out.put out hde ;
  Out.put_char out '\n'
;;


let put s = Out.put !out s
and put_char c = Out.put_char !out c
;;

let cur_level = ref (Section.value "DOCUMENT")
and chapter = ref (Section.value "CHAPTER")
and depth = ref 2
;;


(* Open all lists in toc from chapter to sec, with sec > chapter *)
let rec do_open l1 l2 =
  if l1 < l2 then begin
    openlist !toc ;
    if !tocbis then openlist !out_prefix ;
    do_open (l1+1) l2
  end
;;

(* close from l1 down to l2 *)
let rec do_close l1 l2 =
  if l1 > l2 then begin
     closelist !toc ;
     if !tocbis then closelist !out_prefix ;
     do_close (l1-1) l2
  end else
  cur_level := l1
;;

let anchor = ref 0
;;

let open_section sec name =
  if !phase > 0 then begin
    if !cur_level > sec then do_close !cur_level sec
    else if !cur_level < sec then do_open  !cur_level sec ;
    incr anchor ;
    let label = "toc"^string_of_int !anchor in
    itemanchor !outname label name !toc ;
    if !tocbis then itemanchor "" label name !out_prefix ;
    putanchor label !out ;
    cur_level := sec
  end else
    cur_level := sec

and close_section sec =
  if !phase > 0 then do_close !cur_level sec
  else
    cur_level := sec
;;

let close_chapter () =
  if !verbose > 0 then
    prerr_endline ("Close chapter out="^ !outname^" toc="^ !tocname) ;
  if !phase > 0 then begin
    closehtml !outname !out ;
    if !tocbis then begin
      let real_out = open_out !outname in
      Out.to_chan real_out !out_prefix ;
      Out.to_chan real_out !out ;
      close_out real_out
    end else
      Out.close !out ;
    out := !toc
  end else begin
    lastclosed := !outname ;
    outname := !tocname
  end

and open_chapter name =
  outname := new_filename () ;
  if !verbose > 0 then
    prerr_endline
      ("Open chapter out="^ !outname^" toc="^ !tocname^
       " cur_level="^string_of_int !cur_level) ;
  if !phase > 0 then begin
    if !tocbis then begin
      out_prefix := Out.create_buff () ;
      out := !out_prefix ;
      openhtml name !out_prefix !outname
    end else begin
      out := Out.create_chan (open_out !outname) ;
      openhtml name !out !outname
    end ;
    itemref !outname name !toc ;
    cur_level := !chapter
  end else begin
    if !verbose > 0 then
      prerr_endline ("link prev="^ !lastclosed^" next="^ !outname) ;
    Thread.setup !outname !tocname ;
    Thread.setprevnext !lastclosed !outname ;
    cur_level := !chapter
  end
;;

let open_notes sec_notes =
  if sec_notes <> !chapter || !outname = !tocname then begin
    otheroutname := !outname ;
    outname := new_filename () ;
    if !phase > 0 then begin
      otherout := !out ;
      out := Out.create_chan (open_out !outname) ;
      Out.put !out !doctype ; Out.put_char !out '\n' ;
      Out.put !out !html ; Out.put_char !out '\n' ;
      Out.put !out "<HEAD><TITLE>Notes</TITLE>\n" ;
      Out.put !out !common_headers ;
      Out.put !out "</HEAD>\n" ;
      Out.put !out !body ;
      Out.put !out "\n"
    end
  end else
   otheroutname := ""

and close_notes () =
  if !otheroutname <> "" then begin
     Out.put !out "\n</BODY></HTML>\n" ;
     Out.close !out ;
     outname := !otheroutname ;
     out := !otherout ;
     otheroutname := ""
  end
;;


let toc_buf = Out.create_buff ()
;;

let stack = ref []
;;

let save_state newchapter newdepth =
  if !verbose > 0 then
    prerr_endline ("New state: "^string_of_int newchapter) ;
  push stack
    (!outname,
     !chapter,!depth,!toc,!tocname,!cur_level,!lastclosed,!out_prefix) ;
  chapter := newchapter ;
  depth := newdepth ;
  tocname := !outname ;
  lastclosed := "" ;
  toc := !out
;;

let restore_state () =
  if !verbose > 0 then prerr_endline ("Restore") ;
  let
    oldoutname,
    oldchapter,olddepth,oldtoc,oldtocname,
    oldlevel,oldlastclosed,oldprefix  = pop stack in
  outname := oldoutname ;
  chapter := oldchapter ;
  depth := olddepth ;
  toc := oldtoc ;
  tocname := oldtocname ;
  lastclosed := !lastclosed ;
  cur_level := oldlevel ;
  out_prefix := oldprefix
;;

let close_top lxm =
  Out.put !out "<!--FOOTER-->\n" ;
  begin try
      Mylib.put_from_lib ("cutfoot-"^ !language^".html") (Out.put !out)
  with Mylib.Error s -> begin
    Location.print_pos () ;
    prerr_endline s
  end end ;
  Out.put !toc lxm ;
  if !tocname = "" then
    Out.flush !toc
  else
   Out.close !toc
;;

let open_toc () = if !phase > 0 then openlist !toc
and close_toc () = if !phase > 0 then closelist !toc
;;

let close_all () =
  if !cur_level > !chapter then begin
    close_section !chapter ;
    close_chapter () ;
    close_toc ()
  end else if !cur_level = !chapter then begin
    close_chapter () ;
    close_toc ()
  end ;
  cur_level := (Section.value "DOCUMENT")
}

  rule main = parse
| "<!--HEVEA" [^'>']* "-->" '\n'?
    {let lxm = lexeme lexbuf in
    if !phase > 0 then begin
      put lxm ;
      put ("<!--HACHA command line is: ") ;
      for i = 0 to Array.length Sys.argv - 1 do
        put Sys.argv.(i) ;
        put_char ' '
      done ;
      put "-->\n"
    end ;
    main lexbuf}
|  "<!--" ("TOC"|"toc") ' '+
    {let arg = secname lexbuf in
    let sn = 
      if String.uppercase arg = "NOW" then !chapter
      else Section.value arg in
    let name = tocline lexbuf in
    if !verbose > 1 then begin
      prerr_endline ("TOC "^arg^" "^name)
    end;
    if sn < !chapter then begin
      if !cur_level >= !chapter then begin
        close_section (!chapter) ;
        close_chapter () ;
        close_toc ()
      end ;
      cur_level := sn
    end else if sn = !chapter then begin
      if !cur_level < sn then begin
        open_toc () ;
      end else begin
        close_section !chapter ;
        close_chapter  ()
      end ;
      open_chapter name
    end else if sn <= !chapter + !depth then begin (* sn > !chapter *)
      if !cur_level < !chapter then begin
        open_toc () ;
        open_chapter ""
      end ;
      close_section sn ;
      open_section sn name
    end ;
    main lexbuf}     
| "<!--CUT DEF" ' '+
    {let chapter = Section.value (String.uppercase (secname lexbuf)) in
    skip_blanks lexbuf;
    let depth = intarg lexbuf in
    skip_endcom lexbuf ;
    save_state chapter depth ;
    cur_level := Section.value "DOCUMENT" ;
    main lexbuf}
| "<!--SEC END" ' '* "-->" '\n'?
    {if !phase > 0 then begin
      if !tocbis && !out == !out_prefix then
        out := Out.create_buff ()
    end ;
    main lexbuf}
| "<!--CUT END" ' '* "-->" '\n'?
    {close_all () ;
      restore_state () ;
      main lexbuf}
| "<!--BEGIN" ' '+ "NOTES" ' '+
    {let sec_notes = secname lexbuf in
    skip_endcom lexbuf ;
    open_notes (Section.value sec_notes) ;     
    main lexbuf}
| "<!--END" ' '+ "NOTES" ' '* "-->" '\n'?
    {if !otheroutname <> "" then
      close_notes ();
      main lexbuf}
| "<!--" ' '* "FRENCH" ' '* "-->"
    {language := "fra" ;
      main lexbuf}
| "<A" ' '+
    {if !phase > 0 then put (lexeme lexbuf) ;
    aargs lexbuf}
| "<!--HTML" ' '* "HEAD" ' '* "-->" '\n' ?
    {let head = save_html lexbuf in
    if !phase = 0 then
      html_head := head
    else
      Out.put !out head;
    main lexbuf}
| "<!--HTML" ' '* "FOOT" ' '* "-->" '\n' ?
    {let foot =  save_html lexbuf in
    if !phase = 0 then
      html_foot := foot ;
    main lexbuf}
| "<!--FOOTER-->" '\n'?
    {close_all () ;
      if !phase > 0 then begin
        Out.put !out !html_foot
      end ;
      footer lexbuf}
| "<!DOCTYPE"  [^'>']* '>'
    {let lxm = lexeme lexbuf in
    if !phase = 0 then
      doctype := lxm
    else
      Out.put !out lxm;
    main lexbuf}
| "<HTML"  [^'>']* '>'
    {let lxm = lexeme lexbuf in
    if !phase = 0 then
      html := lxm
    else
      Out.put !out lxm;
    main lexbuf}
| "<BODY" [^'>']* '>'
    {let lxm = lexeme lexbuf in
    if !phase = 0 then
      body := lxm
    else
      Out.put !out lxm;
    main lexbuf}
| "<HEAD" [^'>']* '>'
    {put (lexeme lexbuf);
      if !phase = 0 then begin
        if !verbose > 0 then prerr_endline "Collect header" ;
        collect_header lexbuf
      end else
        main lexbuf}
| "</BODY>" _*
    {let lxm = lexeme lexbuf in
    close_all () ;
    if !phase > 0 then begin
      close_top lxm
    end}
|  _
    {let lxm = lexeme_char lexbuf 0 in
    if !phase > 0 then put_char lxm ;
    main lexbuf}
| eof
    {raise (Error ("No </BODY> tag in input file"))}

and save_html = parse
| "<!--END" ' '* ['A'-'Z']+ ' '* "-->" '\n'?
    {let s = Out.to_string html_buff in
    if !verbose > 0 then
      prerr_endline ("save_html -> ``"^s^"''");
    s}
|  _
    {let lxm = lexeme_char lexbuf 0 in    
    Out.put_char html_buff lxm ;
    save_html lexbuf}
| eof
    {raise (Misc.Fatal ("End of file in save_html"))}

and collect_header = parse
| "</HEAD>"
    {let lxm = lexeme lexbuf in
    finalize_header () ;
    if !verbose > 0 then begin
      prerr_string "Header is: ``" ;
      prerr_string !common_headers ;
      prerr_endline "''"
    end ;
    main lexbuf}

| "<TITLE" [^'>']* '>'
    {skip_title lexbuf ;
      collect_header lexbuf}
| _
    {let lxm = lexeme_char lexbuf 0 in
    adjoin_to_header_char lxm;
    collect_header lexbuf}

and skip_title = parse
|  "</TITLE>" '\n'? {()}
|  _          {skip_title lexbuf}
    
and footer = parse
    "</BODY>" _*
    {let lxm = lexeme lexbuf in
    if !phase > 0 then begin
      close_top lxm 
    end}
| _   {footer lexbuf}
| eof {raise (Misc.Fatal ("End of file in footer (no </BODY> tag)"))}

and secname = parse
    ['a'-'z' 'A'-'Z']+
    {let r = lexeme lexbuf in r}
| "" {raise (Error "Bad section name syntax")}

and intarg = parse
    ['0'-'9']+ {int_of_string (lexeme lexbuf)}
| ""         {!depth}

and tocline = parse
    "-->" '\n' ? {Out.to_string toc_buf}
| _
    {Out.put_char toc_buf (lexeme_char lexbuf 0) ;
      tocline lexbuf}

and aargs = parse
| ("name"|"NAME") ' '* '=' ' '*
  {if !phase = 0 then begin
    let name = refname lexbuf in
    Cross.add name !outname
  end else
    put (lexeme lexbuf) ;
  aargs lexbuf}
| ("href"|"HREF") ' '* '=' ' '*
   {if !phase > 0 then begin
      let lxm = lexeme lexbuf in
      let name = refname lexbuf in
      try
        let newname =
          if String.length name > 0 && String.get name 0 = '#' then
            Cross.fullname !outname (String.sub name 1 (String.length name-1))
          else name in
        put lxm ;
        put "\"" ;
        put newname ;
        put "\""
      with Not_found -> ()
    end ;
    aargs lexbuf}
| '>'
  {if !phase > 0 then put_char '>' ;
  main lexbuf}
| _
  {if !phase > 0 then put_char (lexeme_char lexbuf 0) ;
  aargs lexbuf}
| eof
  {raise (Error "Bad <A ...> tag")}

and refname = parse
|  '"' [^'"']* '"'
   {let lxm = lexeme lexbuf in
   String.sub lxm 1 (String.length lxm-2)}
| ['a'-'z''A'-'Z''0'-'9']+
   {lexeme lexbuf}
| "" {raise (Error "Bad reference name syntax")}

and skip_blanks = parse
  ' '* {()}

and skip_endcom  = parse
  ' '* "-->" '\n'? {()}
| ""               {raise (Error "Bad HTML comment syntax")}
and skip_aref = parse
  "</A>" {()}
| _      {skip_aref lexbuf}  

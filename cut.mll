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
open Stack
let header = "$Id: cut.mll,v 1.40 2003-02-13 14:51:00 maranget Exp $" 

let verbose = ref 0

let language = ref "eng"

type toc_style = Normal | Both | Special

let toc_style = ref Normal

let cross_links = ref true
and some_links = ref false

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
and html_prefix = ref ""

let phase = ref (-1)
;;

let name = ref "main"
and count = ref 0
;;

let body = ref "<BODY>"
and doctype = ref ""
and html = ref "<HTML>"
;;

let changed_t = Hashtbl.create 17

let rec check_changed name =
  try
    let r = Hashtbl.find changed_t name in
    check_changed r
  with
  | Not_found -> name

let new_filename s =  
  incr count ;
  let r1 = Printf.sprintf "%s%0.3d.html" !name !count in
  let r2 = check_changed r1 in
  r2
;;

let out = ref (Out.create_null ())
and out_prefix = ref (Out.create_null ())
and outname = ref ""
and lastclosed = ref ""
and otheroutname = ref ""
and flowname_stack = (Stack.create "flowname" : string Stack.t)
and flow_stack = (Stack.create "flow" : Out.t Stack.t)
;;

let toc = ref !out
and tocname = ref !outname
and otherout = ref !out
;;

let close_loc ctx name out =  Out.close out

let change_name oldname name =
  if !verbose > 0 then
    prerr_endline ("Change "^oldname^" into "^name) ;
  if !phase <= 0 then begin
    Thread.change oldname name ;
    Cross.change oldname name ;
    Hashtbl.add changed_t oldname name ;
    outname := name
  end

    

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

let link_buff = Out.create_buff ()

let putlinks  name =
  let links_there = ref false in
  if !verbose > 0 then
    prerr_endline ("putlinks: "^name) ;
  begin try
    putlink link_buff (Thread.prev name) "previous_motif.gif" 
      (if !language = "fra" then "Précédent"
      else "Previous") ;
    links_there := true
  with Not_found ->
    if !verbose > 0 then
      prerr_endline ("No prev link for "^name)
  end ;
  begin try
    putlink link_buff (Thread.up name) "contents_motif.gif" 
      (if !language = "fra" then "Remonter"
      else "Up") ;
    links_there := true
  with Not_found -> () end ;
  begin try
    putlink link_buff (Thread.next name) "next_motif.gif" 
      (if !language = "fra" then "Suivant"
      else "Next") ;
    links_there := true
  with Not_found -> () end ;
  if !links_there then
    Some (Out.to_string link_buff)
  else
    None

let putlinks_start out outname =
  if !cross_links then
    match putlinks outname with
    | Some s -> 
        some_links := true ;
        Out.put out s ;
        Out.put out "<HR>\n"
    | None -> ()

let putlinks_end out outname =
  if !cross_links then
    match putlinks outname with
    | Some s -> 
        some_links := true ;
        Out.put out "<HR>\n" ;
        Out.put out s
    | None -> ()
  

let openhtml withlinks title out outname =
  Out.put out !doctype ; Out.put_char out '\n' ;
  Out.put out !html ; Out.put_char out '\n' ;
  Out.put out "<HEAD>\n" ;
  Out.put out !common_headers;
  Out.put out "<TITLE>\n" ;
  let title = Save.tagout (Lexing.from_string (!html_prefix^title)) in
  Out.put out title ;
  Out.put out "\n</TITLE>\n" ;
  Out.put out "</HEAD>\n" ;
  Out.put out !body;
  Out.put out "\n" ;
  if withlinks then putlinks_start out outname ;
  Out.put out !html_head


and closehtml withlinks name out =
  Out.put out !html_foot ;
  if withlinks then begin
    putlinks_end out name
  end ;
  Out.put out "</BODY>\n" ;
  Out.put out "</HTML>\n" ;
  close_loc "closehtml" name out
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
    begin match !toc_style with
    | Both|Special -> openlist !out_prefix
    | _ -> ()
    end ;
    do_open (l1+1) l2
  end
;;

(* close from l1 down to l2 *)
let rec do_close l1 l2 =
  if l1 > l2 then begin
     closelist !toc ;
     begin match !toc_style with
     | Both|Special -> closelist !out_prefix
     | _  -> ()
     end ;
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
    begin match !toc_style with
    | Normal ->
        itemanchor !outname label name !toc ;
    | Both ->
        itemanchor !outname label name !toc ;
        itemanchor !outname label name !out_prefix
    | Special    ->
        itemanchor !outname label name !out_prefix
    end ;
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
    closehtml true !outname !out ;
    begin match !toc_style with
    | Both|Special ->
      let real_out = open_out !outname in
      Out.to_chan real_out !out_prefix ;
      Out.to_chan real_out !out ;
      close_out real_out
    | Normal -> ()
    end ;
    out := !toc
  end else begin
    lastclosed := !outname ;
    outname := !tocname
  end

and open_chapter name =
  outname := new_filename ("open_chapter <<"^name^">>") ;
  if !verbose > 0 then
    prerr_endline
      ("Open chapter out="^ !outname^" toc="^ !tocname^
       " cur_level="^string_of_int !cur_level) ;
  if !phase > 0 then begin
    begin match !toc_style with
    | Both|Special ->
        out_prefix := Out.create_buff () ;
        out := !out_prefix ;
        openhtml true name !out_prefix !outname
    | Normal ->
        out := Out.create_chan (open_out !outname) ;
        openhtml true name !out !outname
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
let setlink set target =
  if !phase = 0 && target <> "" then
    set !outname target

let open_notes sec_notes =
  if sec_notes <> !chapter || !outname = !tocname then begin
    otheroutname := !outname ;
    outname := new_filename "open_notes" ;
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
     close_loc "notes" !outname !out ;
     outname := !otheroutname ;
     out := !otherout ;
     otheroutname := ""
  end
;;

let toc_buf = Out.create_buff ()
and arg_buf = Out.create_buff ()
;;

let stack = Stack.create "main"
;;

let save_state newchapter newdepth =
  if !verbose > 0 then
    prerr_endline ("New state: "^string_of_int newchapter) ;
  push stack
    (!outname, Stack.save flowname_stack, Stack.save flow_stack,
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
    oldoutname, oldflowname, oldflow,
    oldchapter,olddepth,oldtoc,oldtocname,
    oldlevel,oldlastclosed,oldprefix  = pop stack in
  outname := oldoutname ;
  Stack.restore flowname_stack oldflowname ;
  Stack.restore flow_stack oldflow ;
  chapter := oldchapter ;
  depth := olddepth ;
  toc := oldtoc ;
  tocname := oldtocname ;
  lastclosed := !lastclosed ;
  cur_level := oldlevel ;
  out_prefix := oldprefix
;;

let hevea_footer = ref false

let close_top lxm =
  Out.put !toc !html_foot ;
  putlinks_end !toc !tocname ;
  if !hevea_footer then begin
    Out.put !out "<!--FOOTER-->\n" ;
    begin try
      Mysys.put_from_file
        (Filename.concat Mylib.libdir ("cutfoot-"^ !language^".html"))
        (Out.put !out)
    with Mysys.Error s -> begin
      Location.print_pos () ;
      prerr_endline s
    end
    end
  end ;
  Out.put !toc lxm ;
  if !tocname = "" then
    Out.flush !toc
  else
   close_loc "top" !tocname !toc
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

let openflow title =
  let new_outname = new_filename "openflow" in
  push flowname_stack !outname ;
  outname := new_outname ;
  if !phase > 0 then begin
    push flow_stack !out ;
    out := Out.create_chan (open_out !outname) ;
    openhtml false title !out !outname
  end

and closeflow () =
  if !phase > 0 then begin
    closehtml false !outname !out;
    out := pop flow_stack
  end ;
  outname := pop flowname_stack


} 

let secname = ['a'-'z' 'A'-'Z']+

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
|  "<!--" "FLOW" ' '+
   {let title = flowline lexbuf in
   openflow title ;
   main lexbuf}
| "<!--" "LINKS" ' '+
   {let links = getargs lexbuf in
   List.iter
     (fun (name,v) -> match name with
     | "UP" -> setlink Thread.setup v
     | "PREV" -> setlink Thread.setprev v
     | "NEXT" -> setlink Thread.setnext v
     | _ -> ())
     links ;
   main lexbuf}
| "<!--" "PREFIX" ' '+
   {let l = getargs lexbuf in   
   if !phase = 0 then begin
     match l with
     | [] -> ()
     | (_,v)::_ -> html_prefix := v
   end ;
   main lexbuf}
| "<!--" "END" ' '+ "FLOW" ' '* "-->" '\n'?
   {closeflow () ;
   main lexbuf}
| "<!--" "NAME" ' '+
    {let name = tocline lexbuf in
    change_name !outname name ;
    main lexbuf} 
|  "<!--" ("TOC"|"toc") ' '+ (secname as arg) ' '+
    {let sn = 
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
| "<!--CUT DEF" ' '+ (secname as name) ' '* (['0'-'9']+ as i_opt)?
    {let chapter = Section.value (String.uppercase name) in
    let depth = match i_opt with
    | None -> !depth 
    | Some s -> int_of_string s in
    skip_endcom lexbuf ;
    save_state chapter depth ;
    cur_level := Section.value "DOCUMENT" ;
    main lexbuf}
| "<!--SEC END" ' '* "-->" '\n'?
    {if !phase > 0 then begin
      match !toc_style with
      | Both|Special when !out == !out_prefix ->
          out := Out.create_buff ()
      | _ -> ()
    end ;
    main lexbuf}
| "<!--CUT END" ' '* "-->" '\n'?
    {close_all () ;
      restore_state () ;
      main lexbuf}
| "<!--BEGIN" ' '+ "NOTES" ' '+ (secname as sec_notes)
    {skip_endcom lexbuf ;
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
    if !phase = 0 then begin
      html_foot := foot
    end ;
    main lexbuf}
| "<!--FOOTER-->" '\n'?
    {if !phase = 0 then hevea_footer := true ;
     close_all () ;
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
    else begin
      Out.put !out lxm ;
      putlinks_start !out !outname
    end ;
    main lexbuf}
| "<HEAD" [^'>']* '>'
    {put (lexeme lexbuf);
      if !phase = 0 then begin
        if !verbose > 0 then prerr_endline "Collect header" ;
        collect_header lexbuf
      end else
        main lexbuf}
| "</BODY>" _ * 
    {let lxm = lexeme lexbuf in
    close_all () ;
    if !phase > 0 then begin
      close_top lxm
    end}
|  _ as lxm
    {if !phase > 0 then put_char lxm ;
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
| _ as lxm
    {adjoin_to_header_char lxm;
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


and tocline = parse
    "-->" '\n' ? {Out.to_string toc_buf}
| _
    {Out.put_char toc_buf (lexeme_char lexbuf 0) ;
      tocline lexbuf}


and flowline = parse
| "<ARG TITLE>"
    {let title = arg lexbuf in
    let _ = flowline lexbuf in
    title}
| "-->" '\n'?
    {""}
| eof {raise (Misc.Fatal "Unclosed comment")}
| _   {flowline lexbuf}

and getargs = parse
| "-->" '\n'? {[]}
| "<ARG" ' '* 
    {let name = argname lexbuf in
    let r = arg lexbuf in
    (name,r)::getargs lexbuf}
| eof {raise (Misc.Fatal "Unclosed comment")}
| _   {getargs lexbuf}

and argname = parse
| ['a'-'z''A'-'Z']* '>'
    {let lxm = lexeme lexbuf in
    String.sub lxm 0 (String.length lxm-1)}
| "" {raise (Misc.Fatal "ARG title")}

and arg = parse
| "</ARG>"  {Out.to_string arg_buf}
| _         {Out.put_char arg_buf (Lexing.lexeme_char lexbuf 0) ; arg lexbuf}
| eof       {raise (Misc.Fatal "Unclosed arg")}


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
| ['a'-'z''A'-'Z''0'-'9''.''_''-']+
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

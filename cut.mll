{
open Lexing

let verbose = ref 0
;;

let push s e = s := e:: !s
and pop s = match !s with
  [] -> failwith "Empty stack"
| e::rs -> s := rs ; e
;;

let phase = ref 0
;;

let name = ref "main"
and count = ref 0
;;

let body = ref "<BODY>"
;;

let new_filename () =
  incr count ;
  Printf.sprintf "%s%0.3d.html" !name !count
;;

let out = ref (Out.create_null ())
and outname = ref ""
and lastclosed = ref ""
and otheroutname = ref ""
;;

let toc = ref !out
and tocname = ref !outname
and otherout = ref !out
;;

let start_phase name =
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
  begin try
    putlink out (Thread.prev name) "previous_motif.gif" "Previous"
  with Not_found -> () end ;
  begin try
    putlink out (Thread.next name) "next_motif.gif" "Next"
  with Not_found -> () end ;
  begin try
    putlink out (Thread.up name) "contents_motif.gif" "Contents"
  with Not_found -> () end

;;

let openhtml title out outname =
  Out.put out "<HTML>\n" ;
  Out.put out "<HEAD>\n" ;
  Out.put out "<TITLE>\n" ;
  let title = Save.tagout (Lexing.from_string title) in
  Out.put out title ;
  Out.put out "\n</TITLE>\n" ;
  Out.put out "</HEAD>\n" ;
  Out.put out !body;
  Out.put out "\n" ;
  putlinks out outname ;
  Out.put out "<HR>\n"

and closehtml name out =
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
;;

let cur_level = ref (Section.value "DOCUMENT")
and chapter = ref (Section.value "CHAPTER")
and depth = ref 2
;;


(* Open all lists in toc from chapter to sec, with sec > chapter *)
let rec do_open l1 l2 =
  if l1 < l2 then begin
    openlist !toc ;
    do_open (l1+1) l2
  end
;;

(* close from l1 down to l2 *)
let rec do_close l1 l2 =
  if l1 > l2 then begin
     closelist !toc ;
     do_close (l1-1) l2
  end else
  cur_level := l1
;;

let anchor = ref 0
;;

let open_section sec name =
  if !cur_level > sec then do_close !cur_level sec
  else if !cur_level < sec then do_open  !cur_level sec ;
  incr anchor ;
  let label = "toc"^string_of_int !anchor in
  itemanchor !outname label name !toc ;
  putanchor label !out ;
  cur_level := sec

and close_section sec =  do_close !cur_level sec
;;

let close_chapter0 () =
  lastclosed := !outname ;
  outname := !tocname

and open_chapter0 name =
  outname := new_filename () ;
  Thread.setup !outname !tocname ;
  Thread.setprevnext !lastclosed !outname
;;

let close_chapter () =
  if !verbose > 0 then
    prerr_endline ("Close chapter out="^ !outname^" toc="^ !tocname) ;
  closehtml !outname !out ;
  out := !toc ;
  close_chapter0 ()

and open_chapter name =
  outname := new_filename () ;
  if !verbose > 0 then
    prerr_endline ("Open chapter out="^ !outname^" toc="^ !tocname) ;
  itemref !outname name !toc ;
  out := Out.create_chan (open_out !outname) ;
  openhtml name !out !outname ;
  cur_level := !chapter
;;

let open_notes sec_notes =
  if sec_notes <> !chapter || !outname = !tocname then begin
    otheroutname := !outname ;
    outname := new_filename () ;
    if !phase > 0 then begin
      otherout := !out ;
      out := Out.create_chan (open_out !outname) ;
      Out.put !out "<HTML><HEAD><TITLE>Notes</TITLE></HEAD>\n" ;
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
  push stack (!chapter,!depth,!toc,!tocname,!cur_level,!lastclosed) ;
  chapter := newchapter ;
  depth := newdepth ;
  tocname := !outname ;
  lastclosed := "" ;
  toc := !out
;;

let restore_state () =
  if !verbose > 0 then prerr_endline ("Restore") ;
  let
    oldchapter,olddepth,oldtoc,oldtocname,
    oldlevel,oldlastclosed  = pop stack in
  chapter := oldchapter ;
  depth := olddepth ;
  toc := oldtoc ;
  tocname := oldtocname ;
  lastclosed := !lastclosed ;
  cur_level := oldlevel
;;

let language = ref "eng"
;;

let close_top lxm =
  closelist !toc ;
  Out.put !out "<!--FOOTER-->\n" ;
  Mylib.put_from_lib ("cutfoot-"^ !language^".html") (Out.put !out) ;
  Out.put !toc lxm ;
  if !tocname = "" then
    Out.flush !toc
  else
   Out.close !toc
;;

let open_toc () = openlist !toc
and close_toc () = closelist !toc
;;

let close_all () =
  if !phase > 0 then begin
    if !cur_level > !chapter then begin
      close_section !chapter ;
      close_chapter () ;
      close_toc ()
    end else if !cur_level = !chapter then begin
      close_chapter () ;
      close_toc ()
    end
  end else begin
    if !cur_level <= !chapter then
      close_chapter0 ();
  end ;
  cur_level := (Section.value "DOCUMENT")
}

rule main = parse
  "<!--" ("TOC"|"toc") ' '
  {let arg = secname lexbuf in
  let sn = 
    if String.uppercase arg = "NOW" then !chapter
    else Section.value arg in
  let name = tocline lexbuf in
  if !phase > 0 then begin
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
    end
  end else begin (* !phase = 0 *)
    if sn < !chapter then begin
      if !cur_level >= !chapter then
        close_chapter0 ();
      cur_level := sn;
    end else if sn = !chapter then begin
      if !cur_level >= !chapter then
        close_chapter0 () ;
      open_chapter0 name ;
      cur_level := sn
    end else if sn <=  !chapter + !depth then begin
       if !cur_level < !chapter then
         open_chapter0 "";
       cur_level :=  sn
     end ;
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
| "<!--CUT END" ' '* "-->" '\n'?
   {close_all () ;
   restore_state () ;
   main lexbuf}
| "<!--BEGIN" ' '+ "NOTES "
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
| "<A" ' '* ("name"|"NAME") ' '* '=' ' '*
  {if !phase = 0 then begin
     let name = refname lexbuf in
     Cross.add name !outname
  end else put (lexeme lexbuf) ;
  main lexbuf}
| "<A" ' '* ("HREF"|"href") ' '* '=' ' '*
   {if !phase > 0 then begin
     let lxm = lexeme lexbuf in
     let name = refname lexbuf in
     try
       let newname =
         if String.length name > 0 && String.get name 0 = '#' then
            Cross.fullname (String.sub name 1 (String.length name-1))
         else name in
       put lxm ;
       put "\"" ;
       put newname ;
       put "\""
     with Not_found -> skip_aref lexbuf
   end ;
   main lexbuf}
| "<!--FOOTER-->" '\n'?
   {let lxm = lexeme lexbuf in
   close_all () ;
   if !phase > 0 then Out.put !out lxm ;
   footer lexbuf}
| "<BODY" [^'>']* '>'
   {let lxm = lexeme lexbuf in
   if !phase = 0 then
     body := lxm
   else
     Out.put !out lxm;
   main lexbuf}
| "</BODY>" _*
   {let lxm = lexeme lexbuf in
   close_all () ;
   if !phase > 0 then begin
     close_top lxm
   end}
|  _
   {let lxm = lexeme lexbuf in
   if !phase > 0 then put lxm ;
   main lexbuf}
| eof
   {failwith "EOF"}

and footer = parse
  "</BODY>" _*
  {let lxm = lexeme lexbuf in
  if !phase > 0 then begin
     close_top lxm 
  end}
| _
   {let lxm = lexeme lexbuf in
   if !phase > 0 then put lxm ;
   footer lexbuf}

and secname = parse
  ['a'-'z' 'A'-'Z']+
    {let r = lexeme lexbuf in r}
| "" {failwith "secname"}

and intarg = parse
  ['0'-'9']+ {int_of_string (lexeme lexbuf)}
| ""         {!depth}

and tocline = parse
  "-->" '\n' ? {Out.to_string toc_buf}
| _
    {Out.put_char toc_buf (lexeme_char lexbuf 0) ;
    tocline lexbuf}

and refname = parse
  '"' [^'"']* '"'
   {let lxm = lexeme lexbuf in
   String.sub lxm 1 (String.length lxm-2)}
| ['a'-'z' '.' 'A'-'Z' '0'-'9']+
   {lexeme lexbuf}
| "" {failwith "refname"}

and skip_blanks = parse
  ' '* {()}

and skip_endcom  = parse
  ' '* "-->" '\n'? {()}
| ""               {failwith "endcom"}
and skip_aref = parse
  "</A>" {()}
| _      {skip_aref lexbuf}  

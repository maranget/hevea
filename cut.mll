{
open Lexing

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
and itemlistref filename s out =
  Out.put out "<LI>" ;
  Out.put out "<A HREF=\"" ;
  Out.put out filename ;
  Out.put out "\">" ;
  Out.put out s ;
  Out.put out "</A>\n"

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
  Out.put out title ;
  Out.put out "\n</TITLE>\n" ;
  Out.put out "</HEAD>\n" ;
  Out.put out "<BODY>\n" ;
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

let in_part = ref false
and in_chapter = ref false
;;

let chapter = ref "CHAPTER"
and part = ref "PART"
;;

let close_chapter () =
(*  prerr_endline ("Close chapter out="^ !outname^" toc="^ !tocname) ; *)
  if !in_chapter then begin
    in_chapter := false ;
    if !phase > 0 then closehtml !outname !out
    else lastclosed := !outname ;
    outname := !tocname ;
    out := !toc
  end else
   if !phase > 0 then openlist !toc

and open_chapter name =
  outname := new_filename () ;
  if !phase > 0 then begin
    itemlistref !outname name !toc ;
     out := Out.create_chan (open_out !outname)
  end ;
  in_chapter := true;
  if !phase > 0 then openhtml name !out !outname
  else begin
    Thread.setup !outname !tocname ;
    Thread.setprevnext !lastclosed !outname
  end
;;

let close_part () =
  if !phase > 0 then closelist !toc ;
  in_part := false;
and open_part () =
  in_part := true
;;

let open_notes sec_notes =
  if String.uppercase sec_notes <> !chapter || !outname = !tocname then begin
    otheroutname := !outname ;
    outname := new_filename () ;
    if !phase > 0 then begin
      otherout := !out ;
      out := Out.create_chan (open_out !outname) ;
      Out.put !out "<HTML><HEAD><TITLE>Notes</TITLE></HEAD>\n" ;
      Out.put !out "<BODY>\n"
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

let save_state newpart newchapter =
(*  prerr_endline ("New state: "^newpart^" "^newchapter) ; *)
  push stack (!part,!chapter,!toc,!tocname,!in_part,!in_chapter,!lastclosed) ;
  part := newpart ;
  chapter := newchapter ;
  tocname := !outname ;
  lastclosed := "" ;
  toc := !out ;
  in_part := false ;
  in_chapter := false
;;

let restore_state () =
(*  prerr_endline ("Restore") ; *)
  let
    oldpart,oldchapter,oldtoc,oldtocname,
    oldinpart,oldinchapter,oldlastclosed  = pop stack in
  chapter := oldchapter ;
  part := oldpart ;
  toc := oldtoc ;
  tocname := oldtocname ;
  lastclosed := !lastclosed ;
  in_part := oldinpart ;
  in_chapter := oldinchapter
;;

let close_top lxm =
  closelist !toc ;
  Out.put !out "<!--FOOTER-->\n" ;
  Mylib.put_from_lib "footer.bis.html" (Out.put !out) ;
  Out.put !out "\n</BODY></HTML>\n" ;
  Out.put !toc lxm ;
  if !tocname = "" then
    Out.flush !toc
  else
   Out.close !toc
;;
}

rule main = parse
  "<!--" ("TOC"|"toc") ' '
  {let sn = String.uppercase (secname lexbuf) in
  let name = tocline lexbuf in
  if sn = !part then begin
      close_chapter () ;
      close_part () ;
      open_part ()
  end else if sn = !chapter then begin
      close_chapter () ;
      open_chapter name
  end ;
  main lexbuf      
  }
| "<!--CUT DEF" ' '+
  {let part = String.uppercase (secname lexbuf) in
  skip_blanks lexbuf ;
  let chapter = String.uppercase (secname lexbuf) in
  skip_endcom lexbuf ;
  save_state part chapter ;
  main lexbuf}
| "<!--CUT END" ' '* "-->" '\n'?
   {close_chapter () ;
   close_part () ;
   restore_state () ;
   main lexbuf}
| "<!--BEGIN" ' '+ "NOTES "
   {let sec_notes = secname lexbuf in
   skip_endcom lexbuf ;
   open_notes sec_notes ;     
   main lexbuf}
| "<!--END" ' '+ "NOTES" ' '* "-->" '\n'?
   {if !otheroutname <> "" then
     close_notes ();
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
   close_chapter () ;
   close_part () ;
   if !phase > 0 then Out.put !out lxm ;
   footer lexbuf}
| "</BODY>" _*
   {let lxm = lexeme lexbuf in
   close_chapter () ;
   close_part () ;
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

and skip_blanks = parse
  ' '* {()}

and skip_endcom  = parse
  ' '* "-->" '\n'? {()}

and skip_aref = parse
  "</A>" {()}
| _      {skip_aref lexbuf}  

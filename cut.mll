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

let out = ref (Out.create_chan stdout)
and outname = ref ""
;;

let toc = ref !out
and tocname = ref !outname
;;

let start_phase () =
  outname := "" ;
  tocname := "" ;
  count := 0
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

let openhtml title out =
  Out.put out "<HTML>\n" ;
  Out.put out "<HEAD>\n" ;
  Out.put out "<TITLE>\n" ;
  Out.put out title ;
  Out.put out "\n</TITLE>\n</HEAD>\n" ;
  Out.put out "<BODY>\n" ;
  Out.put out "<HR>\n"

and closehtml name out =
  Out.put out "</BODY>\n" ;
  Out.put out "</HTML>\n" ;
  prerr_endline ("Closing: "^name) ;
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
and part = ref "PART
;;

let close_chapter () =
  prerr_endline ("Close chapter out="^ !outname^" toc="^ !tocname) ;
  if !in_chapter then begin
    in_chapter := false ;
    if !phase > 0 then closehtml !outname !out ;
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
  if !phase > 0 then openhtml name !out
;;

let close_part () =
  if !phase > 0 then closelist !toc ;
  in_part := false;
and open_part () =
  in_part := true ;
  if !phase > 0 then Out.put !out "<HR>\n"
;;

let toc_buf = Out.create_buff ()
;;

}

rule main = parse
  "<!--" ("TOC"|"toc") ' '*
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
| "<!-->
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
     let newname =
       if String.length name > 0 && String.get name 0 = '#' then
          Cross.fullname (String.sub name 1 (String.length name-1))
       else name in
     put lxm ;
     put "\"" ;
     put newname ;
     put "\""
   end ;
   main lexbuf}
| "</BODY>" _*
   {let lxm = lexeme lexbuf in
   close_chapter () ;
   close_part () ;
   if !phase > 0 then begin
     closelist !toc ;
     Out.put !toc lxm ;
     if !tocname = "" then
       Out.flush !toc
     else
       Out.close !toc
    end}
|  _
   {let lxm = lexeme lexbuf in
   if !phase > 0 then put lxm ;
   main lexbuf}
| eof
   {failwith "EOF"}

and secname = parse
  ['a'-'z' 'A'-'Z']+ ' '
    {let r = lexeme lexbuf in
    String.sub r 0 (String.length r-1)}

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


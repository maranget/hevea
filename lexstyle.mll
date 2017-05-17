(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Extract/abstract style attributes *)

{
open Printf
open Emisc

let error msg _lb =  raise (Emisc.LexError msg)
}

let blank = [' ''\t''\n''\r']
let tag = ['a'-'z''A'-'Z''0'-'9']+
let class_name = ['a'-'z''A'-'Z''0'-'9''-']+
let attr_name = ['a'-'z''A'-'Z''0'-'9']['a'-'z''A'-'Z''-''0'-'9'':']* 

rule extract styles = parse
(* TOC comments are scanned *)
| "<!--TOC" blank+
   { extract styles lexbuf }
| "<!--"
  {skip_comment lexbuf ;
   extract styles lexbuf }
| "<!"| ("</" tag)
  { skip_tag lexbuf ;
    extract styles lexbuf ; }
| '<'  (tag as tag)
  { let styles = extract_tag tag styles lexbuf in
    extract styles lexbuf }
| [^'<']+ { extract styles lexbuf }
| eof { styles }
| "" { error "extract" lexbuf }

and extract_tag tag styles = parse
| '>' { styles }
| blank+ { extract_tag tag styles lexbuf }
| ("style"|"STYLE") blank* "=" blank*
  (('\'' ([^'\'']* as v) '\''
  | '"' ([^'"']* as v) '"'
  | ('#'?['a'-'z''A'-'Z''0'-'9''-''+''_'':''.']+ as v)))
(* '"' *)
  { extract_tag tag (StringCount.incr v styles) lexbuf }
| attr_name ( blank* "=" blank*
  ('\'' ([^'\'']*) '\''
  | '"' ([^'"']*) '"'
  | '#'?['a'-'z''A'-'Z''0'-'9''-''+''_'':''.']+))?
(* '"' *)
  {  extract_tag tag styles lexbuf }
| "" { error "extract_tag"lexbuf }

and skip_tag = parse
| [^'>']* '>' { () }
| "" { error "skip_tag" lexbuf }

and skip_comment = parse
| "-->" '\n'? { }
| _ 
   {skip_comment lexbuf}
| eof
   {error "End of file in comment" lexbuf}   
| "" { error "comment" lexbuf }

and dump m out = parse
| "<style" blank+ "type" blank* "=" blank* '"' "text/css" '"' blank* '>' '\n'?
  as lxm
   { fprintf out "%s" lxm ;
     Emisc.StringMap.iter
       (fun st cl -> Emisc.dump_class out cl st)
       m ;
     dump m out lexbuf }
| "<!--TOC" blank+ as lxm
   { output_string out lxm ;
     dump m out lexbuf }
| "<!--" as lxm
  {fprintf out "%s" lxm ;
   dump_comment out lexbuf ;
   dump m out lexbuf }
| "<!"| ("</" tag) as lxm
  { fprintf out "%s" lxm ;
    dump_tag out lexbuf ;
    dump m out lexbuf }
| '<'  tag as lxm
  { output_string out lxm ;
    abstract_tag [] [] m out lexbuf ;
    dump m out lexbuf }
| [^'<']+ as lxm 
   { output_string out lxm ; dump m out lexbuf }
| eof { true }
| "" { error "dump" lexbuf }
    
and dump_comment out = parse
| "-->" '\n'? as lxm { output_string out lxm }
| _ as c             { output_char out c ; dump_comment out lexbuf }
| eof                {error "End of file in comment" lexbuf}   
| "" { error "dump_comment" lexbuf }

and dump_tag out = parse
| [^'>']* '>' as lxm { output_string out lxm }
| ""                 { error "dump_tag" lexbuf }

and abstract_tag cl attrs m out = parse
| '>'
  {
   let cl = match cl with
   | [] -> []
   | _  -> [sprintf "class=\"%s\"" (String.concat " " (List.rev cl))] in
   let na = cl @ List.rev attrs in
   List.iter
     (fprintf out " %s")
     na ;
   output_char out '>'
  }
| blank+ {  abstract_tag cl attrs m out lexbuf }
| ("style"|"STYLE") blank* "=" blank*
  (('\'' ([^'\'']* as v) '\''
  | '"' ([^'"']* as v) '"'
  | ('#'?['a'-'z''A'-'Z''0'-'9''-''+''_'':''.']+ as v))) as a
(* '"' *)
  {
    try
      let tcl = Emisc.StringMap.find v m  in
      abstract_tag (tcl::cl) attrs m out lexbuf
    with Not_found ->
      abstract_tag cl (a::attrs) m out lexbuf
   }
| ("class"|"CLASS") blank* "=" blank*
  (('\'' ([^'\'']* as v) '\''
  | '"' ([^'"']* as v) '"'
  | ('#'?['a'-'z''A'-'Z''0'-'9''-''+''_'':''.']+ as v)))
(* '"' *)
  { abstract_tag (v::cl) attrs m out lexbuf }      
| attr_name ( blank* "=" blank*
  ('\'' ([^'\'']*) '\''
  | '"' ([^'"']*) '"'
  | '#'?['a'-'z''A'-'Z''0'-'9''-''+''_'':''.']+))? as a
(* '"' *)
  {  abstract_tag cl (a::attrs) m out lexbuf }
| "" { error "abstract_tag" lexbuf }


{
 let get lexbuf = extract StringCount.empty lexbuf

 let set m out lexbuf = dump m out lexbuf
}

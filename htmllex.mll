(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: htmllex.mll,v 1.7 2001-05-28 17:28:55 maranget Exp $          *)
(***********************************************************************)
{
open Lexing
open Lexeme
open Buff

let txt_level = ref 0
and txt_stack = Stack.create "htmllex"

exception Error of string
;;


let error msg lb = 
  raise (Error msg)


let init table (s,t)= Hashtbl.add table s t
;;

let block = Hashtbl.create 17
;;

List.iter (init block)
  ["CENTER", () ; "DIV", (); "BLOCKQUOTE", () ;
  "H1", () ; "H2", () ;"H3", () ;"H4", () ;"H5", () ;"H6", () ;
  "PRE", () ; "TABLE", () ; "TR",() ; "TD", () ; "TH",() ; 
  "OL",() ; "UL",(); "P",() ; "LI",() ;
  "DL",() ; "DT", () ; "DD",() ;
  ]
;;

let ptop () =
  if not (Stack.empty txt_stack) then begin
    let pos = Stack.top txt_stack in
    Location.print_this_fullpos pos ;
    prerr_endline "This opening tag is pending"
  end

let warnings = ref false

let check_nesting lb name =
  try
    Hashtbl.find block (String.uppercase name) ;
    if !txt_level <> 0 && !warnings then begin
      Location.print_fullpos () ;
      prerr_endline 
        ("Warning, block level element: "^name^" nested inside text-level element") ;
      ptop ()
    end
  with
  |  Not_found -> ()

let text = Hashtbl.create 17
;;


List.iter (init text)
  ["TT",TT ; "I",I ; "B",B ; "BIG",BIG ; "SMALL",SMALL ;
   "STRIKE",STRIKE ; "S",S ; "U",U ; "FONT",FONT ;
   "EM",EM ; "STRONG",STRONG ; "DFN",DFN ; "CODE",CODE ; "SAMP",SAMP ;
   "KBD",KBD ; "VAR",VAR ; "CITE",CITE ; "ABBR",ABBR ; "ACRONYM",ACRONYM ; 
   "Q",Q ; "SUB",SUB ; "SUP",SUP ; "A", A ; "SPAN", SPAN ; "SCRIPT", SCRIPT]
;;

let is_textlevel name =
  try
    let _ = Hashtbl.find text (String.uppercase name) in
    true
  with
  | Not_found -> false

let is_br name = "BR" = (String.uppercase name)


let get_value lb = function
  | Some s -> s
  | _ -> error "Bad attribute syntax" lb

let norm_attrs lb attrs =
   List.map
        (fun (name,value,txt) ->
          match String.uppercase name with
          | "SIZE" ->  SIZE (get_value lb value),txt
          | "COLOR" -> COLOR (get_value lb value),txt
          | "FACE" -> FACE (get_value lb value),txt
          | _      -> OTHER, txt)
    attrs

let print_attrs s attrs =
  print_string s ; print_string ":" ;
  List.iter
    (fun x -> match  x with
    | name,Some value when name=s ->
        print_char ' ' ;
        print_string value
    | _ -> ())
    attrs ;
  print_char '\n'

let ouvre lb name attrs txt =
  let uname = String.uppercase name in
  try
    let tag = Hashtbl.find text uname in
    let attrs = norm_attrs lb attrs in
    incr txt_level ;
    Stack.push txt_stack (Location.get_pos ()) ;
    Open (tag, attrs,txt)
  with
  | Not_found -> assert false

and ferme lb name txt =
  try
    let tag = Hashtbl.find text (String.uppercase name) in
    decr txt_level ;
    begin if not (Stack.empty txt_stack) then
      let _  = Stack.pop txt_stack in ()
    end ;
    Close (tag,txt)
  with
  | Not_found -> Text txt
  
              
  


let unquote s =
  let l = String.length s in
  String.sub s 1 (l-2)
;;

let buff = Buff.create ()
and abuff = Buff.create ()

let put s = Buff.put buff s
and putc c = Buff.put_char buff c

let aput s = Buff.put abuff s
and aputc c = Buff.put_char abuff c



}
 

let blank = [' ''\t''\n''\r']

rule main = parse
| (blank|"&nbsp;")+ {Blanks (lexeme lexbuf)}
| "<!--"
  {put (lexeme lexbuf) ;
  in_comment lexbuf ;
  Text (Buff.to_string buff)}
|   "<!"
  {put (lexeme lexbuf) ;
  in_tag lexbuf ;
  Text (Buff.to_string buff)}
| '<' 
    {putc '<' ;
    let tag = read_tag lexbuf in
    if is_textlevel tag then begin
      let attrs = read_attrs lexbuf in    
      ouvre lexbuf tag attrs (Buff.to_string buff)
    end else begin
      check_nesting lexbuf tag ;
      in_tag lexbuf ;
      let txt = Buff.to_string buff in
      if is_br tag then 
        Blanks txt
      else
        Text txt
    end}
|  "</" 
    {put "</" ;
    let tag = read_tag lexbuf in    
    in_tag lexbuf ;
    ferme lexbuf tag (Buff.to_string buff)}
|  eof {Eof}
| _
    {putc (lexeme_char lexbuf 0) ;
    text lexbuf ;
    Text (Buff.to_string buff)}

and text = parse
| [^'<']
  {putc (lexeme_char lexbuf 0) ; text lexbuf}
| "" {()}

and read_tag = parse
| ['a'-'z''A'-'Z''0'-'9']*
    {let lxm = lexeme lexbuf in
    put lxm ; lxm}

and read_attrs = parse
| blank+
    {aput (lexeme lexbuf) ; read_attrs lexbuf}
| ['a'-'z''A'-'Z''-''0'-'9']+
  {let name = lexeme lexbuf in
  aput name ;
  let v = read_avalue lexbuf in
  let atxt = Buff.to_string abuff in
  put atxt ;
  (name,v,atxt)::read_attrs lexbuf}
| '>' {put_char buff '>' ; []}
| ""  {error "Attribute syntax" lexbuf}

and read_avalue = parse
| blank* '=' blank*
    {let lxm = lexeme lexbuf in
    aput lxm ;
    Some (read_aavalue lexbuf)}
| "" {None}

and read_aavalue = parse
| '\''[^'\'']*'\''
| '"'[^'"']*'"'
    {let lxm = lexeme lexbuf in
    aput lxm ;
    unquote lxm}
| '#'?['a'-'z''A'-'Z''0'-'9''-''+''_'':''.']+
    {let lxm = lexeme lexbuf in
    aput lxm ;
    lxm}
| "" {error "Attribute syntax" lexbuf}

and in_tag = parse
| '>' {putc (lexeme_char lexbuf 0)}
| _   {putc (lexeme_char lexbuf 0) ; in_tag lexbuf}
| eof {error "End of file in tag" lexbuf}

and in_comment = parse
| "-->" '\n'?
  {put (lexeme lexbuf)}
| _
   {putc (lexeme_char lexbuf 0) ; in_comment lexbuf}
| eof
    {error "End of file in comment" lexbuf}

{

let to_string = function
  | Open (_,_,txt)  | Close (_,txt)  | Text txt  | Blanks txt -> txt
  | Eof -> "Eof"

let rec cost = function
  | {tag=FONT ; attrs=attrs} -> (1,List.length attrs)
  | _          -> (1,0)

let tok_buff = ref None
;;

let txt_buff = Buff.create ()
;;

let rec read_tokens blanks lb =
  let t = main lb in
  match t with
  | Text txt -> Buff.put txt_buff txt ; read_tokens false lb
  | Blanks txt -> Buff.put txt_buff txt ; read_tokens blanks lb
  | _ ->
      let txt = Buff.to_string txt_buff in
      match txt with
      | "" -> t
      | _  ->
          tok_buff := Some t ;
          if blanks then
            Blanks txt
          else
            Text txt

let reset () =
  txt_level := 0 ;
  Stack.reset txt_stack ;
  Buff.reset txt_buff ;
  Buff.reset buff ;
  Buff.reset abuff

let next_token lb =
  try match !tok_buff with
  | Some t -> tok_buff := None ; t
  | None   -> read_tokens true lb
  with
  | e ->
      reset () ;
      raise e

} 

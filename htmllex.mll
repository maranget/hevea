(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: htmllex.mll,v 1.13 2007-02-08 17:48:28 maranget Exp $          *)
(***********************************************************************)
{
open Lexing
open Lexeme
open Buff

let txt_level = ref 0
and txt_stack = MyStack.create "htmllex"

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
  if not (MyStack.empty txt_stack) then begin
    let pos = MyStack.top txt_stack in
    Location.print_this_fullpos pos ;
    prerr_endline "This opening tag is pending"
  end

let warnings = ref true

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
   "Q",Q ; "SUB",SUB ; "SUP",SUP ; "A", A ; "SPAN", SPAN ; "SCRIPT", SCRIPT;
    "STYLE", STYLE; ]
;;

let is_textlevel name =
  try
    let _ = Hashtbl.find text (String.uppercase name) in
    true
  with
  | Not_found -> false

let is_br name = "BR" = (String.uppercase name)
let is_basefont name = "BASEFONT" = (String.uppercase name)

let set_basefont attrs lb = 
  List.iter
    (fun (name,v,_) -> match String.uppercase name,v with
    | "SIZE",Some s ->
        begin try
          Emisc.basefont := int_of_string s
        with
        | _ -> error "BASEFONT syntax" lb
        end
    | _ -> ())
    attrs

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
    MyStack.push txt_stack (Location.get_pos ()) ;
    Open (tag, attrs,txt)
  with
  | Not_found -> assert false

and ferme lb name txt =
  try
    let tag = Hashtbl.find text (String.uppercase name) in
    decr txt_level ;
    begin if not (MyStack.empty txt_stack) then
      let _  = MyStack.pop txt_stack in ()
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
let tag = ['a'-'z''A'-'Z''0'-'9']+
let class_name = ['a'-'z''A'-'Z''0'-'9''-']+
rule main = parse
| (blank|"&nbsp;"|"&XA0;")+ as lxm {Blanks lxm}
| "<!--"
  {put (lexeme lexbuf) ;
  in_comment lexbuf ;
  Text (Buff.to_string buff)}
| "<!"
  {put (lexeme lexbuf) ;
  in_tag lexbuf ;
  Text (Buff.to_string buff)}
| '<' (tag as tag) as lxm
    {put lxm ;
    if is_textlevel tag then begin
      let attrs = read_attrs lexbuf in    
      ouvre lexbuf tag attrs (Buff.to_string buff)
    end else if is_basefont tag then begin
      let attrs = read_attrs lexbuf in    
      set_basefont attrs lexbuf ;
      Text (Buff.to_string buff)          
    end else begin
      check_nesting lexbuf tag ;
      in_tag lexbuf ;
      let txt = Buff.to_string buff in
      if is_br tag then 
        Blanks txt
      else
        Text txt
    end}
|  "</"  (tag as tag) as lxm
    {put lxm ;
    in_tag lexbuf ;
    ferme lexbuf tag (Buff.to_string buff)}
| eof {Eof}
| _ as c
    {putc c ;
    text lexbuf ;
    Text (Buff.to_string buff)}

and text = parse
| [^'<'] as c
  {putc c ; text lexbuf}
| "" {()}

and read_attrs = parse
| blank+ as lxm
    {aput lxm ; read_attrs lexbuf}
| ['a'-'z''A'-'Z''-''0'-'9']+ as name
  {aput name ;
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
| '\'' ([^'\'']* as x) '\''
| '"' ([^'"']* as x) '"' as lxm
    {aput lxm ;
    x}
| '#'?['a'-'z''A'-'Z''0'-'9''-''+''_'':''.']+ as lxm
    {aput lxm ;
    lxm}
| "" {error "Attribute syntax" lexbuf}

and in_tag = parse
| '>' {putc '>'}
| _ as c   {putc c ; in_tag lexbuf}
| eof {error "End of file in tag" lexbuf}

and in_comment = parse
| "-->" '\n'?
   {put (lexeme lexbuf)}
| _ as c
   {putc c ; in_comment lexbuf}
| eof
   {error "End of file in comment" lexbuf}

and styles = parse
| blank+ { styles lexbuf }
| eof    { [] }
| blank* '.' ([^'{'' ''\t''\n']+ as name) blank*
    ('{' [^'}']* '}' as cl)
  { Css.Class (name, cl) :: styles lexbuf }
| blank* ([^'{']+ '{' [^'}']* '}' as lxm)
  {Css.Other lxm :: styles lexbuf}

(* Extract classes: values of the CLASS attribute *)
and extract_classes cls = parse
| "<!--"
  { skip_comment lexbuf ; extract_classes cls lexbuf}
| "<!"|"</"
  { skip_tag lexbuf ; extract_classes cls lexbuf }
| '<' tag
    { let cls = extract_attrs cls lexbuf in
      extract_classes cls lexbuf }
| [^'<']+ { extract_classes cls lexbuf }
| eof      { cls }

and skip_comment = parse
| "-->" { () }
| _     { skip_comment lexbuf }
| eof   { error "End of file in comment" lexbuf }

and skip_tag = parse
| [^'>']* '>' { () }
| eof         { error "End of file in tag" lexbuf }

and skip_value = parse
| '\'' [^'\'']* '\''
| '"'  [^'"']*  '"'
| '#'?['a'-'z''A'-'Z''0'-'9''-''+''_'':''.']+
   { () }
| "" { error "Attribute syntax" lexbuf }

and extract_value cls = parse
| ['a'-'z''A'-'Z''0'-'9''-''+''_'':''.']+ as name
   { Emisc.Strings.add name cls }
| '\''
    { extract_values_q cls lexbuf }
| '"'
    { extract_values_qq cls lexbuf }
| "" { error "Attribute syntax" lexbuf }

and extract_values_q cls = parse
| blank+ { extract_values_q cls lexbuf }
| class_name as cl { extract_values_q (Emisc.Strings.add cl cls) lexbuf }
| '\'' { cls }
| "" { error "Class value syntax" lexbuf }

and extract_values_qq cls = parse
| blank+ { extract_values_qq cls lexbuf }
| class_name as cl { extract_values_qq (Emisc.Strings.add cl cls) lexbuf }
| '"' { cls }
| ""  { error "Class value syntax" lexbuf }

and extract_attrs cls = parse
(* Blanks or attributes with no value *)
| blank+|['a'-'z''A'-'Z''-''0'-'9']+
   { extract_attrs cls lexbuf }
(* Class attribute *)
| ['c''C']['l''L']['a''A']['s''S']['s''S'] blank* '=' blank*  
   { let cls = extract_value cls lexbuf in
     extract_attrs cls lexbuf }
(* Other attributes with a value *)
| ['a'-'z''A'-'Z''-''0'-'9']+ blank* '=' blank*
   { skip_value lexbuf ;
     extract_attrs cls lexbuf }
(* End of tag *)
| '/'? '>' { cls }
| ""  { error "Attribute syntax" lexbuf }


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
  MyStack.reset txt_stack ;
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

let classes lexbuf =
  extract_classes Emisc.Strings.empty lexbuf

} 

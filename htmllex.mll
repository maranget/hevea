(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


{

open Lexeme

let to_string = function
  | Open (_,_,txt)  | Close (_,txt)  | Text txt  | Blanks txt -> txt
  | Eof -> "Eof"

let cost = function
  | {tag=FONT ; attrs=attrs;_} -> (1,List.length attrs)
  | _          -> (1,0)


module Make(C:DoOut.Config) = struct
open Lexing

module Out = DoOut.Make(C)

let txt_level = ref 0
and txt_stack = MyStack.create "htmllex"

let error msg _lb =  raise (Emisc.LexError msg)


let init table (s,t)= Hashtbl.add table s t
;;

let block = Hashtbl.create 17
;;

List.iter (init block)
  ["center", () ; "div", (); "blockquote", () ;
  "h1", () ; "h2", () ;"h3", () ;"h4", () ;"h5", () ;"h6", () ;
  "pre", () ; "table", () ; "tr",() ; "td", () ; "th",() ; 
  "ol",() ; "ul",(); "p",() ; "li",() ;
  "dl",() ; "dt", () ; "dd",() ;
  ]
;;

let ptop () =
  if not (MyStack.empty txt_stack) then begin
    let pos = MyStack.top txt_stack in
    Location.print_this_fullpos pos ;
    prerr_endline "This opening tag is pending"
  end

let warnings = ref true

let check_nesting _lb name =
  try
    Hashtbl.find block (String.lowercase name) ;
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
  ["tt",TT ; "i",I ; "b",B ; "big",BIG ; "small",SMALL ;
   "strike",STRIKE ; "s",S ; "u",U ; "font",FONT ;
   "em",EM ; "strong",STRONG ; "dfn",DFN ; "code",CODE ; "samp",SAMP ;
   "kbd",KBD ; "var",VAR ; "cite",CITE ; "abbr",ABBR ; "acronym",ACRONYM ; 
   "q",Q ; "sub",SUB ; "sup",SUP ; "a", A ; "span", SPAN ; "script", SCRIPT;
    "style", STYLE; ]
;;

let is_textlevel name =
  try
    let _ = Hashtbl.find text (String.lowercase name) in
    true
  with
  | Not_found -> false

let is_br name = "br" = (String.lowercase name)
let is_basefont name = "basefont" = (String.lowercase name)

let set_basefont attrs lb = 
  List.iter
    (fun (name,v,_) -> match String.lowercase name,v with
    | "size",Some s ->
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

let is_size_relative v =
  match v with
  | "xx-small" | "x-small" | "small" | "medium"
  | "large" | "x-large" | "xx-large" 
      -> false
  | _ -> true
  
let font_value lb v =  
  let v = get_value lb v in
  try
    let k = String.index v ':' in
    let tag = String.sub v 0 k
    and v = String.sub v (k+1) (String.length v - (k+1)) in
    let tag =
      match String.lowercase tag with
      | "font-family" -> Ffamily
      | "font-style" -> Fstyle
      | "font-variant" -> Fvariant
      | "font-weight" -> Fweight
      | "font-size" ->
(* Catch case 'font-size:xxx%' which does not commute
   (with other font-size styles *)
          if is_size_relative v then raise Exit
          else Fsize
      | "color" -> Fcolor
      | "background-color" -> Fbgcolor
      | _ -> raise Exit in
    begin (* checks just one style *)
      try ignore (String.index v ';') ; raise Exit
      with
      | Exit -> raise Exit
      | _ -> ()
    end ;
    tag,v
  with _ -> raise Exit

let norm_attrs lb attrs =
   List.map
        (fun (name,value,txt) ->
          match String.lowercase name with
          | "size" ->  SIZE (get_value lb value),txt
          | "color" -> COLOR (get_value lb value),txt
          | "face" -> FACE (get_value lb value),txt
          | "style" ->
              begin try
                let st,v = font_value lb value in
                ASTYLE (st,v),txt
              with Exit -> OTHER,txt
              end
          | _      -> OTHER, txt)
    attrs

let ouvre lb name attrs txt =
  let uname = String.lowercase name in
  try
    let tag = Hashtbl.find text uname in
    let attrs = norm_attrs lb attrs in
    incr txt_level ;
    MyStack.push txt_stack (Location.get_pos ()) ;
    Open (tag,attrs,txt)
  with
  | Not_found -> assert false

and ferme _lb name txt =
  try
    let tag = Hashtbl.find text (String.lowercase name) in
    decr txt_level ;
    begin if not (MyStack.empty txt_stack) then
      let _  = MyStack.pop txt_stack in ()
    end ;
    Close (tag,txt)
  with
  | Not_found -> Text txt

let buff = Out.create_buff ()
and abuff = Out.create_buff ()

let put s = Out.put buff s
and putc c = Out.put_char buff c

let aput s = Out.put abuff s




}
 
let blank = [' ''\t''\n''\r']
let tag = ['a'-'z''A'-'Z''0'-'9']+
let class_name = ['a'-'z''A'-'Z''0'-'9''-']+
let attr_name = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''-''0'-'9'':']*

rule main = parse
| (blank|"&nbsp;"|"&XA0;")+ as lxm {Blanks lxm}
| "<!--"
  {put (lexeme lexbuf) ;
  in_comment lexbuf ;
  Text (Out.to_string buff)}
| "<!"
  {put (lexeme lexbuf) ;
  in_tag lexbuf ;
  Text (Out.to_string buff)}
| '<' (tag as tag) as lxm
    {put lxm ;
    if is_textlevel tag then begin
      let attrs = read_attrs lexbuf in    
      ouvre lexbuf tag attrs (Out.to_string buff)
    end else if is_basefont tag then begin
      let attrs = read_attrs lexbuf in    
      set_basefont attrs lexbuf ;
      Text (Out.to_string buff)          
    end else begin
      check_nesting lexbuf tag ;
      in_tag lexbuf ;
      let txt = Out.to_string buff in
      if is_br tag then 
        Blanks txt
      else
        Text txt
    end}
|  "</"  (tag as tag) as lxm
    {put lxm ;
    in_tag lexbuf ;
    ferme lexbuf tag (Out.to_string buff)}
| eof {Eof}
| _ as c
    {putc c ;
    text lexbuf ;
    Text (Out.to_string buff)}

and text = parse
| [^'<'] as c
  {putc c ; text lexbuf}
| "" {()}

and read_attrs = parse
| blank+ as lxm
    {aput lxm ; read_attrs lexbuf}
| attr_name as name
  {aput name ;
  let v = read_avalue lexbuf in
  let atxt = Out.to_string abuff in
  put atxt ;
  (name,v,atxt)::read_attrs lexbuf}
| '>' {Out.put_char buff '>' ; []}
| ""  {error "Attribute syntax (read_attrs)" lexbuf}

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
(* '"' *)
| "" {error "Attribute syntax (read_aavalue)" lexbuf}

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
    ((tag blank*)+ as addname)?
    ('{' [^'}']* '}' as cl)
  { Css.Class (name, addname, cl) :: styles lexbuf }
| blank* ([^'{']+ '{' [^'}']* '}' as lxm)
  {Css.Other lxm :: styles lexbuf}

(* Extract classes: values of the CLASS attribute *)
and extract_classes cls = parse
| "<!--" | "-->" (* ignore comment markers *)
  { extract_classes cls lexbuf}
| "<!"|"</"
  { skip_tag lexbuf ; extract_classes cls lexbuf }
| '<' tag
    { let cls = extract_attrs cls lexbuf in
      extract_classes cls lexbuf }
| [^'<']+ { extract_classes cls lexbuf }
| eof      { cls }
| "" { error "Extract classes" lexbuf }

and skip_tag = parse
| [^'>']* '>' { () }
| eof         { error "End of file in tag" lexbuf }

and skip_value = parse
| '\'' [^'\'']* '\''
| '"'  [^'"']*  '"'
| '#'?['a'-'z''A'-'Z''0'-'9''-''+''_'':''.']+
   { () }
| "" { error "Attribute syntax (skip_value)" lexbuf }
(* '"' *)

and extract_value cls = parse
| ['a'-'z''A'-'Z''0'-'9''-''+''_'':''.']+ as name
   { Emisc.Strings.add name cls }
| '\''
    { extract_values_q cls lexbuf }
| '"' (* '"' *)
    { extract_values_qq cls lexbuf }
| "" { error "Attribute syntax (extract_value)" lexbuf }

and extract_values_q cls = parse
| blank+ { extract_values_q cls lexbuf }
| class_name as cl { extract_values_q (Emisc.Strings.add cl cls) lexbuf }
| '\'' { cls }
| "" { error "Class value syntax" lexbuf }

and extract_values_qq cls = parse
| blank+ { extract_values_qq cls lexbuf }
| class_name as cl { extract_values_qq (Emisc.Strings.add cl cls) lexbuf }
| '"' { cls } (* '"' *)
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
| attr_name blank* '=' blank*
   { skip_value lexbuf ;
     extract_attrs cls lexbuf }
(* End of tag *)
| '/'? '>' { cls }
| ""  { error "Attribute syntax (extract_attrs)" lexbuf }


{

let tok_buff = ref None
;;

let txt_buff = Out.create_buff ()
;;

let rec read_tokens blanks lb =
  let t = main lb in
  match t with
  | Text txt -> Out.put txt_buff txt ; read_tokens false lb
  | Blanks txt -> Out.put txt_buff txt ; read_tokens blanks lb
  | _ ->
      let txt = Out.to_string txt_buff in
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
  Out.reset txt_buff ;
  Out.reset buff ;
  Out.reset abuff

let next_token lb =
  try match !tok_buff with
  | Some t -> tok_buff := None ; t
  | None   -> read_tokens true lb
  with
  | e ->
      reset () ;
      raise e

let classes lexbuf =
  let r = extract_classes Emisc.Strings.empty lexbuf in
  r
end
} 

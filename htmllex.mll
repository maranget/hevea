{
open Lexing
open Lexeme
open Buff

let text = Hashtbl.create 17
;;

let init (s,t)= Hashtbl.add text s t
;;

List.iter init
  ["TT",TT ; "I",I ; "B",B ; "BIG",BIG ; "SMALL",SMALL ;
   "STRIKE",STRIKE ; "S",S ; "U",U ; "FONT",FONT ;
   "EM",EM ; "STRONG",STRONG ; "DFN",DFN ; "CODE",CODE ; "SAMP",SAMP ;
   "KBD",KBD ; "VAR",VAR ; "CITE",CITE ; "ABBR",ABBR ; "ACRONYM",ACRONYM ; 
   "Q",Q ; "SUB",SUB ; "SUP",SUP ; "A", A]
;;

let is_textlevel name =
  try
    let _ = Hashtbl.find text (String.uppercase name) in
    true
  with
  | Not_found -> false

exception Error of string
;;

let error msg lb =
  raise (Error (msg^" at char: "^string_of_int (lexeme_start lb)))
;;

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
  try
    let tag = Hashtbl.find text (String.uppercase name) in
    let attrs = norm_attrs lb attrs in
    Open (tag, attrs,txt)
  with
  | Not_found -> Text txt
        

and ferme lb name txt =
  try
    let tag = Hashtbl.find text (String.uppercase name) in
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
 

let blank = [' ''\t''\n']

rule main = parse
| (blank|"&nbsp;"|"<BR>")+
    {Blanks (lexeme lexbuf)}
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
      in_tag lexbuf ;
      Text (Buff.to_string buff)
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
| ['a'-'z''A'-'Z''-']+
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
let next_token lb = main lb
;;

let to_string = function
  | Open (_,_,txt)  | Close (_,txt)  | Text txt  | Blanks txt -> txt
  | Eof -> "Eof"
} 

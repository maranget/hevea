{
open Lexing

let buff = Out.create_buff ()
;;

let put s =
  Out.put buff s
and put_char c =
  Out.put_char buff c
;;


type res = Bang of string | Arobas of string | Bar of string 
| Eof of string
;;

exception Fini
;;

}
rule entry = parse
  "\\\""
    {put "\\\"" ; entry lexbuf}
| "\"!"
    {put_char '!' ; entry lexbuf}
| "\"@"
    {put_char '@' ; entry lexbuf}
| "\"|"
    {put_char '|' ; entry lexbuf}
| '!' {Bang   (Out.to_string buff)}
| '@' {Arobas (Out.to_string buff)}
| '|' {Bar (Out.to_string buff)}
| eof {Eof (Out.to_string buff)}
| _
   {let lxm = lexeme_char lexbuf 0 in put_char lxm ; entry lexbuf}      


and idx = parse
  "\\indexentry"
     {let x = Save.arg lexbuf in
     let _ = Save.arg lexbuf in
     x}
| eof {raise Fini}
| _   {idx lexbuf}



{
open Lexing

let buff = Out.create_buff ()
;;

let put s =
  Out.put buff s
and put_char c =
  Out.put_char buff c
;;

let nesting = ref 0
;;

exception Over of string * string
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
| eof {raise (Over (Out.to_string buff,""))}
| '!'
    {Out.to_string buff,""}
| '@'
    {let name = Out.to_string buff in
    try
      let pretty,_ = entry lexbuf in
      name,pretty
    with Over (s,_) -> raise (Over (name,s))}
| _
   {let lxm = lexeme_char lexbuf 0 in put_char lxm ; entry lexbuf}      


and idx = parse
  "\\indexentry"
     {let x = Save.arg lexbuf in
     let _ = Save.arg lexbuf in
     x}
| eof {raise Fini}
| _ {idx lexbuf}



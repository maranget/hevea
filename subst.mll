{
open Lexing

let debug m lex =
  Printf.fprintf stderr "%s : %s\n" m lex
;;

let outs = Out.create_buff ()
;;

}

rule subst = parse
  "\\#"
   {fun stack ->
   Out.put outs "\\#" ; subst lexbuf stack}
| '#'['1'-'9']
   {fun stack ->
   let s = lexeme lexbuf in
   let i = Char.code (String.get s 1) - Char.code '1' in
   Out.put outs stack.(i) ; subst lexbuf stack}
| [^'#''\\']+
   {fun stack ->
   let s = lexeme lexbuf in
   Out.put outs s ;  subst lexbuf stack}
| _
   {fun stack ->
   let s = lexeme lexbuf in Out.put outs s ; subst lexbuf stack}
| eof
   {fun stack -> Out.to_string outs}



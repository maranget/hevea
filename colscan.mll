{
open Lexing

let buf = Out.create_buff ()
;;
} 
rule one = parse
  ' '*('0'|'1')?'.'?['0'-'9']*' '*
  {let lxm = lexeme lexbuf in
  float_of_string lxm}
| "" {failwith "Colscan.one"}

and other = parse
  ' '* ',' {one lexbuf}
|  ""      {failwith "Colscan.other"}

and three = parse
  ""
  {let fst = one lexbuf in
  let snd = other lexbuf in
  let thrd = other lexbuf in
  fst,snd,thrd}

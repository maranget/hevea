{
open Lexing
let header = "$Id: length.mll,v 1.2 1998-09-04 10:57:52 maranget Exp $" 

exception No
;;

let font = 12.0
;;

let convert unit x =
  truncate (0.5 +.
    (match unit with
      "ex"|"em" -> x
    |  "pt"     -> x /. font
    |  "in"     -> (x *. 72.27) /. font
    |  "cm"     -> (x *. 28.47) /. font
    |  "mm"     -> (x *. 2.847) /. font
    |  "pc"     -> (x *. 12.0)  /. font
    |  _ -> raise No))
;;

}

rule main = parse
  '-' {let x,unit = positif lexbuf in convert unit (0.0 -. x)}
|  "" {let x,unit = positif lexbuf in convert unit x}

and positif = parse
  ['0'-'9']*'.'?['0'-'9']+
   {let lxm = lexeme lexbuf in
   float_of_string lxm,unit lexbuf}
|  "" {raise No}
and unit = parse
  _ * {lexeme lexbuf}



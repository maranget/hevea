{
open Lexing
let header = "$Id: length.mll,v 1.1 1998-09-02 15:47:47 maranget Exp $" 

exception No
;;
let convert unit x =
  truncate (0.5 +.
    (match unit with
      "cm" -> x *. 3.0
    | "ex"|"em" -> x
    |  "in"     -> x *. 6.0
    |  "pt"     -> x /. 12.0
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
  "ex"|"cm"|"em"|"pt"|"in" {lexeme lexbuf}
|  ""                      {raise No}


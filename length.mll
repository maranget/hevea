{
open Lexing
let header = "$Id: length.mll,v 1.6 1999-05-07 11:33:54 maranget Exp $" 

exception No
;;

let font = 10
;;

let font_float = float font
;;

type t = Absolute of int | Percent of int

let mk_absolute x = Absolute (truncate (0.5 +. x))
and mk_percent x = Percent (truncate x)

let convert unit x = match unit with
      "ex"|"em" -> mk_absolute x
    |  "pt"     -> mk_absolute (x /. font_float)
    |  "in"     -> mk_absolute ((x *. 72.27) /. font_float)
    |  "cm"     -> mk_absolute ((x *. 28.47) /. font_float)
    |  "mm"     -> mk_absolute ((x *. 2.847) /. font_float)
    |  "pc"     -> mk_absolute ((x *. 12.0)  /. font_float)
    | "\\linewidth" -> mk_percent (100.0 *. x)
    |  _ -> raise No
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



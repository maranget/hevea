(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: length.mll,v 1.16 2012-06-05 14:55:39 maranget Exp $          *)
(***********************************************************************)

{
open Lexing

exception Cannot
;;

let font = 10
;;

let font_float = float font
;;
type t =
  Char of int | Pixel of int | Percent of int | No of string | Default

let pretty = function
  | Char x -> string_of_int x^" chars"
  | Pixel x -> string_of_int x^" pxls"
  | Percent x  -> string_of_int x^"%"
  | Default    -> "default"
  | No s       -> "*"^s^"*"

let is_zero = function
  | Char 0|Pixel 0|Percent 0 -> true
  | _ -> false

let pixel_to_char x =  (100 * x + 50)/(100 * font)
and char_to_pixel x = font * x
  
let mk_char x = Char (truncate (0.5 +. x))
let mk_pixel x = Pixel (truncate (0.5 +. x))
and mk_percent x = Percent (truncate x)
;;

let convert unit x = match unit with
    |   "ex"|"em" -> mk_char x
    |  "pt"     -> mk_pixel x
    |  "in"     -> mk_char ((x *. 72.27) /. font_float)
    |  "cm"     -> mk_char ((x *. 28.47) /. font_float)
    |  "mm"     -> mk_char ((x *. 2.847) /. font_float)
    |  "pc"     -> mk_char ((x *. 12.0)  /. font_float)
    |  "@percent" -> mk_percent (100.0 *. x)
    |  _ -> No unit
;;

}

rule main_rule = parse
  '-' {let x,unit = positif lexbuf in convert unit (0.0 -. x)}
|  "" {let x,unit = positif lexbuf in convert unit x}

and positif = parse
| ['0'-'9']*'.'?['0'-'9']+
   {let lxm = lexeme lexbuf in
   float_of_string lxm,unit lexbuf}
| "@percent"  {1.0, "@percent"}
|  "" {raise Cannot}
and unit = parse
| [' ''\n''\t''\r']+ {unit lexbuf}
| [^' ''\n''\t''\r']* {lexeme lexbuf}

{
open Lexing

let main lexbuf =
  try main_rule lexbuf with
  | Cannot ->
      let sbuf = lexbuf.lex_buffer in
      No (String.sub sbuf 0 lexbuf.lex_buffer_len)

} 

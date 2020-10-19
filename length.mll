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

exception ConversionFailure
;;

let base_font_size = 12 (* Reasonable default: 1em = 12pt; units: pt *)
;;

let base_font_size_float = float base_font_size
;;

(*  [base_font_relative_ex_size_float] is just a heuristic; any value
 *  between 0.42 to 0.54 seems reasonable, i.e. is backed by fonts
 *  actually used in browsers.
 *
 *  W3C: "In the cases where it is impossible or impractical to
 *  determine the x-height, a value of 0.5em should be used." *)
let base_font_relative_ex_size_float = 0.5
;;

let points_per_pixel = 0.75 (* W3C definition: 12pt = 16px; units: pt/px *)
and pixels_per_char = 16 (* units: px/char *)
;;

let pixel_to_char x = (100 * x + 50) / (100 * pixels_per_char)
and pixel_to_char_float x = float_of_int x /. float_of_int pixels_per_char
and char_to_pixel x = pixels_per_char * x
;;

type t =
  | Char of int
  | Pixel of int
  | Percent of int
  | NotALength of string
  | Default

let pretty = function
  | Char x -> string_of_int x ^ " chars"
  | Pixel x -> string_of_int x ^ " pxls"
  | Percent x -> string_of_int x ^ "%"
  | Default -> "default"
  | NotALength s -> "*" ^ s ^ "*"

let is_zero = function
  | Char 0 | Pixel 0 | Percent 0 -> true
  | _ -> false

let as_number_of_chars = function
  | Char n -> n
  | Pixel x -> pixel_to_char x
  | Percent _ | NotALength _ | Default -> raise ConversionFailure
;;

let pixel_of_em x = Pixel (int_of_float (Float.round (float_of_int pixels_per_char *. x)))
and pixel_of_point x = Pixel (int_of_float (Float.round (x /. points_per_pixel)))
and as_percent x = Percent (int_of_float (Float.round x))

let convert unit x =
  (* mainly TeX Book, Chapter 10 *)
  match unit with
  | "bp" -> pixel_of_point (x *. 72.27 /. 72.0)
  | "cc" -> pixel_of_point (x *. 14856.0 /. 1157.0)
  | "cm" -> pixel_of_em ((x *. 28.47) /. base_font_size_float)
  | "dd" -> pixel_of_point (x *. 1238.0 /. 1157.0)
  | "em" -> pixel_of_em x
  | "ex" -> pixel_of_em (x *. base_font_relative_ex_size_float)
  | "in" -> pixel_of_em ((x *. 72.27) /. base_font_size_float)
  | "mm" -> pixel_of_em ((x *. 2.847) /. base_font_size_float)
  | "pc" -> pixel_of_em ((x *. 12.0)  /. base_font_size_float)
  | "pt" -> pixel_of_point x
  | "sp" -> pixel_of_point (x /. 65536.0)
  | "@percent" -> as_percent (100.0 *. x)
  | _ -> NotALength unit
;;

}

rule main_rule = parse
  '-' {let x, unit = positif lexbuf in convert unit (0.0 -. x)}
|  "" {let x, unit = positif lexbuf in convert unit x}

and positif = parse
| ['0'-'9']*'.'?['0'-'9']+
   {let lxm = lexeme lexbuf in float_of_string lxm, unit lexbuf}
| "@percent" {1.0, "@percent"}
| "" {raise ConversionFailure}
and unit = parse
| [' ''\n''\t''\r']+ {unit lexbuf}
| [^' ''\n''\t''\r']* {lexeme lexbuf}

{
open Lexing

let main lexbuf =
  try main_rule lexbuf with
  | ConversionFailure ->
      let sbuf = lexbuf.lex_buffer in
      NotALength (Bytes.sub_string sbuf 0 lexbuf.lex_buffer_len)
}

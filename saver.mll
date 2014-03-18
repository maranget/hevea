(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

{
open Lexing
open SaveUtils

module type Config = sig
  type t
  val of_string : string -> t
  val of_out : Out.t -> t
end

module type S = sig
  type out
  val opt : Lexing.lexbuf -> out
  val arg : Lexing.lexbuf -> out
  val arg2 : Lexing.lexbuf -> out
end

module Make(C:Config) = struct
  type out = C.t

}
let command_name =
 '\\' (( ['@''A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'] | "\\*")
let space = [' ''\t''\r']

rule opt = parse
| space* '\n'? space* '['
    {put_echo (lexeme lexbuf) ;
    opt2 lexbuf}
| '%' { skip_comment lexbuf ; opt lexbuf }
|  eof  {raise Eof}
|  ""   {raise NoOpt}


and opt2 =  parse
| '{'         {incr brace_nesting;
                 put_both_char '{' ; opt2 lexbuf}
| '}'        { decr brace_nesting;
               if !brace_nesting >= 0 then begin
                 put_both_char '}' ; opt2 lexbuf
               end else begin
                 error "Bad brace nesting in optional argument"
               end}
| ']'
    {if !brace_nesting > 0 then begin
      put_both_char ']' ; opt2 lexbuf
    end else begin
      put_echo_char ']' ;
      C.of_out arg_buff
    end}
| '%' { skip_comment lexbuf ; opt2 lexbuf }
| command_name as lxm
   {put_both lxm ; opt2 lexbuf }
| _ as lxm 
   {put_both_char lxm ; opt2 lexbuf }

and skip_comment = parse
  | eof       {()}
  | '\n' space* {()}
  | _         {skip_comment lexbuf}

and arg = parse
    space+ | '\n'+  {put_echo (lexeme lexbuf) ; arg lexbuf}
  | '{'
      {incr brace_nesting;
      put_echo_char '{' ;
      arg2 lexbuf}
  | '%'
     {skip_comment lexbuf  ; arg lexbuf}
  | "\\box" '\\' (['A'-'Z' 'a'-'z']+ '*'? | [^ 'A'-'Z' 'a'-'z'])
     {let lxm = lexeme lexbuf in
     put_echo lxm ;
     C.of_string lxm}
  | command_name
     {blit_both lexbuf ;
     skip_blanks lexbuf}
  | '#' ['1'-'9']
     {let lxm = lexeme lexbuf in
     put_echo lxm ;
     C.of_string lxm}
  | [^ '}']
      {let c = lexeme_char lexbuf 0 in
      put_both_char c ;
      C.of_out arg_buff}
  | eof    {raise Eof}
  | ""     {error "Argument expected"}


and skip_blanks = parse
| space* '\n' as lxm
    {seen_par := false ;
    put_echo lxm ;
    more_skip lexbuf}
| space*  as lxm
    {put_echo lxm ; C.of_out arg_buff}

and more_skip = parse
  (space* '\n' space*)+ as lxm
   {seen_par := true ;
   put_echo lxm ;
   more_skip lexbuf}
| space* as lxm
  { put_echo lxm ; C.of_out arg_buff}

and arg2 = parse
  '{'         
     {incr brace_nesting;
     put_both_char '{' ;
     arg2 lexbuf}
| '}'
     {decr brace_nesting;
     if !brace_nesting > 0 then begin
       put_both_char '}' ; arg2 lexbuf
     end else begin
       put_echo_char '}' ;
       C.of_out arg_buff
     end}
| '%'
     {skip_comment lexbuf  ; arg2 lexbuf}
| command_name
| [^'\\''{''}''%']+
      {blit_both lexbuf ; arg2 lexbuf }
| _
    {let c = lexeme_char lexbuf 0 in
    put_both_char c ; arg2 lexbuf}
| eof
    {error "End of file in argument"}


{

end

module String =
  Make
    (struct
      type t = string
      let of_string x = x
      let of_out = Out.to_string
    end)

module List =
  Make
    (struct
      type t = string list
      let of_string x = [x]
      let of_out = Out.to_list
    end)

} 

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

let header = "$Id: save.mll,v 1.25 1999-01-28 22:21:26 maranget Exp $" 

let verbose = ref 0 and silent = ref false
;;
let set_verbose s v =
  silent := s ; verbose := v
;;

let seen_par = ref false
;;

type formatopt = Wrap | NoMath
type format = Align of string * formatopt list | Inside of string
;;

let border = ref false

let brace_nesting = ref 0
and arg_buff = Out.create_buff ()
and echo_buff = Out.create_buff ()
and delim_buff = Out.create_buff ()
and tag_buff = Out.create_buff ()
;;

let echo = ref false
;;

let get_echo () = echo := false ; Out.to_string echo_buff
and start_echo () = echo := true ; Out.reset echo_buff
;;


exception BadParse of string
;;
exception EofDelim of int
;;
exception NoDelim
;;
exception NoOpt
;;

let put_echo s =
  if !echo then Out.put echo_buff s
and put_echo_char c =
  if !echo then Out.put_char echo_buff c
;;

let put_both s =
  put_echo s ; Out.put arg_buff s
;;

let put_both_char c =
  put_echo_char c ; Out.put_char arg_buff c
;;

}

rule opt = parse
   '['
        {put_echo_char '[' ;
        opt2 lexbuf}
| ' '+  {put_echo (lexeme lexbuf) ; opt lexbuf}
|  eof  {raise (BadParse "EOF")}
|  ""   {raise NoOpt}


and opt2 =  parse
    '{'         {incr brace_nesting;
                put_both_char '{' ; opt2 lexbuf}
  | '}'        { decr brace_nesting;
                 if !brace_nesting >= 0 then begin
                    put_both_char '}' ; opt2 lexbuf
                 end else begin
                   failwith "Bad brace nesting in optional argument"
                 end}
  | ']'
      {if !brace_nesting > 0 then begin
        put_both_char ']' ; opt2 lexbuf
      end else begin
        put_echo_char ']' ;
        Out.to_string arg_buff
      end}
  | _
      {let s = lexeme_char lexbuf 0 in
      put_both_char s ; opt2 lexbuf }

and arg = parse
    ' '+   {put_echo (lexeme lexbuf) ; arg lexbuf}
  | '\n'+  {put_echo (lexeme lexbuf) ; arg lexbuf}
  | '{'
      {incr brace_nesting;
      put_echo_char '{' ;
      arg2 lexbuf}
  | '%' [^'\n']* '\n'
     {put_echo (lexeme lexbuf) ; arg lexbuf}
  | "\\box" '\\' (['A'-'Z' 'a'-'z']+ '*'? | [^ 'A'-'Z' 'a'-'z'])
     {let lxm = lexeme lexbuf in
     put_echo lxm ;
     lxm}
  | '\\' ( [^'A'-'Z' 'a'-'z'] | ('@' ? ['A'-'Z' 'a'-'z']+ '*'?))
     {put_both (lexeme lexbuf) ;
     skip_blanks lexbuf}
  | [^ '}']
      {let c = lexeme_char lexbuf 0 in
      put_both_char c ;
      Out.to_string arg_buff}
  | eof    {raise (BadParse "EOF")}
  | ""     {raise (BadParse "Empty Arg")}

and skip_blanks = parse
  ' '+
    {seen_par := false ;
    put_echo (lexeme lexbuf) ;
    skip_blanks lexbuf}
| '\n'
    {put_echo_char '\n' ; more_skip lexbuf}
| ""
    {Out.to_string arg_buff}


and more_skip = parse
  '\n'+
   {seen_par := true ;
   put_echo (lexeme lexbuf) ;
   more_skip lexbuf}
| ""
  {Out.to_string arg_buff}

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
       Out.to_string arg_buff
     end}
| "\\{" | "\\}" | "\\\\"
      {let s = lexeme lexbuf in
      put_both s ; arg2 lexbuf }
| eof    {raise (BadParse "EOF")}
| _
      {let c = lexeme_char lexbuf 0 in
      put_both_char c ; arg2 lexbuf }

and csname = parse
  [' ''\n']+ {put_echo (lexeme lexbuf) ; csname lexbuf}
| '{'? "\\csname" ' '+
      {let lxm = lexeme lexbuf in
      put_echo lxm ; Out.put_char arg_buff '\\' ;
      incsname lexbuf}
| ""  {arg lexbuf}

and incsname = parse
  "\\endcsname"  '}'?
    {let lxm = lexeme lexbuf in
    put_echo lxm ; Out.to_string arg_buff}
| _ 
    {put_both_char (lexeme_char lexbuf 0) ;
    incsname lexbuf}
| eof           {raise (BadParse "EOF (csname)")}

and eat_delim = parse
  "" {fun delim ->
    begin try
      do_eat_delim lexbuf delim 0
    with (NoDelim | EofDelim _) ->
      raise (BadParse ("delim : "^delim))
    end ;
    let _ = Out.to_string delim_buff in
    ()}

and do_eat_delim = parse
  _
   {fun delim i ->
     let lxm = lexeme_char lexbuf 0 in
     let c = String.get delim i in
     Out.put_char delim_buff lxm ;
     if c = lxm then
       if i+1 >= String.length delim then
         ()
       else
         do_eat_delim lexbuf delim (i+1)
     else
         raise NoDelim
   }
| eof {fun delim i -> raise (EofDelim i)}

and arg_delim = parse  "" {fun delim ->  failwith "Hum"}

and cite_arg = parse
  ' '* '{'   {cite_args_bis lexbuf}

and cite_args_bis = parse
  [^'}'' ''\n''%'',']* {let lxm = lexeme lexbuf in lxm::cite_args_bis lexbuf}
|  '%' [^'\n']* '\n' {cite_args_bis lexbuf}
| ','         {cite_args_bis lexbuf}
| [' ''\n']+ {cite_args_bis lexbuf}
| '}'         {[]}

and macro_names = parse
  eof {[]}
| '\\' (('@'? ['A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])
  {let name = lexeme lexbuf in
  name :: macro_names lexbuf}
| _   {macro_names lexbuf}

and num_arg = parse
   '#' ['1'-'9'] 
|  ['0'-'9']+ 
    {let lxm = lexeme lexbuf in
    int_of_string lxm}
|  "'" ['0'-'7']+ 
    {let lxm = lexeme  lexbuf in
    int_of_string ("0o"^String.sub lxm 1 (String.length lxm-1))}
|  '"' ['0'-'9' 'a'-'f' 'A'-'F']+ 
    {let lxm = lexeme  lexbuf in
    int_of_string ("0x"^String.sub lxm 1 (String.length lxm-1))}
| '`' '\\' _
    {let c = lexeme_char lexbuf 2 in
    Char.code c}
| '`' _
    {let c = lexeme_char lexbuf 1 in
    Char.code c}
| "" {failwith "num_arg"}

and input_arg = parse
  [' ''\n']      {put_echo (lexeme lexbuf) ; input_arg lexbuf}
| [^'\n''{'' ']+ {let lxm = lexeme lexbuf in put_echo lxm ; lxm}
| ""             {arg lexbuf}  

and tformat = parse
  'c' {Align ("center",[])::tformat lexbuf}
| 'l' {Align ("left",[])::tformat lexbuf}
| 'r' {Align ("right",[])::tformat lexbuf}
| 'p'
   {let _ = arg lexbuf in
   if !verbose > 0 then begin
     Location.print_pos () ;
     prerr_endline "Warning, p column specification, argument ignored"
   end ;
   Align ("left",[Wrap]):: tformat lexbuf}
| '*'
   {let ntimes = arg lexbuf in let what = arg lexbuf in
   let rec do_rec = function
     0 -> tformat lexbuf
   | i ->
      let sbuf = Lexing.from_string what in
      tformat sbuf@do_rec (i-1) in
   do_rec (int_of_string ntimes)}
| "tc" {Align ("center",[NoMath])::tformat lexbuf}
| "tl" {Align ("left",[NoMath])::tformat lexbuf}
| "tr" {Align ("right",[NoMath])::tformat lexbuf}
| '|' {border := true ; tformat lexbuf}
| '@'
    {let inside = arg lexbuf in
    Inside inside::tformat lexbuf}
| _   {tformat lexbuf}
| eof {[]}

and skip_equal = parse
  ' '* '=' ' '* {()}
| ""            {()}

and get_sup_sub = parse
  ' '* '^'
    {let sup = arg lexbuf in
    sup,get_sub lexbuf}
| ' '* '_'
    {let sub = arg lexbuf in
    get_sup lexbuf,sub}
| "" {("","")}

and get_sup = parse
  ' '* '^'  {arg lexbuf}
| ""   {""}

and get_sub = parse
  ' '* '_'  {arg lexbuf}
| ""   {""}

and defargs = parse 
  '#' ['1'-'9'] | [^'#' '{']+
    {let lxm = lexeme lexbuf in
    put_echo lxm ;
    lxm::defargs lexbuf}
| "" {[]}

and tagout = parse
  '<'  {intag lexbuf}
| "&nbsp;" {Out.put tag_buff " " ; tagout lexbuf}
| "&gt;" {Out.put tag_buff ">" ; tagout lexbuf}
| "&lt;" {Out.put tag_buff "<" ; tagout lexbuf}
| "&nbsp;" {Out.put tag_buff " " ; tagout lexbuf}
| _    {Out.put tag_buff (lexeme lexbuf) ; tagout lexbuf}
| eof  {Out.to_string tag_buff}

and intag = parse
  '>'  {tagout lexbuf}
| '"'  {instring lexbuf}
| _    {intag lexbuf}
| eof  {Out.to_string tag_buff}

and instring = parse
  '"'  {intag lexbuf}
| '\\' '"' {instring lexbuf}
| _    {instring lexbuf}
| eof  {Out.to_string tag_buff}


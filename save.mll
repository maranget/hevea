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

let header = "$Id: save.mll,v 1.46 1999-10-13 08:21:27 maranget Exp $" 

let verbose = ref 0 and silent = ref false
;;

let set_verbose s v =
  silent := s ; verbose := v
;;

exception Error of string
;;
exception Delim of string
;;

let seen_par = ref false
;;

let my_int_of_string s =
  try int_of_string s
  with Failure "int_of_string" ->
    raise (Error ("Integer argument expected: ``"^s^"''"))


let brace_nesting = ref 0
and arg_buff = Out.create_buff ()
and echo_buff = Out.create_buff ()
and tag_buff = Out.create_buff ()
;;

let echo = ref false
;;

let get_echo () = echo := false ; Out.to_string echo_buff
and start_echo () = echo := true ; Out.reset echo_buff
;;


exception Eof
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
| ' '* '\n'? ' '* '['
    {put_echo (lexeme lexbuf) ;
      opt2 lexbuf}
|  eof  {raise Eof}
|  ""   {raise NoOpt}


and opt2 =  parse
    '{'         {incr brace_nesting;
                  put_both_char '{' ; opt2 lexbuf}
| '}'        { decr brace_nesting;
               if !brace_nesting >= 0 then begin
                 put_both_char '}' ; opt2 lexbuf
               end else begin
                 raise (Error "Bad brace nesting in optional argument")
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
    ' '+ | '\n'+  {put_echo (lexeme lexbuf) ; arg lexbuf}
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
  | '#' ['1'-'9']
     {let lxm = lexeme lexbuf in
     put_echo lxm ; lxm}
  | [^ '}']
      {let c = lexeme_char lexbuf 0 in
      put_both_char c ;
      Out.to_string arg_buff}
  | eof    {raise Eof}
  | ""     {raise (Error "Argument expected")}


and first_char = parse
  | _ 
      {let lxm = lexeme_char lexbuf 0 in
      put_echo_char lxm ;
      lxm}
  | eof {raise Eof}

and rest = parse
  |   _ * eof
      {let lxm = lexeme lexbuf in
      put_echo lxm ;
      lxm}
  
and skip_blanks = parse
| ' '* '\n'
    {seen_par := false ;
    put_echo (lexeme lexbuf) ;
    more_skip lexbuf}
| ' '*
    {put_echo (lexeme lexbuf) ; Out.to_string arg_buff}

and more_skip = parse
  (' '* '\n' ' '*)+
   {seen_par := true ;
   put_echo (lexeme lexbuf) ;
   more_skip lexbuf}
| ""
  {Out.to_string arg_buff}

and skip_equal = parse
    [' ']* '='? [' ']* {()}

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
| eof
    {raise (Error ("End of file in argument"))}
| _
      {let c = lexeme_char lexbuf 0 in
      put_both_char c ; arg2 lexbuf }

and csname = parse
  [' ''\n']+ {(fun subst -> put_echo (lexeme lexbuf) ; csname lexbuf subst)}
| '{'? "\\csname" ' '*
      {(fun subst_fun ->
        let lxm = lexeme lexbuf in
        put_echo lxm ;
        let r = incsname lexbuf in
        "\\"^subst_fun r)}
| ""  {fun subst -> let r = arg lexbuf in subst r}

and incsname = parse
  "\\endcsname"  '}'?
    {let lxm = lexeme lexbuf in
    put_echo lxm ; Out.to_string arg_buff}
| _ 
    {put_both_char (lexeme_char lexbuf 0) ;
    incsname lexbuf}
| eof           {raise (Error "End of file in command name")}

and cite_arg = parse
  ' '* '{'   {cite_args_bis lexbuf}
| ""         {raise (Error ("No opening ``{''"))}

and cite_args_bis = parse
  [^'}'' ''\n''%'',']* {let lxm = lexeme lexbuf in lxm::cite_args_bis lexbuf}
|  '%' [^'\n']* '\n' {cite_args_bis lexbuf}
| ','         {cite_args_bis lexbuf}
| [' ''\n']+ {cite_args_bis lexbuf}
| '}'         {[]}
| ""          {raise (Error ("Bad syntax for \\cite argument"))}

and macro_names = parse
  eof {[]}
| '\\' (('@'? ['A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])
  {let name = lexeme lexbuf in
  name :: macro_names lexbuf}
| _   {macro_names lexbuf}

and num_arg = parse
| [' ''\n']+ {(fun get_int -> num_arg lexbuf get_int)}
| ['0'-'9']+ 
    {fun get_int ->
      let lxm = lexeme lexbuf in
      my_int_of_string lxm}
|  "'" ['0'-'7']+ 
    {fun get_int ->let lxm = lexeme  lexbuf in
    my_int_of_string ("0o"^String.sub lxm 1 (String.length lxm-1))}
|  '"' ['0'-'9' 'a'-'f' 'A'-'F']+ 
    {fun get_int ->let lxm = lexeme  lexbuf in
    my_int_of_string ("0x"^String.sub lxm 1 (String.length lxm-1))}
| '`' '\\' _
    {fun get_int ->let c = lexeme_char lexbuf 2 in
    Char.code c}
| '`' '#' ['1'-'9']
    {fun get_int ->
      let lxm = lexeme lexbuf in
      get_int (String.sub lxm 1 2)}
| '`' _
    {fun get_int ->let c = lexeme_char lexbuf 1 in
    Char.code c}
| ""
    {fun get_int ->
      let s = arg lexbuf in
      get_int s}
    

and filename = parse
  [' ''\n']+     {put_echo (lexeme lexbuf) ; filename lexbuf}
| [^'\n''{'' ']+ {let lxm = lexeme lexbuf in put_echo lxm ; lxm}
| ""             {arg lexbuf}  

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
|  '#' ['1'-'9']
    {let lxm = lexeme lexbuf in
    put_echo lxm ;
    lxm::defargs lexbuf}
| [^'{'] | "\\{"
    {put_both (lexeme lexbuf) ;
    let r = in_defargs lexbuf in
    r :: defargs lexbuf}
| "" {[]}

and in_defargs = parse
| "\\{" | "\\#" {put_both (lexeme lexbuf) ; in_defargs lexbuf}
| [^'{''#']     {put_both_char (lexeme_char lexbuf 0) ; in_defargs lexbuf}
| ""            {Out.to_string arg_buff}

and get_defargs = parse
  [^'{']* {let r = lexeme lexbuf in r}

and tagout = parse
| "<BR>" {Out.put_char tag_buff ' ' ; tagout lexbuf}
|  '<'  {intag lexbuf}
| "&nbsp;" {Out.put tag_buff " " ; tagout lexbuf}
| "&gt;" {Out.put tag_buff ">" ; tagout lexbuf}
| "&lt;" {Out.put tag_buff "<" ; tagout lexbuf}
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


and checklimits = parse
  "\\limits"   {true}
| "\\nolimits" {false}
| ""           {false}

and eat_delim_init = parse
| eof {raise Eof}
| ""  {eat_delim_rec lexbuf}

and eat_delim_rec = parse
| '{'
  {fun delim next i ->
    put_echo_char '{' ;
    Out.put arg_buff (if i > 0 then String.sub delim 0 i else "") ;
    Out.put_char arg_buff '{' ;
    incr brace_nesting ;
    let r = arg2 lexbuf in
    Out.put arg_buff r ;
    Out.put_char arg_buff '}' ;
    eat_delim_rec lexbuf delim next 0}
| _
  {let c = lexeme_char lexbuf 0 in
  put_echo_char c ;

  let rec kmp_char delim next i =

    if i < 0 then begin
      Out.put_char arg_buff c ;
      eat_delim_rec lexbuf delim next 0
    end else if c = delim.[i] then begin
      if i >= String.length delim - 1 then
        Out.to_string arg_buff
      else
        eat_delim_rec lexbuf delim next (i+1)
    end else begin
      if next.(i) >= 0 then
        Out.put arg_buff (String.sub delim 0 (i-next.(i))) ;
      kmp_char delim next next.(i)
    end in
  kmp_char}
|  eof
    {raise (Error ("End of file in delimited argument, read:\n\t"^
            Out.to_string echo_buff))}

and skip_delim_init = parse
| ' '|'\n' {skip_delim_init lexbuf}
| ""       {skip_delim_rec lexbuf}

and skip_delim_rec = parse
| _
  {fun delim i ->
    let c = lexeme_char lexbuf 0 in
    put_echo_char c ;
    if c <> delim.[i] then
      raise (Delim delim) ;
    if i+1 < String.length delim then
      skip_delim_rec lexbuf delim (i+1)}
|  eof
    {fun delim i ->
      raise (Error ("End of file checking delimiter ``"^delim^"''"))}

{

let init_kmp s =
  let l = String.length s in
  let r = Array.create l (-1) in  
  let rec init_rec i j =

    if i+1 < l then begin
      if j = -1 || s.[i]=s.[j] then begin
        r.(i+1) <- j+1 ;
        init_rec (i+1) (j+1)
      end else
        init_rec i r.(j)
    end in
  init_rec 0 (-1) ;
  r

let with_delim delim lexbuf =
  let next = init_kmp delim  in
  let r = eat_delim_init lexbuf delim next 0 in
  r

and skip_delim delim lexbuf =
  skip_delim_init lexbuf delim 0

let skip_blanks_init lexbuf =
  let _ = skip_blanks lexbuf in
  ()

let arg_verbatim lexbuf = match first_char lexbuf with
  | '{' ->
       incr brace_nesting ;
       arg2 lexbuf
  | c ->
      let delim = String.make 1 c in
      with_delim delim lexbuf
        
} 

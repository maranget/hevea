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
open Misc

let header = "$Id: save.mll,v 1.55 2000-06-05 08:07:32 maranget Exp $" 

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


let brace_nesting = ref 0
and arg_buff = Out.create_buff ()
and echo_buff = Out.create_buff ()
and tag_buff = Out.create_buff ()
;;

  
let echo = ref false
;;

let get_echo () = echo := false ; Out.to_string echo_buff
and start_echo () = echo := true ; Out.reset echo_buff
and stop_echo () = echo := false ; Out.reset echo_buff
;;

let empty_buffs () =
  brace_nesting := 0 ; Out.reset arg_buff ;
  echo := false ; Out.reset echo_buff ;
  Out.reset tag_buff 
;;

let error s =
  empty_buffs () ;
  raise (Error s)
;;

let my_int_of_string s =
  try int_of_string s
  with Failure "int_of_string" ->
    error ("Integer argument expected: ``"^s^"''")

exception Eof
;;
exception NoOpt
;;

let put_echo s =
  if !echo then Out.put echo_buff s
and put_echo_char c =
  if !echo then Out.put_char echo_buff c
and blit_echo lb =
  if !echo then Out.blit echo_buff lb
;;

let put_both s =
  put_echo s ; Out.put arg_buff s
;;
let blit_both lexbuf =
  blit_echo lexbuf ; Out.blit arg_buff lexbuf

let put_both_char c =
  put_echo_char c ; Out.put_char arg_buff c
    ;;

type kmp_t = Continue of int | Stop of string

let rec kmp_char delim next i c =
  if i < 0 then begin
    Out.put_char arg_buff c ;
    Continue 0
  end else if c = delim.[i] then begin
    if i >= String.length delim - 1 then
      Stop (Out.to_string arg_buff)
    else
      Continue (i+1)
  end else begin
    if next.(i) >= 0 then
      Out.put arg_buff (String.sub delim 0 (i-next.(i))) ;
    kmp_char delim next next.(i) c
  end
}
let command_name = '\\' (( ['@''A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])

  rule opt = parse
| ' '* '\n'? ' '* '['
    {put_echo (lexeme lexbuf) ;
    opt2 lexbuf}
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
      Out.to_string arg_buff
    end}
  | _
      {let s = lexeme_char lexbuf 0 in
      put_both_char s ; opt2 lexbuf }

and skip_comment = parse
  | eof       {()}
  | '\n' ' '* {()}
  | _         {skip_comment lexbuf}

and check_comment = parse
  | '%' {skip_comment lexbuf}
  | ""  {()}

and arg = parse
    ' '+ | '\n'+  {put_echo (lexeme lexbuf) ; arg lexbuf}
  | '{'
      {incr brace_nesting;
      put_echo_char '{' ;
      arg2 lexbuf}
  | '%'
     {skip_comment lexbuf  ; arg lexbuf}
  | "\\box" '\\' (['A'-'Z' 'a'-'z']+ '*'? | [^ 'A'-'Z' 'a'-'z'])
     {let lxm = lexeme lexbuf in
     put_echo lxm ;
     lxm}
  | command_name
     {blit_both lexbuf ;
     skip_blanks lexbuf}
  | '#' ['1'-'9']
     {let lxm = lexeme lexbuf in
     put_echo lxm ; lxm}
  | [^ '}']
      {let c = lexeme_char lexbuf 0 in
      put_both_char c ;
      Out.to_string arg_buff}
  | eof    {raise Eof}
  | ""     {error "Argument expected"}


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
      blit_both lexbuf ; arg2 lexbuf }
| eof
    {error "End of file in argument"}

| [^'\\''{''}']+
      {blit_both lexbuf ; arg2 lexbuf }

| _
    {let c = lexeme_char lexbuf 0 in
    put_both_char c ; arg2 lexbuf}
and csname = parse
  [' ''\n']+
    {(fun get_prim subst ->
      blit_echo lexbuf ; csname lexbuf get_prim subst)}
| '{'? "\\csname" ' '*
      {(fun get_prim subst_fun ->
        blit_echo lexbuf ;
        let r = incsname lexbuf in
        "\\"^get_prim r)}
| ""  {fun get_prim subst -> let r = arg lexbuf in subst r}

and incsname = parse
  "\\endcsname"  '}'?
    {let lxm = lexeme lexbuf in
    put_echo lxm ; Out.to_string arg_buff}
| _ 
    {put_both_char (lexeme_char lexbuf 0) ;
    incsname lexbuf}
| eof           {error "End of file in command name"}

and cite_arg = parse
  ' '* '{'   {cite_args_bis lexbuf}
| ""         {error "No opening ``{'' in citation argument"}

and cite_args_bis = parse
  [^'}'' ''\n''%'',']* {let lxm = lexeme lexbuf in lxm::cite_args_bis lexbuf}
|  '%' [^'\n']* '\n' {cite_args_bis lexbuf}
| ','         {cite_args_bis lexbuf}
| [' ''\n']+ {cite_args_bis lexbuf}
| '}'         {[]}
| ""          {error "Bad syntax for \\cite argument"}

(*
and macro_names = parse
  eof {[]}
| '\\' ((['@''A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])
  {let name = lexeme lexbuf in
  name :: macro_names lexbuf}
| _   {macro_names lexbuf}
*)

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

and get_limits = parse
  ' '+          {get_limits lexbuf}
| "\\limits"    {Some Limits}
| "\\nolimits"  {Some NoLimits}
| "\\intlimits" {Some IntLimits}
| eof           {raise Eof}
| ""            {None}

and get_sup = parse
| ' '* '^'  {try Some (arg lexbuf) with Eof -> error "End of file after ^"}
| eof       {raise Eof}
| ""        {None}


and get_sub = parse
| ' '* '_'  {try Some (arg lexbuf) with Eof -> error "End of file after _"}
| eof       {raise Eof}
| ""        {None}

and defargs = parse 
|  '#' ['1'-'9']
    {let lxm = lexeme lexbuf in
    put_echo lxm ;
    lxm::defargs lexbuf}
| [^'{'] | "\\{"
    {blit_both lexbuf ;
    let r = in_defargs lexbuf in
    r :: defargs lexbuf}
| "" {[]}

and in_defargs = parse
| "\\{" | "\\#" {blit_both lexbuf ; in_defargs lexbuf}
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
| _    {Out.blit tag_buff lexbuf ; tagout lexbuf}
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
| "\\{"
  {fun delim next i ->
    put_echo "\\{" ;
    match kmp_char delim next i '\\' with
    | Stop _ ->
        error "Delimitors cannot end with ``\\''"
    | Continue i -> match  kmp_char delim next i '{' with
      | Stop s -> s
      | Continue i ->  eat_delim_rec lexbuf delim next i}
      
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
  {fun delim next i ->
    let c = lexeme_char lexbuf 0 in
    put_echo_char c ;
    match kmp_char delim next i c with
    | Stop s -> s
    | Continue i -> eat_delim_rec lexbuf delim next i}
|  eof
    {error ("End of file in delimited argument, read:
	"^
            Out.to_string echo_buff)}

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
      error ("End of file checking delimiter ``"^delim^"''")}
and check_equal = parse
| '=' {true}
| ""  {false}
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
  check_comment lexbuf ;
  let r = eat_delim_init lexbuf delim next 0 in
  r

and skip_delim delim lexbuf =
  check_comment lexbuf ;
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

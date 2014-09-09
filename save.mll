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
open SaveUtils


let rec peek_next_char lb =
  let pos = lb.lex_curr_pos
  and len = lb.lex_buffer_len in
  if pos >= len then begin
    if lb.lex_eof_reached then
      raise Not_found
    else begin
      lb.refill_buff lb ;
      peek_next_char lb
    end
  end else
    Bytes.unsafe_get lb.lex_buffer pos

let if_next_char  c lb =
  try
     peek_next_char lb = c
  with
  | Not_found -> false


let rec if_next_string s lb =
  if s = "" then
    true
  else
    let pos = lb.lex_curr_pos
    and len = lb.lex_buffer_len
    and slen = String.length s in
    if pos + slen - 1 >= len then begin
      if lb.lex_eof_reached then begin
          false
      end else begin
          lb.refill_buff lb ;
        if_next_string s lb
      end
    end else
      let b = lb.lex_buffer in
      let rec do_rec k =
        if k >= slen then true
        else
          Bytes.get b (pos+k) = String.get s k &&
          do_rec (k+1) in
      do_rec 0
  

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
let command_name =
 '\\' (( ['@''A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'] | "\\*")
let space = [' ''\t''\r']

rule skip_comment = parse
  | eof       {()}
  | '\n' space* {check_comment lexbuf}
  | _         {skip_comment lexbuf}

and check_comment = parse
  | '%' {skip_comment lexbuf}
  | ""  {()}

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
| space* '\n' as lxm
    {seen_par := false ;
    put_echo lxm ;
    more_skip lexbuf}
| space*  as lxm
    {put_echo lxm ; Out.to_string arg_buff}

and more_skip = parse
  (space* '\n' space*)+ as lxm
   {seen_par := true ;
   put_echo lxm ;
   more_skip lexbuf}
| space* as lxm
  { put_echo lxm ; Out.to_string arg_buff}

and skip_equal = parse
    space* '='? space* {()}

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
      {blit_both lexbuf ; arg2 lexbuf }
| eof
    {error "End of file in argument"}

| [^'\\''{''}']+
      {blit_both lexbuf ; arg2 lexbuf }

| _
    {let c = lexeme_char lexbuf 0 in
    put_both_char c ; arg2 lexbuf}

and csname get_prim subst = parse
  (space|'\n')+
    { blit_echo lexbuf ; csname get_prim subst lexbuf }
| '{'? "\\csname" space*
      {blit_echo lexbuf ;
       let r = incsname lexbuf in
       "\\"^get_prim r}
| "" 
   {let r = Saver.String.arg lexbuf in
   let r = subst r in
   try
     check_csname get_prim (MyLexing.from_string r)
   with
   | Exit -> r }

and check_csname get_prim = parse
| "\\csname" space*
  { let r = incsname lexbuf in
   "\\"^get_prim r}
| command_name
| ""
   { raise Exit }

and incsname = parse
  "\\endcsname"  '}'?
    {let lxm = lexeme lexbuf in
    put_echo lxm ; Out.to_string arg_buff}
| _ 
    {put_both_char (lexeme_char lexbuf 0) ;
    incsname lexbuf}
| eof           {error "End of file in command name"}

and cite_arg = parse
| space* '{' {cite_args_bis lexbuf}
| eof        {raise Eof} 
| ""         {error "No opening ``{'' in citation argument"}

and cite_args_bis = parse
  [^'}'' ''\t''\r''\n''%'',']+
  {let lxm = lexeme lexbuf in lxm::cite_args_bis lexbuf}
|  '%' [^'\n']* '\n' {cite_args_bis lexbuf}
| ','           {cite_args_bis lexbuf}
| (space|'\n')+ {cite_args_bis lexbuf}
| '}'         {[]}
| ""          {error "Bad syntax for \\cite argument"}

and num_arg = parse
| (space|'\n')+ {(fun get_int -> num_arg lexbuf get_int)}
| ['0'-'9']+ 
    {fun _get_int ->
      let lxm = lexeme lexbuf in
      my_int_of_string lxm}
|  "'" ['0'-'7']+ 
    {fun _get_int ->let lxm = lexeme  lexbuf in
    my_int_of_string ("0o"^String.sub lxm 1 (String.length lxm-1))}
|  '"' ['0'-'9' 'a'-'f' 'A'-'F']+ (* '"' *)
    {fun _get_int ->let lxm = lexeme  lexbuf in
    my_int_of_string ("0x"^String.sub lxm 1 (String.length lxm-1))}
| '`' '\\' _
    {fun _get_int ->let c = lexeme_char lexbuf 2 in
    Char.code c}
| '`' '#' ['1'-'9']
    {fun get_int ->
      let lxm = lexeme lexbuf in
      get_int (String.sub lxm 1 2)}
| '`' _
    {fun _get_int ->let c = lexeme_char lexbuf 1 in
    Char.code c}
| ""
    {fun get_int ->
      let s = Saver.String.arg lexbuf in
      get_int s}
    

and filename = parse
  [' ''\n']+     {put_echo (lexeme lexbuf) ; filename lexbuf}
| [^'\n''{'' ']+ {let lxm = lexeme lexbuf in put_echo lxm ; lxm}
| ""             {Saver.String.arg lexbuf}  

and remain = parse
 _ * eof {Lexing.lexeme lexbuf}

and get_limits r = parse
  space+        {get_limits r lexbuf}
| "\\limits"    {get_limits (Some Limits) lexbuf}
| "\\nolimits"  {get_limits (Some NoLimits) lexbuf}
| "\\intlimits" {get_limits (Some IntLimits) lexbuf}
| eof           {raise (LimitEof r)}
| ""            {r}

and get_sup = parse
| space* '^'  {try Some (Saver.String.arg lexbuf) with Eof -> error "End of file after ^"}
| eof       {raise Eof}
| ""        {None}


and get_sub = parse
| space* '_'  {try Some (Saver.String.arg lexbuf) with Eof -> error "End of file after _"}
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
| "<br>" {Out.put_char tag_buff ' ' ; tagout lexbuf}
|  '<'  {intag lexbuf}
| "&nbsp;" {Out.put tag_buff " " ; tagout lexbuf}
| "&gt;" {Out.put tag_buff ">" ; tagout lexbuf}
| "&lt;" {Out.put tag_buff "<" ; tagout lexbuf}
| _    {Out.blit tag_buff lexbuf ; tagout lexbuf}
| eof  {Out.to_string tag_buff}

and intag = parse
  '>'  {tagout lexbuf}
| '"'  {instring lexbuf} (* '"' *)
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

and eat_delim_init delim next i = parse
| eof {raise Eof}
| '{'
    { put_echo_char '{' ;
      incr brace_nesting ;
      let r = arg2 lexbuf in
      check_comment lexbuf ;
      if if_next_string delim lexbuf then begin
        skip_delim_rec  delim 0 lexbuf ;
        r
      end else begin
        Out.put_char arg_buff '{' ;
        Out.put arg_buff r ;
        Out.put_char arg_buff '}' ;
        eat_delim_rec delim next 0 lexbuf
      end}
| ""  {eat_delim_rec  delim next i lexbuf}

and eat_delim_rec  delim next i = parse
| "\\{"
  {
    put_echo "\\{" ;
    match kmp_char delim next i '\\' with
    | Stop _ ->
        error "Delimitors cannot end with ``\\''"
    | Continue i -> match  kmp_char delim next i '{' with
      | Stop s -> s
      | Continue i ->  eat_delim_rec delim next i lexbuf}
      
| '{'
  {
    put_echo_char '{' ;
    Out.put arg_buff (if i > 0 then String.sub delim 0 i else "") ;
    Out.put_char arg_buff '{' ;
    incr brace_nesting ;
    let r = arg2 lexbuf in
    Out.put arg_buff r ;
    Out.put_char arg_buff '}' ;
    eat_delim_rec delim next 0 lexbuf
   }
| _
  {
    let c = lexeme_char lexbuf 0 in
    put_echo_char c ;
    match kmp_char delim next i c with
    | Stop s -> s
    | Continue i -> eat_delim_rec delim next i lexbuf}
|  eof
    {error
       ("End of file in delimited argument, read:\n" ^
        Out.to_string echo_buff)}

and skip_delim_init delim i = parse
| space|'\n' {skip_delim_init delim i lexbuf}
| ""       {skip_delim_rec delim i lexbuf}

and skip_delim_rec delim i = parse
| _
  {
    let c = lexeme_char lexbuf 0 in
    put_echo_char c ;
    if c <> delim.[i] then
      raise (Delim delim) ;
    if i+1 < String.length delim then
      skip_delim_rec delim (i+1) lexbuf}
|  eof
    { error ("End of file checking delimiter ``"^delim^"''")}
and check_equal = parse
| '=' {true}
| ""  {false}

and do_xyarg = parse
| [^'{']
    {let lxm = Lexing.lexeme_char lexbuf 0 in
    put_both_char lxm ;
    do_xyarg lexbuf}
| eof {raise Eof}
| ""  {Out.to_string arg_buff}

and simple_delim c = parse
| _ as x
  {if c = x then begin
    put_echo_char x ;
    Out.to_string arg_buff
  end else begin
    put_both_char x ;
    simple_delim c lexbuf
  end
  } 
| eof
  {error (Printf.sprintf "End of file in simple delim '%c'" c)}

and gobble_one_char = parse 
| _   {()}
| ""  {fatal ("Gobble at end of file")}


{

let arg = Saver.String.arg
let arg_list = Saver.List.arg
let opt = Saver.String.opt
let opt_list = Saver.List.opt
let start_echo = SaveUtils.start_echo
let get_echo = SaveUtils.get_echo
exception NoOpt =  SaveUtils.NoOpt
exception LimitEof = SaveUtils.LimitEof
exception Eof =  SaveUtils.Eof
let seen_par = SaveUtils.seen_par
let set_verbose = SaveUtils.set_verbose
let empty_buffs = SaveUtils.empty_buffs
exception Delim = SaveUtils.Delim
exception Error = SaveUtils.Error

let init_kmp s =
  let l = String.length s in
  let r = Array.make l (-1) in  
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
  let r = eat_delim_init delim next 0 lexbuf in
  r

and skip_delim delim lexbuf =
  check_comment lexbuf ;
  skip_delim_init delim 0 lexbuf

let skip_blanks_init lexbuf =
  let _ = skip_blanks lexbuf in
  ()

let arg_verbatim lexbuf =
  ignore (skip_blanks lexbuf) ;
  match first_char lexbuf with
  | '{' ->
       incr brace_nesting ;
       arg2 lexbuf
  | c -> simple_delim c lexbuf


let xy_arg lexbuf = do_xyarg lexbuf
} 

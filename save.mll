{
open Lexing

let silent = ref false
;;

type formatopt = Wrap | NoMath
type format = Align of string * formatopt list | Inside of string
;;

let border = ref false

let brace_nesting = ref 0
and arg_buff = Out.create_buff ()
and delim_buff = Out.create_buff ()
and tag_buff = Out.create_buff ()
;;


exception BadParse of string
;;
exception EofDelim of int
;;
exception NoDelim
;;
exception NoOpt
;;
}

rule opt = parse
   '['
        {incr brace_nesting ; opt2 lexbuf}
| ' '+  {opt lexbuf}
|  eof  {raise (BadParse "EOF")}
|  ""   {raise NoOpt}


and opt2 =  parse
    '['         {  incr brace_nesting;
                   if !brace_nesting > 1 then begin
                     Out.put arg_buff "[" ; opt2 lexbuf
                   end else opt2 lexbuf}
  | ']'        { decr brace_nesting;
                 if !brace_nesting > 0 then begin
                    Out.put arg_buff "]" ; opt2 lexbuf
                 end else Out.to_string arg_buff}
  | _
      {let s = lexeme_char lexbuf 0 in
      Out.put_char arg_buff s ; opt2 lexbuf }

and arg = parse
    ' '* '\n'? ' '* {arg lexbuf}
  | '{'
      {incr brace_nesting;
      arg2 lexbuf}
(*  | '='?'-'?(['0'-'9']*'.')?(['0'-'9']+|'"'['A'-'E''0'-'9']+)
    ("pt"|"cm"|"in"|"em"|"ex")?
            {lexeme lexbuf}
*)
  | '%' [^'\n']* '\n'
            {arg lexbuf}
  | "\\box" '\\' (['A'-'Z' 'a'-'z']+ '*'? | [^ 'A'-'Z' 'a'-'z'])
            {lexeme lexbuf}
  | "`\\" [^'A'-'Z' 'a'-'z']
      {lexeme lexbuf}
  | '\\' ((['A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])
            {lexeme lexbuf}
  | [^ '}'] {String.make 1 (lexeme_char lexbuf 0)}
  | eof    {raise (BadParse "EOF")}
  | ""     {raise (BadParse "Empty Arg")}

and sarg = parse
  [^'{'] {lexeme lexbuf}
| ""     {arg lexbuf}

and arg2 = parse
    '{'         {  incr brace_nesting;
                   if !brace_nesting > 1 then begin
                     Out.put arg_buff "{" ; arg2 lexbuf
                   end else arg2 lexbuf}
  | '}'        { decr brace_nesting;
                 if !brace_nesting > 0 then begin
                    Out.put arg_buff "}" ; arg2 lexbuf
                 end else Out.to_string arg_buff}
  | "\\{" | "\\}" | "\\\\"
      {let s = lexeme lexbuf in
      Out.put arg_buff s ; arg2 lexbuf }
  | eof    {raise (BadParse "EOF")}
  | _
      {let s = lexeme_char lexbuf 0 in
      Out.put_char arg_buff s ; arg2 lexbuf }

and csname = parse
  [' ''\n']+ {csname lexbuf}
| '{'? "\\csname" ' '+
    {Out.put_char arg_buff '\\' ; incsname lexbuf}
| ""         {arg lexbuf}

and incsname = parse
  "\\endcsname"  '}'? {Out.to_string arg_buff}
| _ 
    {Out.put_char arg_buff (lexeme_char lexbuf 0) ;
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
  [^'}'',']* {let lxm = lexeme lexbuf in lxm::cite_args_bis lexbuf}
| ','        {cite_args_bis lexbuf}
| '}'        {[]}

and macro_names = parse
  eof {[]}
| '\\' ((['A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])
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
  [' ''\n'] {input_arg lexbuf}
| [^'\n''{'' ']+ {lexeme lexbuf}
| "" {arg lexbuf}  

and tformat = parse
  'c' {Align ("center",[])::tformat lexbuf}
| 'l' {Align ("left",[])::tformat lexbuf}
| 'r' {Align ("right",[])::tformat lexbuf}
| 'p'
   {let _ = arg lexbuf in
   if not !silent then begin
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

and skip_blanks = parse 
  [' ']* '\n'? [' ']* {()}
| eof {()}

and skip_equal = parse
  ' '* '=' ' '* {()}
| ""            {()}

and get_sup_sub = parse
  '^'
    {let sup = sarg lexbuf in
    sup,get_sub lexbuf}
| '_'
    {let sub = sarg lexbuf in
    get_sup lexbuf,sub}
| "" {("","")}

and get_sup = parse
  '^'  {sarg lexbuf}
| ""   {""}

and get_sub = parse
  '_'  {sarg lexbuf}
| ""   {""}

and defargs = parse 
  '#' ['1'-'9'] | [^'#' '{']+
    {let lxm = lexeme lexbuf in
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
(*  
and do_arg_delim = parse
  _
   {fun delim i inarg ->
     let lxm = lexeme_char 0 lexbuf
     and c = String.get delim i in
     if inarg then begin
       if c <> lxm then begin
         Out.put_char arg_buff c ;
         do_arg_delim lexbuf delim i true
       end else begin
         if i+1 >= String.length delim then
           Out.to_string arg_buff
         else begin
           begin try
             do_arg_delim lexbuf delim (i+1) false
           with NoDelim ->
             let del = Out.to_string delim_buff in
             Out.put_char arg_buff (String.get del 0) ;
             let lexer =
               Lexing.from_string
                 (String.sub del 1 (String.length del-1) in
             begin try
               do_arg_delim delim 0 lexer
             with
               EofDelim i -> do_arg_delim delim i lexbuf
             | NoDelim ->
                
      |  
         
       end
     end}
     

*)

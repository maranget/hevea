{
open Lexing

let brace_nesting = ref 0
and arg_buff = Out.create_buff ()
and delim_buff = Out.create_buff ()
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
|  ' '+ {opt lexbuf}
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
    [' ''\n']+ {arg lexbuf}
  | '{'
      {incr brace_nesting;
      arg2 lexbuf}
  | '='?'-'?(['0'-'9']*'.')?(['0'-'9']+|'"'['A'-'E''0'-'9']+)
    ("pt"|"cm"|"in"|"em"|"ex")?
            {lexeme lexbuf}
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

and cite_args = parse
  [^'}'',']* {let lxm = lexeme lexbuf in lxm::cite_args lexbuf}
| ','        {cite_args lexbuf}
| '}'        {[]}

and num_arg = parse
   ['0'-'9']+ 
    {let lxm = lexeme lexbuf in
    int_of_string lxm}
|  "'" ['0'-'7']+ 
    {let lxm = lexeme  lexbuf in
    int_of_string ("0o"^String.sub lxm 1 (String.length lxm-1))}
|  '"' ['0'-'9' 'a'-'f' 'A'-'F']+ 
    {let lxm = lexeme  lexbuf in
    int_of_string ("0x"^String.sub lxm 1 (String.length lxm-1))}
| '`' _
    {let c = lexeme_char lexbuf 1 in
    Char.code c}
| "" {failwith "num_arg"}

and input_arg = parse
  [' ''\n'] {input_arg lexbuf}
| [^'\n''{'' ']+ {lexeme lexbuf}
| "" {arg lexbuf}  
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

{
exception VError of string

module type S = sig val init : unit -> unit end
;;
module MakeAlso (Dest : OutManager.S) (Image : ImageManager.S) (Scan : Latexscan.S) : S =
struct
open Misc
open Lexing
open Lexstate
open Latexmacros
open Stack

exception EndAlltt
exception Eof of string
;;

let verb_delim = ref (Char.chr 0)
let env_extract s =
  let i = String.index s '{'
  and j = String.rindex s '}' in
  String.sub s (i+1) (j-i-1)

} 

rule inverb = parse
|  _
  {let c = lexeme_char lexbuf 0 in
  if c = !verb_delim then begin
    Dest.close_group () ;
    Scan.close_env "*verb"
  end else begin
    Dest.put (Dest.iso c) ;
    inverb lexbuf
  end}
| eof {raise
        (Fatal ("End of file inside ``\\verb"^
                String.make 1 !verb_delim^"'' construct"))}

and start_inverb = parse
| _
  {let c = lexeme_char lexbuf 0 in
  verb_delim := c ;
  inverb lexbuf}
| eof
    {if not (empty stack_lexbuf) then
      let lexbuf = previous_lexbuf () in
      start_inverb lexbuf
    else
      raise (VError ("End of file after \\verb"))}

and verbenv = parse
  "\\end" " " * "{" ['a'-'z'] + "}"
    {let lxm = lexeme lexbuf in
    let env = env_extract lxm in
    if env = !Scan.cur_env then begin
      Scan.top_close_block "PRE" ;
      Scan.close_env env
    end else begin
      Dest.put lxm ;
      verbenv lexbuf
    end}
| "\\esc" ' '*
    {if !Scan.cur_env <> "program" then begin
      Dest.put (lexeme lexbuf)
    end else begin
      let arg = save_arg lexbuf in
      scan_this Scan.main ("{"^arg^"}")
    end ;
    verbenv lexbuf}
| '\t'
      {for i=1 to !tab_val do
        Dest.put_char ' '
      done ;
      verbenv lexbuf}
| _   { Dest.put (Dest.iso (lexeme_char lexbuf 0)) ; verbenv lexbuf}
| eof
    {raise
        (Eof
           ("End of file inside ``"^ !Scan.cur_env^"'' environment"))}

and rawhtml = parse
|   "\\end" " " * "{" ['a'-'z'] + "}"
    {let lxm = lexeme lexbuf in
    let env = env_extract lxm in
    if env = !Scan.cur_env then begin
(*      Dest.close_block "TEMP";*)
      Scan.close_env env
    end else begin
      Dest.put lxm ;
      rawhtml lexbuf
    end}
| _   {Dest.put_char(lexeme_char lexbuf 0); rawhtml lexbuf}
| eof {raise (VError ("End of file inside ``rawhtml'' environment"))}

and verblatex = parse
| "\\end" " " * "{" ['a'-'z'] + "}"
    {let lxm = lexeme lexbuf in
    let env = env_extract lxm in
    if env = !Scan.cur_env then begin
      Scan.close_env env
    end else      
      verblatex lexbuf}
|  _ 
    {verblatex lexbuf}
|  eof {raise (VError ("End of file inside ``verblatex'' environment"))}

and verbimage = parse
|  "\\end"
    {let lxm = lexeme lexbuf in
    Save.start_echo () ;
    let env = save_arg lexbuf in
    let true_env = Save.get_echo () in
    if env = "verbimage" then begin
      Scan.close_env env ;
      Image.put_char '\n'
    end else begin
      Image.put lxm ;
      Image.put true_env ;
      verbimage lexbuf
    end}
|  _
    {let lxm = lexeme_char lexbuf 0 in
    Image.put_char lxm ;
    verbimage lexbuf}
|  eof {raise (VError "End of file in ``verbimage'' environment")}

and latex2html_latexonly = parse
| '%' + [ ' ' '\t' ] * "\\end{latexonly}" [ ^ '\n' ] * '\n'
    { () }
| _ 
    {latex2html_latexonly lexbuf}
| eof
    {raise (VError ("End of file inside ``latexonly'' environment"))}
{

let open_verb lexbuf =
  Dest.open_group "CODE" ;
  Scan.new_env "*verb" ;
  start_inverb lexbuf

let open_verbenv lexbuf =
  Scan.top_close_block "" ;
  Scan.top_open_block "PRE" "" ;
  let lexbuf = previous_lexbuf () in
  try verbenv lexbuf
  with Eof s -> raise (VError s)
  

let open_rawhtml lexbuf =
  Scan.top_close_block "" ;
  let lexbuf = previous_lexbuf () in
  begin match !Parse_opts.destination with
  | Parse_opts.Html -> rawhtml lexbuf 
  | _ -> begin
      Misc.warning "rawhtml detected";
      rawhtml lexbuf
  end;
  end
 (* Dest.open_block "TEMP" "";*)

let open_verblatex lexbuf =
  Scan.top_close_block "" ;
  let lexbuf = previous_lexbuf () in
  verblatex lexbuf 

let open_verbimage lexbuf =
  Scan.top_close_block "" ;
  let lexbuf = previous_lexbuf () in
  verbimage lexbuf 
  
let init () =
  def_code "\\verb" open_verb ;
  def_code "\\verb*" open_verb ;
  def_code "\\prog" open_verb ;
  def_code "\\verbatim" open_verbenv ;
  def_code "\\program" open_verbenv ;
  def_code "\\rawhtml" open_rawhtml ;
  def_code "\\verblatex" open_verblatex ;
  def_code "\\verbimage" open_verbimage ;
  def_code "\\verbatiminput"
    (fun lexbuf ->
      let tabs = Get.get_int (save_opt "8" lexbuf) in      
      let arg = save_arg lexbuf in
      let old_tabs = !tab_val in
      tab_val := tabs ;
      Scan.top_open_block "PRE" "" ;
      begin try
        input_file !verbose verbenv arg ;
      with
      | Eof _ -> ()
      | Myfiles.Except | Myfiles.Error _ -> ()
      end ;
      Scan.top_close_block "PRE" ;
      tab_val := old_tabs)
;;

end
} 








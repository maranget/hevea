{
exception Error of string

module type S = sig val init : unit -> unit end
;;
module MakeAlso (Html : OutManager.S) (Scan : Latexscan.S) : S =
struct
open Misc
open Lexing
open Lexstate
open Latexmacros

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
    Html.close_group () ;
    Scan.close_env "*verb"
  end else begin
    Html.put (Html.iso c) ;
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
      raise (Error ("End of file after \\verb"))}

and verbenv = parse
  "\\end" " " * "{" ['a'-'z'] + "}"
    {let lxm = lexeme lexbuf in
    let env = env_extract lxm in
    if env = !Scan.cur_env then begin
      Scan.top_close_block "PRE" ;
      Scan.close_env env
    end else begin
      Html.put lxm ;
      verbenv lexbuf
    end}
| "\\esc" ' '*
    {if !Scan.cur_env <> "program" then begin
      Html.put (lexeme lexbuf)
    end else begin
      let arg = save_arg lexbuf in
      scan_this Scan.main ("{"^arg^"}")
    end ;
    verbenv lexbuf}
| '\t'
      {for i=1 to !tab_val do
        Html.put_char ' '
      done ;
      verbenv lexbuf}
| _   { Html.put (Html.iso (lexeme_char lexbuf 0)) ; verbenv lexbuf}
| eof
    {raise
        (Error
           ("End of file inside ``"^ !Scan.cur_env^"'' environment"))}

and rawhtml = parse
|   "\\end" " " * "{" ['a'-'z'] + "}"
    {let lxm = lexeme lexbuf in
    let env = env_extract lxm in
    if env = !Scan.cur_env then begin
      Scan.close_env env
    end else begin
      Html.put lxm ;
      rawhtml lexbuf
    end}
| _   {Html.put_char(lexeme_char lexbuf 0); rawhtml lexbuf}
| eof {raise (Error ("End of file inside ``rawhtml'' environment"))}

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
|  eof {raise (Error ("End of file inside ``verblatex'' environment"))}

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
|  eof {raise (Error "End of file in ``verbimage'' environment")}

and latex2html_latexonly = parse
| '%' + [ ' ' '\t' ] * "\\end{latexonly}" [ ^ '\n' ] * '\n'
    { () }
| _ 
    {latex2html_latexonly lexbuf}
| eof
    {raise (Error ("End of file inside ``latexonly'' environment"))}

{

let open_verb lexbuf _ =
  Html.open_group "CODE" ;
  Scan.new_env "*verb" ;
  start_inverb lexbuf

let open_verbenv lexbuf _ =
  Scan.top_close_block "" ;
  Scan.top_open_block "PRE" "" ;
  let lexbuf = previous_lexbuf () in
  verbenv lexbuf 

let open_rawhtml lexbuf _ =
  Scan.top_close_block "" ;
  let lexbuf = previous_lexbuf () in
  rawhtml lexbuf 

let open_verblatex lexbuf _ =
  Scan.top_close_block "" ;
  let lexbuf = previous_lexbuf () in
  verblatex lexbuf 

let open_verbimage lexbuf _ =
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
    (fun lexbuf lxm ->
      let tabs = Get.get_int (save_opt "8" lexbuf) in      
      let arg = save_arg lexbuf in
      let old_tabs = !tab_val in
      tab_val := tabs ;
      Scan.top_open_block "PRE" "" ;
      begin try
        input_file !verbose verbenv arg ;
      with
        Myfiles.Except | Myfiles.Error _ -> ()
      end ;
      Scan.top_close_block "PRE" ;
      tab_val := old_tabs)
;;

end
} 

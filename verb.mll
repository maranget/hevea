{
module type S = sig val init : unit -> unit end
;;
module MakeAlso (Html : OutManager.S) (Scan : Latexscan.S) : S =
struct
open Misc
open Lexing
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

and set_delim = parse
| _
  {let c = lexeme_char lexbuf 0 in
  verb_delim := c}
| "" {raise (Misc.Fatal ("End of file after \\verb"))}

and verbenv = parse
  "\\end" " " * "{" ['a'-'z'] + "}"
    {let lxm = lexeme lexbuf in
    let env = env_extract lxm in
    if env = !Scan.cur_env then begin
      Scan.top_close_block "PRE" ;
      Scan.close_env env ;
      Scan.top_close_block "" ;
      Lexstate.eat_space := false
    end else begin
      Html.put lxm ;
      verbenv lexbuf
    end}
| "\\esc" ' '*
    {if !Scan.cur_env <> "program" then begin
      Html.put (lexeme lexbuf)
    end else begin
      let arg = Lexstate.save_arg lexbuf in
      Lexstate.scan_this Scan.main ("{"^arg^"}")
    end ;
    verbenv lexbuf}
| '\t'
      {for i=1 to !Lexstate.tab_val do
        Html.put_char ' '
      done ;
      verbenv lexbuf}
| eof {()}
| _   { Html.put (Html.iso (lexeme_char lexbuf 0)) ; verbenv lexbuf}

{

let open_verb lexbuf _ =
  set_delim lexbuf ;
  Html.open_group "CODE" ;
  Scan.new_env "*verb" ;
  inverb lexbuf

let open_verbenv lexbuf _ =
  Scan.top_open_block "PRE" "" ;
  let lexbuf = Lexstate.previous_lexbuf () in
  verbenv lexbuf 

let init () =
  def_code "\\verb" open_verb ;
  def_code "\\verb*" open_verb ;
  def_code "\\prog" open_verb ;
  def_code "\\verbatim" open_verbenv ;
  def_code "\\program" open_verbenv ;
  def_code "\\verbatiminput"
    (fun lexbuf lxm ->
      let tabs = Get.get_int (Lexstate.save_opt "8" lexbuf) in      
      let arg = Lexstate.save_arg lexbuf in
      let old_tabs = !Lexstate.tab_val in
      Lexstate.tab_val := tabs ;
      Scan.top_open_block "PRE" "" ;
      begin try
        Lexstate.input_file !verbose verbenv arg ;
      with
        Myfiles.Except | Myfiles.Error _ -> ()
      end ;
      Scan.top_close_block "PRE" ;
      Lexstate.tab_val := old_tabs)
;;

end
} 

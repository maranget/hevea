{
exception VError of string

module type S = sig  end
;;
module Make
  (Dest : OutManager.S) (Image : ImageManager.S)
  (Scan : Latexscan.S) : S =
struct
open Misc
open Lexing
open Lexstate
open Latexmacros
open Stack

exception Eof of string
;;

let verb_delim = ref (Char.chr 0)
and line_buff = Out.create_buff ()
and process = ref (fun () -> ())
and finish = ref (fun () -> ())
;;

let env_extract s =
  let i = String.index s '{'
  and j = String.rindex s '}' in
  String.sub s (i+1) (j-i-1)

} 

rule inverb = parse
|  _
    {(fun put -> let c = lexeme_char lexbuf 0 in
    if c = !verb_delim then begin
      Dest.close_group () ;
    end else begin
      put c ;
      inverb lexbuf put
    end)}
| eof
    {(fun put -> if not (empty stack_lexbuf) then
      let lexbuf = previous_lexbuf () in
      inverb lexbuf put
    else
      raise (VError ("End of file after \\verb")))}

and start_inverb = parse
| _
    {(fun put -> let c = lexeme_char lexbuf 0 in
    verb_delim := c ;
    inverb lexbuf put)}
| eof
    {(fun put ->
      if not (empty stack_lexbuf) then
        let lexbuf = previous_lexbuf () in
        start_inverb lexbuf put
      else
        raise (VError ("End of file after \\verb")))}

and scan_byline = parse
    "\\end" [' ''\t']* '{' [^'}']+ '}'
    {let lxm = lexeme lexbuf in
    let env = env_extract lxm in
    if env = !Scan.cur_env then begin
      !finish () ;
      scan_this Scan.main ("\\end"^env) ;
      Scan.top_close_block "" ;
      Scan.close_env !Scan.cur_env ;
      Scan.check_alltt_skip lexbuf
    end else begin
      Out.put line_buff lxm ;
      scan_byline lexbuf
    end}
| '\n'
    {!process () ; scan_byline lexbuf}
| _ 
    {let lxm = lexeme_char lexbuf 0 in
    Out.put_char line_buff lxm ;
    scan_byline lexbuf}
| eof
    {if not (Stack.empty stack_lexbuf) then begin
      let lexbuf = previous_lexbuf () in
      scan_byline lexbuf
    end else
      raise
        (Eof "scan_byline")}


{
let _ = ()
;;
let put_char_star = function
  | ' '|'\t' -> Dest.put_char '_' ;
  | c ->  Dest.put (Dest.iso c)
and put_char = function
  |  '\t' -> Dest.put_char ' '
  | c -> Dest.put (Dest.iso c)
;;


let open_verb put lexbuf =
  Dest.open_group "CODE" ;
  start_inverb lexbuf put
;;
  
def_code "\\verb" (open_verb (fun c -> Dest.put (Dest.iso c)));
def_code "\\verb*" (open_verb put_char_star);
();;

let put_line_buff_verb () =
  Out.iter put_char line_buff ;
  Out.reset line_buff

and put_line_buff_verb_star () =
  Out.iter put_char_star line_buff ;
  Out.reset line_buff  
;;

let noeof lexbuf =
  try scan_byline lexbuf
  with Eof s ->
    raise (VError ("End of file in environment: ``"^ !Scan.cur_env^"''"))

and verb_input file =
  let save = !Scan.cur_env in
  Scan.cur_env := "*verb-file*" ;
  begin try
    input_file !verbose
      (fun lexbuf ->
        try scan_byline lexbuf with Eof _ -> raise Misc.EndInput)
      file
  with
  | Myfiles.Except -> ()
  end ;
  !finish () ;
   Scan.cur_env := save

let open_verbenv star =
  Scan.top_open_block "PRE" "" ;
  process :=
     if star then
       (fun () -> put_line_buff_verb_star () ; Dest.put_char '\n')
     else
       (fun () -> put_line_buff_verb () ; Dest.put_char '\n') ;
  finish :=
     if star then
       put_line_buff_verb_star
     else put_line_buff_verb

and close_verbenv _ = Scan.top_close_block "PRE"

let put_html () =
  Out.iter (fun c -> Dest.put_char c) line_buff ;
  Out.reset line_buff
;;

let open_rawhtml lexbuf =
  begin match !Parse_opts.destination with
    | Parse_opts.Html -> ()
    | _ ->  Misc.warning "rawhtml detected"
  end ;
  process :=
     (fun () -> put_html () ; Dest.put_char '\n') ;
  finish := put_html ;
  noeof lexbuf

and close_rawhtml _ = ()

let open_forget lexbuf =
  process := (fun () -> Out.reset line_buff) ;
  finish := (fun () -> Out.reset line_buff) ;
  noeof lexbuf

and close_forget _ = ()

let open_tofile chan lexbuf =
  process :=
     (fun () ->
       output_string chan (Out.to_string line_buff) ;
       output_char chan '\n') ;
  finish :=
     (fun () ->
       output_string chan (Out.to_string line_buff) ;
       close_out chan) ;
  noeof lexbuf

and close_tofile lexbuf = ()


let put_line_buff_image () =
  Out.iter (fun c -> Image.put_char c) line_buff ;
  Out.reset line_buff

let open_verbimage lexbuf =
  process := (fun () -> put_line_buff_image () ; Image.put_char '\n') ;
  finish := put_line_buff_image ;
  noeof lexbuf

and close_verbimage _ = ()
;;

let def_code name f =
  Latexmacros.def_code name f ;
  Scan.macro_register name

and redef_code name f =
  Latexmacros.redef_code name f ;
  Scan.macro_register name
;;

def_code "\\verbatim"
    (fun lexbuf ->
      open_verbenv false ;
      let first = Save.rest lexbuf in
      scan_this Scan.main first ;
      noeof lexbuf) ;
def_code "\\endverbatim" close_verbenv ;


def_code "\\verbatim*"
    (fun lexbuf ->
      open_verbenv true ;
      let first = Save.rest lexbuf in
      scan_this Scan.main first ;
      noeof lexbuf) ;        
def_code "\\endverbatim*" close_verbenv ;

def_code "\\rawhtml" open_rawhtml ;
def_code "\\endrawhtml" close_forget ;
def_code "\\verblatex" open_forget ; 
def_code "\\endverblatex" Scan.check_alltt_skip ;
def_code "\\verbimage" open_verbimage ; 
def_code "\\endverbimage" Scan.check_alltt_skip ;
()
;;

let init_verbatim () =

def_code "\\verbatiminput"
    (fun lexbuf ->
      let name = Scan.get_prim_arg lexbuf in
      open_verbenv false ;
      verb_input name ;
      close_verbenv lexbuf) ;
def_code "\\verbatiminput*"
    (fun lexbuf ->
      let name = Scan.get_prim_arg lexbuf in
      open_verbenv true ;
      verb_input name ;
      close_verbenv lexbuf) ;
(* comment clashes with the ``comment'' package *)
silent_def "\\comment"  0 (CamlCode open_forget) ;
silent_def "\\endcomment" 0 (CamlCode Scan.check_alltt_skip) ;
()
;;

register_init "verbatim" init_verbatim 
;;


(* The moreverb package *)
let tab_val = ref 8

let put_verb_tabs () =
  let char = ref 0 in
  Out.iter
    (fun c -> match c with
| '\t' ->
  let limit = !tab_val - !char mod !tab_val in
  for j = 1 to limit do
    Dest.put_char ' ' ; incr char
  done ;  
  | c -> Dest.put (Dest.iso c) ; incr char)
  line_buff ;
  Out.reset line_buff

let open_verbenv_tabs () =
  Scan.top_open_block "PRE" "" ;
  process := (fun () -> put_verb_tabs () ; Dest.put_char '\n') ;
  finish := put_verb_tabs 

and close_verbenv_tabs lexbuf =
  Scan.top_close_block "PRE" ;
  Scan.check_alltt_skip lexbuf
;;

let line = ref 0
and interval = ref 1
;;


let output_line inter_arg star =
  if !line = 1 || !line mod inter_arg = 0 then
    scan_this Scan.main ("\\listinglabel{"^string_of_int !line^"}")
  else
    Dest.put "     " ;
  if star then
    put_line_buff_verb_star ()
  else
    put_verb_tabs () ;
  incr line


let open_listing start_arg inter_arg star =
  Scan.top_open_block "PRE" "" ;
  line := start_arg ;
  let first_line = ref true in
  let inter = if inter_arg <= 0 then 1 else inter_arg in
  process := 
    (fun () ->
      if !first_line then begin
        first_line := false ;
        if not (Out.is_empty line_buff) then
          output_line inter_arg star ;        
      end else
        output_line inter_arg star  ;
      Dest.put_char '\n') ;
  finish :=
     (fun () ->
       if not (Out.is_empty line_buff) then
         output_line inter_arg star)

and close_listing lexbuf =
  Scan.top_close_block "PRE" ;
  Scan.check_alltt_skip lexbuf
;;


register_init "moreverb"
(fun () ->
  def_code "\\verbatimwrite"
    (fun lexbuf ->
      let name = Scan.get_prim_arg lexbuf in
      Scan.check_alltt_skip lexbuf ;
      let chan = open_out name in
      open_tofile chan lexbuf) ;

  def_code "\\endverbatimwrite" Scan.check_alltt_skip ;
    
  def_code "\\verbatimtab"
    (fun lexbuf ->
      let opt = Get.get_int (save_opt "\\verbatimtabsize" lexbuf) in
      tab_val := opt ;
      open_verbenv_tabs () ;
      let first = Save.rest lexbuf in
      scan_this Scan.main first ;
      noeof lexbuf) ;
  def_code "\\endverbatimtab" close_verbenv_tabs ;
  def_code "\\verbatimtabinput"
    (fun lexbuf ->
      let opt = Get.get_int (save_opt "\\verbatimtabsize" lexbuf) in
      tab_val := opt ;
      let name = Scan.get_prim_arg lexbuf in
      open_verbenv_tabs () ;
      verb_input name ;
      close_verbenv_tabs lexbuf) ;
  def_code "\\listinglabel"
    (fun lexbuf ->
      let arg = Get.get_int (save_arg lexbuf) in
      Dest.put (Printf.sprintf "%4d " arg)) ;

  def_code "\\listing"
    (fun lexbuf ->
      let inter = Get.get_int (save_opt "1" lexbuf) in
      let start = Get.get_int (save_arg lexbuf) in
      interval := inter ;
      open_listing start inter false ;
      noeof lexbuf) ;
  def_code "\\endlisting" close_listing ;

  def_code "\\listinginput"
    (fun lexbuf ->
      let inter = Get.get_int (save_opt "1" lexbuf) in
      let start = Get.get_int (save_arg lexbuf) in
      let name = Scan.get_prim_arg lexbuf in
      interval := inter  ;
      open_listing start inter false ;
      verb_input name ;
      close_listing lexbuf) ;

  def_code "\\listingcont"
    (fun lexbuf ->
      open_listing !line !interval false ;
      noeof lexbuf) ;
  def_code "\\endlistingcont" close_listing ;

  def_code "\\listing*"
    (fun lexbuf ->
      let inter = Get.get_int (save_opt "1" lexbuf) in
      let start = Get.get_int (save_arg lexbuf) in
      interval := inter ;
      open_listing start inter true ;
      noeof lexbuf) ;
  def_code "\\endlisting*" close_listing ;

  def_code "\\listingcont*"
    (fun lexbuf ->
      Scan.check_alltt_skip lexbuf ;
      open_listing !line !interval false ;
      noeof lexbuf) ;
  def_code "\\endlistingcont*" close_listing ;
  ())

(* The comment package *)

let init_comment () =
  def_code "\\@excludecomment" open_forget ;
  def_code "\\end@excludecomment"  Scan.check_alltt_skip ;
;;

register_init "comment" init_comment      
;;    

end
} 

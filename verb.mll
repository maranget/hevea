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
open Scan
open Subst

exception Eof of string
;;

(* For scanning by line *)
let verb_delim = ref (Char.chr 0)
and line_buff = Out.create_buff ()
and process = ref (fun () -> ())
and finish = ref (fun () -> ())
;;

let env_extract s =
  let i = String.index s '{'
  and j = String.rindex s '}' in
  String.sub s (i+1) (j-i-1)

and newlines_extract s =
  let rec do_rec i =
    if i < String.length s then begin
      if s.[i] = '\n' then
        1+do_rec (i+1)
      else
        0
    end else
      0 in
  do_rec 0

(* For scanning the ``listings'' way *)

let lst_process_error _ lxm =
   warning ("listings, unknown character: ``"^Char.escaped lxm^"''")

let lst_char_table = Array.create 256 lst_process_error
;;

let lst_init_char c f =
  lst_char_table.(Char.code c) <- f

let lst_init_chars s f =
  let last = String.length s - 1 in
  for i = 0 to last do
    lst_char_table.(Char.code s.[i]) <- f
  done

let lst_init_save_char c f =
    let old = lst_char_table.(Char.code c) in
    lst_char_table.(Char.code c) <- f old

let lst_init_save_chars s f =
  let last = String.length s - 1 in
  for i = 0 to last do
    lst_init_save_char s.[i] f
  done

(* Output functions *)
let lst_gobble  = ref 0
and lst_nchars  = ref 0
and lst_nlines  = ref 0
and lst_first   = ref 1
and lst_last    = ref 9999
and lst_print   = ref true
and lst_extended = ref false

let lst_buff = Out.create_buff ()

let lst_last_char = ref ' '
and lst_finish_comment = ref 0

let lst_put c =
  lst_last_char := c ;
  Out.put_char lst_buff c

and lst_direct_put c =
  lst_last_char := c ;
  Dest.put_char c

type lst_scan_mode = Letter | Other | Empty
let lst_scan_mode = ref Empty

type comment_type =
  | Nested of int
  | Balanced of (char -> string -> bool)
  | Line

type lst_top_mode =
  | Skip
  | String of char | Escape | Normal | Comment of comment_type
  | Delim of int * (char * (Lexing.lexbuf -> char -> unit)) list

let lst_top_mode = ref Skip


let lst_ptok s =  prerr_endline (s^": "^Out.to_string lst_buff)

let dest_string s =
  for i = 0 to String.length s - 1 do
    Dest.put (Dest.iso s.[i])
  done

(* Keywords *)

let def_print name s =
  Latexmacros.def name zero_pat
    (CamlCode (fun _ -> dest_string s))
;;

let lst_output_other () =
  if not (Out.is_empty lst_buff) then begin
    match !lst_top_mode with
    | Normal ->
        let arg = Out.to_string lst_buff in
        def_print "\@tmp@lst" arg ;
        scan_this Scan.main ("{\lst@output@other{\@tmp@lst}}")
    | _ ->
        dest_string (Out.to_string lst_buff)
  end


and lst_output_letter () =
  if not (Out.is_empty lst_buff) then begin
    match !lst_top_mode with
    | Normal ->
        let arg = Out.to_string lst_buff in
        def_print "\\@tmp@lst" arg ;
        scan_this Scan.main ("{\lst@output{\@tmp@lst}}")
    | _ ->
        dest_string (Out.to_string lst_buff)
  end

let lst_output_token () = match !lst_scan_mode with
| Letter -> lst_output_letter ()
| Other  -> lst_output_other ()
| Empty  -> ()


let lst_finalize () =
  lst_output_token () ;
  Dest.put_char '\n'


(* Process functions *)
let rec lst_process_newline lb c = match !lst_top_mode with
| Skip ->
    if !lst_nlines = !lst_first - 1 then begin
      lst_top_mode := Normal ;
      lst_process_newline lb c
    end else
      incr lst_nlines
| Comment Line ->
    lst_output_token () ;
    scan_this Scan.main "}" ;
    lst_top_mode := Normal ;
    lst_process_newline lb c
| _    ->
    scan_this Scan.main "\\lsthk@InitVarEOL\\lsthk@EOL" ;
    lst_output_token () ;
    lst_scan_mode := Empty ;
    Dest.put_char '\n' ;
    incr lst_nlines ;  
    if !lst_nlines <= !lst_last then begin
      scan_this Scan.main
        "\\lsthk@InitVarBOL\\lsthk@EveryLine\\lst@doindent\\lst@linenumber"
    end else
      lst_top_mode := Skip


let lst_process_letter _ lxm = match !lst_top_mode with
| Skip -> ()
| _ -> match !lst_scan_mode with
  | Letter -> lst_put lxm
  | Empty ->
      lst_scan_mode := Letter ;
      lst_put lxm
  | Other  ->
      lst_output_other () ;
      lst_scan_mode := Letter ;
      lst_put lxm

let lst_process_digit _ lxm = match !lst_top_mode with
| Skip -> ()
| _ ->  match !lst_scan_mode with
  | Letter|Other -> lst_put lxm
  | Empty  ->
      lst_scan_mode := Other ;
      lst_put lxm

let lst_process_other _ lxm = match !lst_top_mode with
|  Skip -> ()
|  _ -> match !lst_scan_mode with
    | Other -> lst_put lxm
    | Empty ->
        lst_scan_mode := Other ;
        lst_put lxm        
    | Letter ->
        lst_output_letter () ;
        lst_scan_mode := Other ;
        lst_put lxm

let lst_process_space _ lxm =  match !lst_top_mode with
| Skip -> ()
| _ ->
    begin match !lst_scan_mode with
    | Other -> lst_output_other ()
    | Letter -> lst_output_letter ()
    | _ -> ()
    end ;
    lst_scan_mode := Empty ;
    lst_direct_put lxm

  

let lst_init_char_table () =
  lst_init_chars
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@_$"
    lst_process_letter ;
  lst_init_chars "!\"#%&'()*+,-./:;<=>?[\\]^{}|`" lst_process_other ;
  lst_init_chars "0123456789" lst_process_digit ;
  lst_init_chars " \t" lst_process_space ;
  lst_init_char '\n' lst_process_newline
;;

(* Strings *)
let lst_quote_bs = ref false

let lst_init_quote s =
  for i = 0 to String.length s-1 do
    if s.[i] = 'b' then lst_quote_bs := true
  done

let lst_process_stringizer old_process lb lxm = match !lst_top_mode with
  | Normal ->
      lst_output_token () ;
      lst_top_mode := String lxm ;
      scan_this Scan.main "{\\lst@string@style" ;
      old_process lb lxm
  | String c when lxm = c &&
    (not !lst_quote_bs || !lst_last_char <> '\\') ->
      old_process lb lxm ;
      lst_output_token () ;
      scan_this Scan.main "}" ;
      lst_top_mode := Normal
  | _ -> old_process lb lxm

(* Comment *)

let chars_string c s =
  let rec do_rec r i =
    if i < String.length s then
      if List.mem s.[i] r then
        do_rec r (i+1)
      else
        do_rec (s.[i]::r) (i+1)
    else
      r in
  do_rec [c] 0

let init_char_table_delim chars wrapper =
  List.map
    (fun c ->
      let old_process = lst_char_table.(Char.code c) in
      lst_init_save_char c wrapper ;
      (c,old_process))
  chars

and restore_char_table_delim to_restore =
  let rec do_rec = function
    | [] -> ()
    | (c,f)::rest ->
        lst_init_char c f ;
        do_rec rest in
  do_rec to_restore
          

let eat_delim k new_mode old_process lb c s =
  let chars = chars_string c s in
  let wrapper old_process lb c = match !lst_top_mode with
  | Delim (n,to_restore) ->
      old_process lb c ;
      if n = 1 then begin
        lst_output_token () ;
        lst_top_mode := new_mode ;
        restore_char_table_delim to_restore ;
        k ()
      end else
        lst_top_mode := Delim (n-1,to_restore)
  | _ -> assert false in
  let to_restore = init_char_table_delim chars wrapper in
  lst_top_mode := Delim (1+String.length s, to_restore) ;
  wrapper old_process lb c 

let begin_comment () =
  lst_output_token () ;
  scan_this Scan.main "{\\lst@comment@style"

let lst_process_BNC _ s old_process lb c = match !lst_top_mode with
| Normal when if_next_string s lb -> 
    begin_comment () ;
    eat_delim (fun () -> ()) (Comment (Nested 0)) old_process lb c s
| Comment (Nested n) when if_next_string s lb ->
    eat_delim (fun () -> ()) (Comment (Nested (n+1))) old_process lb c s
| _ -> old_process lb c

and lst_process_ENC s old_process lb c = match !lst_top_mode with
| Comment (Nested 0) when if_next_string s lb ->
    eat_delim
      (fun () -> scan_this Scan.main "}")
      Normal
      old_process
      lb c s
|  Comment (Nested n) when if_next_string s lb ->
    eat_delim
      (fun () -> ())
      (Comment (Nested (n-1)))
      old_process lb c s
| _ -> old_process lb c

let lst_process_BBC check s old_process lb c = match !lst_top_mode with
| Normal when if_next_string s lb ->
    begin_comment () ;
    eat_delim
      (fun () -> ())
      (Comment (Balanced check))
      old_process lb c s
| _ -> old_process lb c

and lst_process_EBC s old_process lb c = match !lst_top_mode with
| Comment (Balanced check) when
  check c s && if_next_string  s lb ->
     eat_delim
      (fun () -> scan_this Scan.main "}")
      Normal
      old_process
      lb c s
| _ -> old_process lb c

let lst_process_LC s old_process lb c = match !lst_top_mode with
| Normal ->
    begin_comment () ;
    eat_delim
      (fun () -> ())
      (Comment Line)
      old_process lb c s
| _ -> old_process lb c
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
    end else begin
      !finish () ;
      raise
        (Eof "scan_byline")
    end}

and listings = parse
 '\n'* "\\end" [' ''\t']* '{' [^'}']+ '}'
    {let lxm = lexeme lexbuf in
    let env = env_extract lxm in
    lst_finalize () ;
    while !Scan.cur_env = "" do
      scan_this Scan.main "}"
    done ;
    if env = !Scan.cur_env then begin
      scan_this Scan.main ("\\end"^env) ;
      Scan.top_close_block "" ;
      Scan.close_env !Scan.cur_env ;
      Scan.check_alltt_skip lexbuf
    end else
      Misc.fatal "listings"}
|  eof
    {if not (Stack.empty stack_lexbuf) then begin
      let lexbuf = previous_lexbuf () in
      listings lexbuf
    end else begin
      raise
        (Eof "listings")
    end}
| '\n'
    {lst_nchars := 0 ;
    lst_process_newline lexbuf '\n' ;
    listings lexbuf}
| _
    {incr lst_nchars ;
    if !lst_nchars > !lst_gobble then begin
      let lxm = lexeme_char lexbuf 0 in
      lst_char_table.(Char.code lxm) lexbuf lxm
    end ;
    listings lexbuf}

and eat_line = parse
| eof
    {if not (Stack.empty stack_lexbuf) then begin
      let lexbuf = previous_lexbuf () in
      eat_line lexbuf
    end else begin
      raise
        (Eof "eat_line")
    end}
| [^'\n']  {eat_line lexbuf}
| '\n'     {lst_nchars := 0 ; lst_process_newline lexbuf '\n'}
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

let noeof lexer lexbuf =
  try lexer lexbuf
  with Eof s ->
    raise (VError ("End of file in environment: ``"^ !Scan.cur_env^"''"))

(*
and verb_input lexer file =
  let save = !Scan.cur_env in
  Scan.cur_env := "*verb-file*" ;
  begin try
    input_file !verbose
      (fun lexbuf ->
        try lexer lexbuf with Eof _ -> raise Misc.EndInput)
      file
  with
  | Myfiles.Except -> ()
  | Myfiles.Error _ -> ()
  end ;
   Scan.cur_env := save
*)

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
  noeof scan_byline lexbuf

and close_rawhtml _ = ()

let open_forget lexbuf =
  process := (fun () -> Out.reset line_buff) ;
  finish := (fun () -> Out.reset line_buff) ;
  noeof scan_byline lexbuf

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
  noeof scan_byline lexbuf

and close_tofile lexbuf = ()


let put_line_buff_image () =
  Out.iter (fun c -> Image.put_char c) line_buff ;
  Out.reset line_buff

let open_verbimage lexbuf =
  process := (fun () -> put_line_buff_image () ; Image.put_char '\n') ;
  finish := put_line_buff_image ;
  noeof scan_byline lexbuf

and close_verbimage _ = ()
;;


def_code "\\verbatim"
    (fun lexbuf ->
      open_verbenv false ;
      noeof scan_byline lexbuf) ;
def_code "\\endverbatim" close_verbenv ;


def_code "\\verbatim*"
    (fun lexbuf ->
      open_verbenv true ;
      noeof scan_byline lexbuf) ;        
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
(*
def_code "\\verbatiminput"
    (fun lexbuf ->
      let name = Scan.get_prim_arg lexbuf in
      open_verbenv false ;
      verb_input scan_byline name ;
      close_verbenv lexbuf) ;
def_code "\\verbatiminput*"
    (fun lexbuf ->
      let name = Scan.get_prim_arg lexbuf in
      open_verbenv true ;
      verb_input scan_byline name ;
      close_verbenv lexbuf) ;
*)
(* comment clashes with the ``comment'' package *)
Latexmacros.def "\\comment"  zero_pat (CamlCode open_forget) ;
Latexmacros.def "\\endcomment" zero_pat (CamlCode Scan.check_alltt_skip) ;
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
      noeof scan_byline lexbuf) ;
  def_code "\\endverbatimtab" close_verbenv_tabs ;
(*
  def_code "\\verbatimtabinput"
    (fun lexbuf ->
      let opt = Get.get_int (save_opt "\\verbatimtabsize" lexbuf) in
      tab_val := opt ;
      let name = Scan.get_prim_arg lexbuf in
      open_verbenv_tabs () ;
      verb_input scan_byline name ;
      close_verbenv_tabs lexbuf) ;
*)
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
      noeof scan_byline lexbuf) ;
  def_code "\\endlisting" close_listing ;
(*
  def_code "\\listinginput"
    (fun lexbuf ->
      let inter = Get.get_int (save_opt "1" lexbuf) in
      let start = Get.get_int (save_arg lexbuf) in
      let name = Scan.get_prim_arg lexbuf in
      interval := inter  ;
      open_listing start inter false ;
      verb_input scan_byline name ;
      close_listing lexbuf) ;
*)
  def_code "\\listingcont"
    (fun lexbuf ->
      open_listing !line !interval false ;
      noeof scan_byline lexbuf) ;
  def_code "\\endlistingcont" close_listing ;

  def_code "\\listing*"
    (fun lexbuf ->
      let inter = Get.get_int (save_opt "1" lexbuf) in
      let start = Get.get_int (save_arg lexbuf) in
      interval := inter ;
      open_listing start inter true ;
      noeof scan_byline lexbuf) ;
  def_code "\\endlisting*" close_listing ;

  def_code "\\listingcont*"
    (fun lexbuf ->
      Scan.check_alltt_skip lexbuf ;
      open_listing !line !interval false ;
      noeof scan_byline lexbuf) ;
  def_code "\\endlistingcont*" close_listing ;
  ())

(* The comment package *)

let init_comment () =
  def_code "\\@excludecomment" open_forget ;
  def_code "\\end@excludecomment"  Scan.check_alltt_skip ;
;;

register_init "comment" init_comment      
;;    

(* The listings package *)

let init_double_comment mode process_B process_E =
  let lxm_B = Scan.get_this_main ("\\@getprint{\\lst@B"^mode^"}")
  and lxm_E = Scan.get_this_main ("\\@getprint{\\lst@E"^mode^"}") in
  if lxm_B <> "" && lxm_E <> "" then begin
    let head_B = lxm_B.[0]
    and rest_B = String.sub lxm_B 1 (String.length lxm_B-1)
    and head_E = lxm_E.[0]
    and rest_E = String.sub lxm_E 1 (String.length lxm_E-1) in
    lst_init_save_char head_B
      (process_B
         (fun c s -> c = head_E && s = rest_E)
         rest_B) ;
    lst_init_save_char head_E (process_E rest_E)
  end

let open_lst keys lab =
  scan_this Scan.main ("\\lsthk@PreSet\\lstset{"^keys^"}") ;
(* Ignoring output *)
  lst_gobble := Get.get_int (string_to_arg "\\lst@gobble") ;
  lst_nchars := 0 ;
  lst_first := Get.get_int (string_to_arg "\\lst@first") ;
  lst_last := Get.get_int (string_to_arg "\\lst@last") ;
  lst_nlines := 0 ;
  if not !lst_print then begin
    lst_last := -2 ; lst_first := -1
  end ;  
  Printf.fprintf stderr "first=%d, last=%d\nprint=%b\n"
    !lst_first !lst_last !lst_print ;
  lst_init_char_table () ;
  scan_this Scan.main "\\lsthk@SelectCharTable" ;
  if !lst_extended then
    for i = 128 to 255 do
      lst_init_char (Char.chr i) lst_process_letter
    done ;
  scan_this Scan.main "\\lsthk@Init" ;
(* Strings *)
  let quote_mode =
     Scan.get_this_main "\\@getprint{\\lst@quote@stringizers}" in
  lst_init_quote quote_mode ;
  let stringizers =
     Scan.get_this_main "\\@getprint{\\lst@stringizers}" in
  lst_init_save_chars stringizers lst_process_stringizer ;
  Printf.fprintf stderr "quotemode=%s, stringizers=%s\n" quote_mode
    stringizers ;
(* Comments *)
  (* nested *)
  init_double_comment "NC" lst_process_BNC lst_process_ENC ;
  (* Balanced *)
  init_double_comment "BC@one" lst_process_BBC lst_process_EBC ;
  init_double_comment "BC@two" lst_process_BBC lst_process_EBC ;
  init_double_comment "BC@three" lst_process_BBC lst_process_EBC ;
  (* line *)
  let lxm_LC = Scan.get_this_main "\\@getprint{\\lst@LC}" in
  if lxm_LC <> "" then begin
    let head = lxm_LC.[0]
    and rest = String.sub lxm_LC 1 (String.length lxm_LC-1) in
    lst_init_save_char head (lst_process_LC rest)
  end ;
    
  scan_this Scan.main "\\lsthk@InitVar" ;
  Scan.top_open_block "PRE" "" ;  
  scan_this Scan.main "\\lst@basic@style" ;
  lst_scan_mode := Empty ;
  lst_top_mode := Skip ;

and close_lst lexbuf =
  scan_this Scan.main "\\lsthk@DeInit" ;
  Scan.top_close_block "PRE" ;
  Scan.check_alltt_skip lexbuf
;;

let lst_boolean lexbuf =
  let b = get_prim_arg lexbuf in
  prerr_endline ("BOOL: "^b) ;
  Dest.put
    (match b with
    | "" -> "false"
    | s  when s.[0] = 't' || s.[0] = 'T' -> "true"
    | _ -> "false")
;;

let init_listings () =
  Scan.newif_ref "lst@print" lst_print ;
  Scan.newif_ref "lst@extendedchars" lst_extended ;
  def_code "\\lst@boolean" lst_boolean ;
  def_code "\\lst@funcall"
    (fun lexbuf ->
      let csname = Scan.get_csname lexbuf in
      let old_raw = !raw_chars in
      let all_arg = get_prim_arg lexbuf in
      let lexarg = Lexing.from_string all_arg in
      let opt = Subst.subst_opt "" lexarg in
      let arg = Save.rest lexarg in
      let exec = csname^"["^opt^"]{"^arg^"}" in
      prerr_endline exec ;
      scan_this  Scan.main exec) ;
  def_code "\\lst@AddTo"
    (fun lexbuf ->
      let name = Scan.get_csname lexbuf in
      let old = get_prim name in
      let toadd = get_prim_arg lexbuf in
      Latexmacros.def name zero_pat (Subst (old^to_add))) ;      
  def_code "\\lstlisting"
    (fun lexbuf ->
      let keys = Subst.subst_opt "" lexbuf in
      let lab = Scan.get_prim_arg lexbuf in
      let lab = if lab = " " then "" else lab in
      def_print "\\lst@intname" lab ;
      open_lst keys lab ;
      (* Eat first line *)
      save_lexstate () ;
      noeof eat_line lexbuf ;
      restore_lexstate () ;
      (* Go ! *)
      noeof listings lexbuf) ;
  def_code "\\endlstlisting" close_lst ;

  def_code "\\lst@definelanguage"
    (fun lexbuf ->
      let dialect = get_prim_opt "" lexbuf in
      let language = get_prim_arg lexbuf in
      let base_dialect = get_prim_opt "!*!" lexbuf in

      let mk_dialect l d = match d with
      | "" -> get_prim ("\\lstdd@"^l)
      | _ -> d in
      let dialect = mk_dialect language dialect in

      match base_dialect with
      | "!*!" ->
          let keys = subst_arg lexbuf in
          let _ = save_opt "" lexbuf in
          scan_this main
            ("\\lst@definelanguage@{"^language^"}{"^
             dialect^"}{"^keys^"}")
      | _  ->
          let base_language = get_prim_arg lexbuf in
          let base_dialect = mk_dialect base_language base_dialect in
          let keys = subst_arg lexbuf in
          let _ = save_opt "" lexbuf in
          scan_this main
            ("\\lst@derivelanguage@{"^
             language^"}{"^ dialect^"}{"^
             base_language^"}{"^base_dialect^"}{"^
             keys^"}"))        
;;

register_init "listings" init_listings
;;


def_code "\\@scaninput"
  (fun lexbuf ->
    let pre = save_arg lexbuf in
    let file = get_prim_arg lexbuf in
    let {arg=post ; subst=post_subst} = save_arg lexbuf in
    try
      let true_name,chan = Myfiles.open_tex file in
      let filebuff = Lexing.from_channel chan in
      start_lexstate () ;
      Location.set true_name filebuff ;
      begin try
        record_lexbuf (Lexing.from_string post) post_subst ;
        scan_this_may_cont Scan.main filebuff top_subst
          pre ;
      with e -> 
        restore_lexstate () ;
        Location.restore () ;
        close_in chan ;
        raise e
      end ;
      restore_lexstate () ;
      Location.restore () ;
      close_in chan
    with
    | Myfiles.Except ->
        warning ("Not opening file: "^file)
    | Myfiles.Error s ->
        warning s)
end
} 

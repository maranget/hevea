(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*****************************2******************************************)

{
open Parse_opts
open Lexing
open Myfiles
open Latexmacros
open Html
open Save

let header = "$Id: latexscan.mll,v 1.36 1998-09-02 13:40:27 maranget Exp $" 

let push s e = s := e:: !s
and pop s = match !s with
  [] -> failwith "Empty stack"
| e::rs -> s := rs ; e
;;

let stack_lexbuf = ref []
and stack_stack_lexbuf = ref []
;;

let eat_space = ref true
and stack_eat = ref []
and stack_stack_eat = ref []
;;

let save_lexstate () =
  push stack_eat !eat_space ;
  push stack_stack_lexbuf (!stack_lexbuf,!stack_eat)

and restore_lexstate () =
  let l,s = pop stack_stack_lexbuf in
  stack_lexbuf := l ;
  stack_eat := s;
  eat_space := pop stack_eat
;;

let start_lexstate () =
  save_lexstate () ;
  stack_lexbuf := [] ;
  eat_space := false
;;


let stack = ref [||]
and stack_stack = ref []
;;

let pretty_lexbuf lb =
  let  pos = lb.lex_curr_pos and len = String.length lb.lex_buffer in
  prerr_endline "Buff contents:" ;
  prerr_endline ("<<"^String.sub lb.lex_buffer pos (len-pos)^">>");
  prerr_endline ("curr_pos="^string_of_int lb.lex_curr_pos);
  prerr_endline "End of buff"
;;

let out_file = ref (Out.create_null ())
;;

let prelude = ref true
;;

let flushing = ref false
;;


let my_int_of_string s =
  try int_of_string s with
  Failure m -> raise (Failure (m^": "^s))
;;

let env_extract s =
  let i = String.index s '{'
  and j = String.rindex s '}' in
  String.sub s (i+1) (j-i-1)
;;

let name_extract s =
  if s = "\\ " then s
  else
    let j = String.rindex s '\\' in
    let s = String.sub s j (String.length s - j) in
    let j = try String.index s ' ' with Not_found -> String.length s in
    String.sub s 0 j


and spaces_extract s =
  let i = ref 0 in
  while String.get s !i = ' ' || String.get s !i = '\n' do
    i := !i + 1
  done ;
  String.sub s 0 !i

and last_letter name =
  let c = String.get name (String.length name-1) in
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
;;

let subst_arg arg = try
  let _ = String.index arg '#' in
  Subst.subst (Lexing.from_string arg) !stack
with
  Subst.BadArg -> begin
    prerr_endline ("Bad var number in  ``"^arg^"''");
    raise (Failure "subst")
    end
| Not_found -> arg
;;

let save_quote_arg lexbuf =

  let rec save_rec lexbuf =
    try Save.arg lexbuf
    with Save.BadParse "EOF" -> begin
        if !stack_lexbuf = [] then failwith "Eof while looking for arg";
        let lexbuf = pop stack_lexbuf in
        if !verbose > 2 then begin
          prerr_endline "popping stack_lexbuf in save_arg";
          pretty_lexbuf lexbuf
        end;
        save_rec lexbuf end in

  let old_stack = !stack_lexbuf in
  let arg = save_rec lexbuf in
  if !verbose > 2 then
    prerr_endline ("Arg parsed: <"^arg^">") ;
  stack_lexbuf := old_stack ;
  arg
;;

let save_arg lexbuf =
  let arg = save_quote_arg lexbuf in
  let r = subst_arg arg in
   if !verbose > 2 then
    prerr_endline ("Arg subst: <"^arg^">") ;
  r
;;

let rec parse_args_norm pat lexbuf = match pat with
  [] -> []
| s :: pat ->
    let arg = save_arg lexbuf in
    let r = parse_args_norm pat lexbuf in
     arg :: r
;;


type ok = No of string | Yes of string
;;

let from_ok = function
  Yes s -> (Latexmacros.optarg := true ; s)
| No s  -> (Latexmacros.optarg := false ; s)
;;

let pretty_ok = function
  Yes s -> "+"^s^"+"
| No s  -> "-"^s^"-"
;;

let rec parse_quote_arg_opt def lexbuf =
  let old_stack_lexbuf = !stack_lexbuf in
  let r = try Yes (Save.opt lexbuf) with
    Save.NoOpt -> No def
  | Save.BadParse "EOF" -> begin
      if !stack_lexbuf = [] then No def
      else let lexbuf = pop stack_lexbuf in
        if !verbose > 2 then begin
          prerr_endline "poping stack_lexbuf in parse_quote_arg_opt";
          pretty_lexbuf lexbuf
        end;
        parse_quote_arg_opt def lexbuf end in
  if !verbose > 2 then begin
     Printf.fprintf stderr "Parse opt : %s" (pretty_ok r) ;
     prerr_endline ""
  end ;
  stack_lexbuf := old_stack_lexbuf ;
  r
;;

let parse_arg_opt def lexbuf =
  let arg = parse_quote_arg_opt def lexbuf in
   (match arg with Yes s -> Yes (subst_arg s)
           | No s -> No (subst_arg s))
;;

let rec parse_args_opt pat lexbuf = match pat with
  [] -> []
| def::rest ->
   let arg = parse_arg_opt def lexbuf in
   let r   = parse_args_opt rest lexbuf in
   arg :: r
;;


let skip_opt lexbuf =
  let _ =  parse_quote_arg_opt "" lexbuf  in
  ()

and check_opt lexbuf =
  match parse_quote_arg_opt "" lexbuf  with
    Yes _ -> true
  | No  _ -> false

and save_opt def lexbuf =
  match parse_arg_opt def  lexbuf with
    Yes s -> s
  | No s  -> s
;;


let parse_args (popt,pat) lexbuf =
  let opts =  parse_args_opt popt lexbuf in
  let args =  parse_args_norm pat lexbuf in
  (opts,args)
;;

let make_stack name pat lexbuf =
  let (opts,args) = parse_args pat lexbuf in
  let stack = Array.of_list (List.map from_ok opts@args) in
  if !verbose > 2 then begin
    Printf.fprintf stderr "make_stack for macro: %s "  name ;
    Latexmacros.pretty_pat pat ;
    prerr_endline "";
    for i = 0 to Array.length stack-1 do
      Printf.fprintf stderr "\t#%d = %s\n" (i+1) stack.(i)
    done
  end ;
  stack
;;


let rec pretty_opts = function
  [] -> ""
| NoMath::r -> "{NoMath}"^pretty_opts r
| Wrap::r -> "{Wrap}"^pretty_opts r
;;

let pretty_format = function
  Align (s,o) -> s^pretty_opts o
| Inside s -> "@{"^s^"}"
;;

let pretty_formats f =
  Array.iter (fun f -> prerr_string (pretty_format f) ; prerr_char ' ') f
;;



let if_level = ref 0
;;

exception IfFalse
;;

let verb_delim = ref (Char.chr 0)
let tab_val = ref 8
;;

let cur_env = ref ""
and macros = ref []
and stack_env = ref []
and env_level = ref 0
and stack_in_math = ref []
and stack_display = ref []
;;

let macro_register name =
  if !env_level > 0 then
   macros := name :: !macros
;;

let macros_unregister () =
  List.iter
   (fun name -> Latexmacros.unregister name) !macros
;;

let get_script_font () =
  let n = Html.get_fontsize () in
  if n >= 1 then n-1 else n
;;

let open_script_font () =
  Html.open_mod (Font (get_script_font ()))
;;

let inc_size i =
  let n = Html.get_fontsize () in
  let new_size =
    if n+i <= 1 then 1
    else if n+i >= 7 then 7
    else n+i in
  Html.open_mod (Font new_size)
;;

let big_size () =  Html.open_mod (Font 7)
;;

(* Horizontal display *)
let display_arg  verbose =
  if verbose > 0 then
    "BORDER=1 CELLSPACING=0 CELLPADDING=0"
  else
    "CELLSPACING=0 CELLPADDING=0"
;;

let open_display () =
  if !display then begin
    if !verbose > 1 then
       prerr_endline "open display" ;
    Html.open_display (display_arg !verbose)
  end

and item_display () =
  if !display then begin
    Html.item_display ()
  end
;;

let close_display () =
  if !display then begin
    Html.close_display ()
  end

and close_pending_display () =
  if !display then begin
    Html.erase_display ();
  end
;;

(* vertical display *)
let open_vdisplay () =
  if !verbose > 1 then
    prerr_endline "open_vdisplay";
  Html.open_block "TABLE" (display_arg !verbose)

and close_vdisplay () =
  if !verbose > 1 then
    prerr_endline "close_vdisplay";
  Html.close_vdisplay ()

and open_vdisplay_row s =
  if !verbose > 1 then
    prerr_endline "open_vdisplay_row";
  Html.open_block "TR" "" ;
  Html.open_block "TD" s ;
  open_display ()

and close_vdisplay_row () =
  if !verbose > 1 then
    prerr_endline "close_vdisplay_row";
  close_display () ;
  Html.force_block "TD" "&nbsp;" ;
  Html.close_block "TR"
;;


let open_center () =  Html.open_block "DIV" "ALIGN=center"

and close_center () = Html.close_block "DIV"
;;



let iput_opt def arg =
  if arg = def then ()
  else begin
    Image.put "[" ;
    Image.put arg ;
    Image.put "]"
  end
;;

let iput_arg arg =
  Image.put "{" ;
  Image.put arg ;
  Image.put "}"
;;

let iput_newpage arg =
  let n = Image.page () in
  Image.put ("% page: "^n^"\n") ;
  Image.put "\\clearpage\n\n" ;
  Html.put "<IMG " ;
  if arg <> "" then begin
    Html.put arg;
    Html.put_char ' '
  end ;
  Html.put "SRC=\"" ;
  Html.put n ;
  Html.put "\">"
;;

let unparse_args opts args =
  let rec do_args = function
    [] -> ""
  | s::rest -> "{"^s^"}"^do_args rest in
  let rec do_opts = function
    Yes s::rest -> "["^s^"]"^do_opts rest
  | _ -> "" in
  do_opts opts^do_args args
;;

let scan_this lexfun s =
  start_lexstate ();
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this : [%s]" s ;
    prerr_endline ""  
  end ;
  let lexer = Lexing.from_string s in
  let r = lexfun lexer in
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this : over" ;
    prerr_endline ""
  end ;
  restore_lexstate ();
  r
;;

let scan_this_may_cont eat lexfun lexbuf s =
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_may_cont : [%s]" s ;
    prerr_endline "" ;
    if !verbose > 2 then begin
      prerr_endline "Pushing lexbuf" ;
      pretty_lexbuf lexbuf
    end
  end ;

  save_lexstate ();
  push stack_eat !eat_space ;
  eat_space := eat ;
  push stack_lexbuf lexbuf ;

  let lexer = Lexing.from_string s in
  let r = lexfun lexer in

  restore_lexstate ();

  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_may_cont : over" ;
    prerr_endline ""
  end ;
  r
;;

let get_this lexfun s =
  start_lexstate ();
  push stack_display !display;
  display := false;
  if !verbose > 1 then begin
    Printf.fprintf stderr "get_this : [%s] = " s ;
  end ;
  let lexer = Lexing.from_string ("\\mbox{"^s^"}") in
  let r = Html.to_string (fun () -> lexfun lexer) in
  if !verbose > 1 then begin
    prerr_endline r
  end ;
  display := pop stack_display;
  restore_lexstate();
  r
;;

let get_style lexfun s =
  start_lexstate ();
  push stack_display !display;
  display := false;
  let lexer = Lexing.from_string s in
  let r = Html.to_style (fun () -> lexfun lexer) in
  display := pop stack_display;
  restore_lexstate();
  r
;;
  
let put_delim delim i =
  if !verbose > 1 then
    prerr_endline
     ("put_delim: ``"^delim^"'' ("^string_of_int i^")") ;
  if delim <> "." then begin
    Html.begin_item_display () ;
    Symb.put_delim Html.skip_line Html.put delim i ;
    let _ = Html.end_item_display () in ()
  end
;;

let default_format = Align ("left",[])
;;

type in_table = Table | Border | NoTable | Tabbing
;;

let is_table = function
  (Table|Border) -> true
| _       -> false

and is_tabbing = function
  Tabbing -> true
| _ -> false
;;

let cur_format = ref [||]
and stack_format = ref []
and cur_col = ref 0
and stack_col = ref []
and in_table = ref NoTable
and stack_table = ref []
and first_col = ref false
and stack_first = ref []
;;

let prerr_array_state () =
  prerr_string "  format:";
  pretty_formats !cur_format ;
  prerr_endline "" ;
  prerr_endline ("  cur_col="^string_of_int !cur_col) ;
  prerr_endline ("  first_col="^
      (if !first_col then "true" else "false"))
;;

let save_array_state () =
  push stack_format !cur_format ;
  push stack_col !cur_col ;
  push stack_table !in_table ;
  push stack_first !first_col ;
  if !verbose > 1 then begin
    prerr_endline "Save array state:" ;
    prerr_array_state ()
  end    

and restore_array_state () =
  in_table := pop stack_table ;
  cur_col := pop stack_col ;
  cur_format := pop stack_format ;
  first_col  := pop stack_first ;
  if !verbose > 1 then begin
    prerr_endline "Restore array state:" ;
    prerr_array_state ()
  end  
;;


exception EndInside
;;
exception NoMulti
;;

let rec keep_math = function
 [] -> true
| NoMath::_ -> false
| _::r -> keep_math r

let rec is_par = function
  [] -> false
| Wrap::_ -> true
| _::r -> is_par r
;;

let is_inside = function
  Inside _ -> true
| _ -> false

and as_inside = function
  Inside s -> s
| _        -> ""

and as_align = function
  Align (s,o) -> if is_par o then "ALIGN="^s else "NOWRAP ALIGN="^s
| _       -> failwith "as_align"

and is_display = function
    Align (_,o) -> keep_math o
  | _           -> true

and as_colspan = function
  1 -> ""
| n -> " COLSPAN="^string_of_int n
;;

let get_col format i =
  let r = 
    if i >= Array.length format then default_format
    else format.(i) in
  if !verbose > 2 then begin
   Printf.fprintf stderr "get_col : %d: " i ;
   prerr_string (pretty_format r) ;
   prerr_char '\t' ;
   pretty_formats format ;
   prerr_newline ()
  end ;
  r
;;

let show_inside main format i =
(*
  if !verbose > -1 then begin
    prerr_string ("show_inside: "^string_of_int i)
  end ;
*)
  let t = ref i in
  begin try while true do
    begin match get_col format !t with
      Inside s ->
        let s = get_this main s in
        Html.open_block "TD" "ALIGN=center";
        Html.put s ;
        Html.force_block "TD" ""
    | _ -> raise EndInside
    end ;
    t := !t+1
  done with EndInside -> ()
  end ;
(*
  if !verbose > -1 then
    prerr_endline (" -> "^string_of_int !t) ;
*)
  !t
;;

let rec eat_inside format i =
  if i >= Array.length format then i
  else if is_inside (get_col format i) then
    eat_inside format (i+1)
  else i
;;

let rec eat_cols n format i = match n with
  0 -> eat_inside format i
| _ ->
   let f = get_col format i in
   if is_inside f then
     eat_cols n format (i+1)
   else
     eat_cols (n-1) format (i+1)
;;



let find_align format =
  let t = ref 0 in
  while is_inside (get_col format !t) do
    t := !t+1
  done ;
  !t
;;

let show_inside_multi main format i j =
  let rec show_rec i =
    if i >= j then ()
    else begin
      let s = get_this main (as_inside (get_col format i)) in
      Html.put s ;
      show_rec (i+1)
    end in
  show_rec i
;;

let open_col main  =
  cur_col :=  show_inside main !cur_format !cur_col ;
  let format = (get_col !cur_format !cur_col) in
  Html.open_block "TD" (as_align format) ;
  open_display ();
  if not (is_display format) then begin
    push stack_in_math !in_math ;
    in_math := false;
  end
;;

let open_first_col main =
  first_col := true ;
  Html.open_group "" ;
  open_col main  
;;

let erase_col () =
  close_pending_display () ;
  Html.erase_block "TD" ;
  if !first_col then
    Html.erase_block ""
;;


let open_row () =
  cur_col := 0 ;
  Html.open_block "TR" ""

and close_row () =
  Html.close_block "TR"
;;


let do_hline main =
  if !verbose > 2 then begin
    Printf.fprintf stderr "hline: %d %d" !cur_col (Array.length !cur_format) ;
    prerr_newline ()
  end ;
    erase_col () ;
    Html.erase_block "TR" ;
    Html.open_block "TR" "" ;
    Html.open_block
      "TD"
      ("ALIGN=center HEIGHT=2"^
      as_colspan (Array.length !cur_format)) ;
    Html.close_mods () ;
    Html.put "<HR NOSHADE SIZE=2>" ;
    Html.close_block "TD" ;
    Html.close_block "TR" ;
    open_row () ;
    open_first_col main
;;

let change_td_pending args =
  close_pending_display () ;
  Html.change_block "TD" args  ;
  open_display ()
;;

let do_multi n format main =
  if !verbose > 1 then begin
    prerr_string
      ("multicolumn: n="^string_of_int n^" format:") ;
    pretty_formats format ;
    prerr_endline ""
  end ;

  if !first_col then begin
    close_display () ;
    Html.close_block "TD" ;
    Html.erase_block "" ;
    Html.open_group "" ;
    Html.open_block "TD" "" ;
    open_display ()
  end ;
    
  let i = find_align format
  and next_cur_col = eat_cols n !cur_format !cur_col in
  change_td_pending
    (as_align (get_col format i)^
    as_colspan (next_cur_col - !cur_col)) ;
  if !verbose > 1 then begin
     prerr_string ("multi-end: cur="^string_of_int !cur_col^
       " next="^string_of_int next_cur_col^" ") ;
     pretty_formats !cur_format;
     prerr_endline ""
   end ;     
  cur_col := next_cur_col-1
;;


let close_col main content =
  let old_format = get_col !cur_format !cur_col in
  if not (is_display old_format) then begin
    in_math := pop stack_in_math
  end ;
  close_display ();
  Html.force_block "TD" content ;
  if !first_col then begin
    first_col := false ;
    Html.close_group ()
  end ;
  cur_col := !cur_col + 1;
  cur_col := show_inside main !cur_format !cur_col
;;

let close_last_col main content =
    close_col main content
;;

      
let new_env env =
  push stack_env (!cur_env,!macros)   ;
  cur_env := env ;
  macros := [] ;
  if env <> "document" && env <> "*input" then incr env_level ;
  if !verbose > 1 then begin
    Location.print_pos () ;
    Printf.fprintf stderr "Begin : %s <%d>" env !env_level ;
    prerr_endline ""
  end ;
;;

let error_env close_e open_e =
  raise (Failure (close_e^" closes : "^open_e))
;;

let close_env env  =
  if !verbose > 1 then begin
    Printf.fprintf stderr "End: %s <%d>" env !env_level ;
    prerr_endline  ""
  end ;
  if env = !cur_env then begin  
    if env <> "document" && env <> "*input" then decr env_level ;
    let e,m = pop stack_env in    
    cur_env := e ;
    macros_unregister () ;
    macros := m
  end else
    error_env env !cur_env
;;

let top_par () =
  if not (!display || !in_math) then Html.par ();
;;

let open_top_group () = 
  if !verbose > 2 then prerr_endline "Open brace" ;
    push stack_table !in_table ;
    in_table := NoTable ;
    if !display then begin
      item_display () ; Html.open_group "" ; open_display ()
    end else
      Html.open_group "" ;
    new_env " " 

and close_top_group () =
  if !verbose > 2 then prerr_endline "Close brace" ;
  in_table := pop stack_table ;
  let env = " " in
  if env = !cur_env then begin
    if !display then begin
      close_display () ; Html.close_group () ; item_display ()
    end else
      Html.close_group ();
    close_env env
  end else if !cur_env = "*mbox" then begin
    in_math := pop stack_in_math ;
    Html.close_group () ;
    display := pop stack_display ;
    if !display then item_display () ;
    close_env !cur_env
  end else if !cur_env = "*hbox" then begin
    Html.close_group () ;
    display := pop stack_display ;
    if !display then item_display () ;
    close_env !cur_env
  end else begin
    error_env env !cur_env
  end
;;

type ital = Ital | NoItal | Complex | Mixed
;;

let check_char = function
  '{' | '}' | '$' | '^' | '_' | '\\' -> Complex
| c ->
   if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) then
     Ital
   else NoItal
;;

exception Over
;;

let check_ital s =
  let rec check_rec = function
    -1 -> failwith "Empty string in check_rec"
  | 0  -> check_char (String.get s 0)
  | i  ->
     let t = check_char (String.get s i)
     and tt = check_rec (i-1) in
     match t,tt with
       Ital,Ital -> Ital
     | NoItal,NoItal -> NoItal
     | Ital,NoItal   -> Mixed
     | NoItal,Ital   -> Mixed
     | Complex,_ -> raise Over
     | _,Complex -> raise Over
     | _,Mixed   -> Mixed
     | Mixed,_   -> Mixed in

  match s with "" -> NoItal
  | _ ->
     try check_rec (String.length s-1) with Over -> Complex
;;

let complex s = match check_ital s with
  Complex -> true
| _       -> false
;;

let put_sup_sub tag main = function
  "" -> ()
| s  -> begin
   match check_ital s with
     Complex ->
       Html.open_group tag ;
       open_script_font () ;
       scan_this main s;
       Html.close_group ()
   | _ ->     
       Html.put_char '<' ;
       Html.put tag ;
       Html.put_char '>' ;
       let sf = get_script_font () in
       Html.put ("<FONT SIZE="^string_of_int sf^">") ;
       scan_this main s ;
       Html.put "</FONT>" ;
       Html.put "</" ;
       Html.put tag ;
       Html.put_char '>'
  end
;;



let standard_sup_sub main what sup sub =
  if !display && (complex sup || complex sub) then begin
    force_item_display () ;
    open_vdisplay () ;
    if sup <> "" then begin
      open_vdisplay_row "" ;
      open_script_font () ;
      scan_this main sup ;
      close_vdisplay_row ()
    end ;           
    open_vdisplay_row "" ;
    what ();
    close_vdisplay_row () ;
    if sub <> "" then begin
      open_vdisplay_row "" ;
      open_script_font () ;
      scan_this main sub ;
      close_vdisplay_row ()
    end ;
      close_vdisplay () ;
      force_item_display ()
  end else begin
     what ();
     put_sup_sub "SUB" main sub ;
     put_sup_sub "SUP" main sup
  end
;;


let limit_sup_sub main what sup sub =
  if sup = "" && sub = "" then
    what ()
  else begin
    force_item_display () ;
    open_vdisplay () ;
    open_vdisplay_row "ALIGN=center" ;
    open_script_font () ;
    scan_this main sup ;
    close_vdisplay_row () ;
    open_vdisplay_row "ALIGN=center" ;
    what () ;
    close_vdisplay_row () ;
    open_vdisplay_row "ALIGN=center" ;
    open_script_font () ;
    scan_this main sub ;
    close_vdisplay_row () ;
    close_vdisplay () ;
    force_item_display ()
  end
;;

let int_sup_sub main what sup sub =
    force_item_display () ;
    what () ;
    force_item_display () ;    
    open_vdisplay () ;
    open_vdisplay_row "ALIGN=left" ;
    open_script_font () ;
    scan_this main sup ;
    close_vdisplay_row () ;
    open_vdisplay_row "ALIGN=left" ;
    Html.put "&nbsp;" ;
    close_vdisplay_row () ;
    open_vdisplay_row "ALIGN=left" ;
    open_script_font () ;
    scan_this main sub ;
    close_vdisplay_row () ;
    close_vdisplay () ;
    force_item_display ()
;;

let input_file verbose main filename =
  try
    let filename,input = Myfiles.open_tex filename in
    if verbose > 0 then
      prerr_endline ("Input file: "^filename) ;
    let buf = Lexing.from_channel input in
    Location.set filename buf ;
    new_env "*input" ;
    main buf ;
    Location.restore () ;
    close_env "*input"
  with Myfiles.Except -> begin
    if verbose > 0 then
      prerr_endline ("Not opening file: "^filename) ;
    end
 | Myfiles.Error m -> begin
     if not !silent || verbose > 0 then begin
       Location.print_pos () ;
       prerr_endline ("Warning: "^m) ;
     end ;
   end
;;

let no_prelude () =
  flushing := true ;
  prelude := false ;
  Html.forget_par () ;
  Html.set_out !out_file
;;
}

rule  main = parse
(* comments *)
   '%'+ {comment lexbuf}
(* included images *)
| ".PS\n" {Image.dump  "\n.PS\n" image lexbuf}
| "\\box\\graph" ' '*
    {if !verbose > 2 then prerr_endline "Graph" ;
    Image.put "\\box\\graph\n";
    iput_newpage "" ;
    main lexbuf}
(* Styles and packages *)
| "\\documentstyle"  | "\\documentclass"
    {let command = lexeme lexbuf in
    let opt = parse_quote_arg_opt "" lexbuf in
    let arg =  Save.arg lexbuf in
    let oldverb = !verbose in
    verbose := 0;
    begin try if not !Latexmacros.styleloaded then
      input_file oldverb main (arg^".sty") with
    Not_found -> prerr_endline "Warning: cannot read style file" end ;
    Image.start () ;
    Image.put command ;
    Image.put (unparse_args [opt] [arg]) ;
    Image.put "\n" ;
    verbose := oldverb ;
    main lexbuf}
(* Paragraphs *)
  | "\n\n" '\n' *
    {if !alltt then begin
      Html.put (lexeme lexbuf)
    end else  top_par () ;
    main lexbuf }
| "\\input" | "\\include" | "\\bibliography"
     {let lxm = lexeme lexbuf in
     let arg = Save.input_arg lexbuf in
     let filename =
       if lxm = "\\bibliography" then Location.get_base ()^".bbl"
       else arg in
     begin try input_file !verbose main filename
     with Not_found ->
       Image.put (lxm^"{"^arg^"}\n")
     end ;
     main lexbuf}
| "\\verbatiminput"
      {let lxm = lexeme lexbuf in
      let tabs = my_int_of_string (save_opt "8" lexbuf) in      
      let arg = save_arg lexbuf in
      let old_tabs = !tab_val in
      tab_val := tabs ;
      Html.open_block "PRE" "" ;
      begin try
        input_file !verbose verbenv arg ;
      with Not_found ->
       Image.put (lxm^"{"^arg^"}\n")
      end ;
      Html.close_block "PRE" ;
      tab_val := old_tabs ;
      main lexbuf}
| "\\usepackage"
      {let lxm = lexeme lexbuf in
      let arg = save_arg lexbuf in
      Image.put (lxm^unparse_args [] [arg]^"\n") ;
      main lexbuf}
(* subscripts and superscripts *)
  | ('_' | '^')
     {let lxm = lexeme lexbuf in
     if !alltt then Html.put lxm
     else begin
       let sup,sub = match lxm with
         "^" ->
           let sup = Save.sarg lexbuf in
           let sub = get_sub lexbuf in
           sup,sub
       | _   ->
           let sub = Save.sarg lexbuf in
           let sup = get_sup lexbuf in
           sup,sub in
       standard_sup_sub main (fun () -> ()) sup sub
     end ;
     main lexbuf}
(* Math mode *)
| "$" | "$$" | "\\(" | "\\)" | "\\[" | "\\]"
     {let lxm = lexeme lexbuf in
     if !alltt && (lxm = "$" || lxm = "$$") then
       begin Html.put lxm ; main lexbuf end
     else begin
       let lxm = match lxm with
         "\\(" | "\\)" -> "$"
       | "\\[" | "\\]" -> "$$"
       | _ -> lxm in
       let dodo = lxm <> "$" in
       let math_env = if dodo then "*display" else "*math" in
       if !in_math then begin
         in_math := pop stack_in_math ;
         if dodo then begin
           close_display () ;
           close_center ()
         end else begin
           Html.close_group ()
         end ;
         display := pop stack_display ;
         if !display then begin
           item_display ()
         end ;
         close_env math_env ;
         main lexbuf
     end else begin
       push stack_in_math !in_math ;
       in_math := true ;
       let lexfun lb =
         if !display then  begin item_display () end ;
         push stack_display !display ;
         if dodo then begin
           display  := true ;
           open_center() ;
           open_display ()
         end else begin
           display := false ;
           Html.open_group ""
         end;
         skip_blanks lb ; main lb in
       new_env math_env ;
       lexfun lexbuf
     end end}
| "\\mbox"
   {mbox_arg lexbuf}

(* Definitions of  simple macros *)
  | "\\def" | "\\gdef" | "\\global\\def"
     {let lxm = lexeme lexbuf in
     let name = Save.csname lexbuf in
     let args_pat = defargs lexbuf in
     let body = Save.arg lexbuf in
     if !env_level = 0 || lxm <> "\\def" then
       Image.put
         (lxm^name^
         (List.fold_right (fun s r -> s^r) args_pat ("{"^body^"}\n"))) ;
     begin try
       def_macro_pat name ([],args_pat) [Subst body] ;
       if lxm = "\\def" then macro_register name
     with Latexmacros.Failed -> () end ;
     main lexbuf
    }
  | "\\renewcommand" | "\\newcommand" | "\\providecommand"
    {let lxm = lexeme lexbuf in
    let name = Save.csname lexbuf in
    let nargs = parse_args_opt ["0" ; ""] lexbuf in
    let body = Save.arg lexbuf in
    if (!env_level = 0) then
      Image.put
        (lxm^"{"^name^"}"^unparse_args nargs [body]^"\n") ;
    let nargs,(def,defval) = match nargs with
      [a1 ; a2] ->
        my_int_of_string (from_ok a1),
        (match a2 with
           No s -> false,s
        | Yes s -> true,s)
    | _ -> failwith "Opts args in newcomand" in
    begin try
      (match lxm with
        "\\newcommand"   -> def_macro_pat
      | "\\renewcommand" -> redef_macro_pat
      | _                -> provide_macro_pat) name
        (Latexmacros.make_pat (if def then [defval] else []) nargs)
        [Subst body] ;
      macro_register name
    with Latexmacros.Failed -> () end ;
    main lexbuf}
  | "\\newenvironment" ' '*
     {let lxm = lexeme lexbuf in
     let name = save_quote_arg lexbuf in
     let nargs = parse_quote_arg_opt "0" lexbuf in
     let optdef = parse_quote_arg_opt "" lexbuf in
     let body1 = save_quote_arg lexbuf in
     let body2 = save_quote_arg lexbuf in
     if !env_level = 0 then
       Image.put
         (lxm^
         unparse_args [] [name]^
         unparse_args [nargs;optdef] [body1;body2]) ;
     begin try
       def_env_pat name
         (Latexmacros.make_pat
           (match optdef with No _ -> [] | Yes s -> [s])
           (match nargs with No _ -> 0 | Yes s -> my_int_of_string s))
         [Subst body1] [Subst body2];
       macro_register ("\\"^name) ; 
       macro_register ("\\end"^name)
     with Latexmacros.Failed -> () end ;
     main lexbuf}
  | "\\newtheorem" | "\\renewtheorem"
      {let lxm = lexeme lexbuf in
      let name = save_quote_arg lexbuf in
      let numbered_like = parse_quote_arg_opt "" lexbuf in
      let caption = save_arg lexbuf in
      let within = parse_quote_arg_opt "" lexbuf in
      if !env_level = 0 then
      Image.put (lxm^
       unparse_args [] [name]^
       unparse_args [numbered_like] [caption]^
       unparse_args  [within] []^"\n") ;

      begin try
        let cname = match numbered_like,within with
          No _,No _ ->
            Counter.def_counter name "" ;
            def_macro ("\\the"^name) 0 [Subst ("\\arabic{"^name^"}")] ;
            name
        | _,Yes within ->
            Counter.def_counter name within ;
            def_macro ("\\the"^name) 0
              [Subst ("\\the"^within^".\\arabic{"^name^"}")] ;
           name
        | Yes numbered_like,_ -> numbered_like in
      
        def_env_pat name (Latexmacros.make_pat [""] 0)
          [Subst ("\\cr{\\bf\\stepcounter{"^cname^"}"^caption^"~"^
            "\\the"^cname^"}\\quad\\ifoptarg{\\purple[#1]\\quad}\\fi\\it")]
          [Subst "\\cr"]
      with Latexmacros.Failed -> () end ;
      main lexbuf}
  | "\\let" | "\\global\\let"
     {let lxm = lexeme lexbuf in
     let name = save_arg lexbuf in
     skip_equal lexbuf ;
     let alt = save_arg lexbuf in
     begin try
       let nargs,body = find_macro alt in
       begin try
         def_macro_pat name nargs body ;
         if lxm = "\\let" then macro_register name
      with Latexmacros.Failed -> () end
     with Not_found -> () end ;
     if !env_level = 0 || lxm <> "\\let" then begin
       Image.put lxm ;
       Image.put name ;
       Image.put "=" ;
       Image.put alt ;
       Image.put "\n"
     end ;
     if lxm = "\\let" then macro_register name ;
     main lexbuf}
(* Raw html, latex only *)
| "\\begin" ' '* "{rawhtml}"
     {rawhtml lexbuf; main lexbuf }
| "\\begin" ' '* "{latexonly}"
     { latexonly lexbuf; main lexbuf }
| "\\begin" ' '* "{toimage}"
     {Image.dump "" image lexbuf}
(* tabbing *)
| "\\begin" ' '* "{tabbing}"
   {let lexfun lb =
     Html.open_block "TABLE" "CELLSPACING=0 CELLPADDING=0" ;
     Html.delay (fun _ -> ()) ;
     Html.open_block "TR" "" ;
     Html.open_block "TD" "" ;
     main lb in
   push stack_table !in_table ;
   in_table := Tabbing ;
   new_env "tabbing" ;
   lexfun lexbuf}
| "\\end" ' '* "{tabbing}"
   {Html.close_block "TD" ;
   Html.close_block "TR" ;
   let _ = Html.flush 0 in
   Html.close_block "TABLE" ;
   in_table := pop stack_table ;
   close_env "tabbing" ;
   main lexbuf}
 | [' ''\n']* ("\\>" | "\\=")  [' ''\n']*
    {if is_tabbing !in_table then begin
      Html.force_block "TD" "&nbsp;";
      Html.open_block "TD" ""
    end ;
    main lexbuf}
 |  [' ''\n']* "\\kill"  [' ''\n']*
    {if is_tabbing !in_table then begin
      Html.force_block "TD" "&nbsp;";
      Html.close_block "TR" ;
      Html.forget () ;
      Html.delay (fun _ -> ()) ;
      Html.open_block "TR" "" ;
      Html.open_block "TD" ""
    end ;
    main lexbuf}
(* tables and array *)
| "\\begin" ' '* ("{tabular}" | "{array}")
    {let lxm = lexeme lexbuf  in
    save_array_state ();
    let env = env_extract lxm in
    border := false ;
    skip_opt lexbuf ;
    let format = save_arg lexbuf in
    let format = Array.of_list (scan_this tformat format) in
    cur_format := format ;
    in_table := (if !border then Border else Table);
    let lexfun lb =
      if !display then force_item_display () ;
      push stack_display !display ;
      display := true ;
      if !border then
        Html.open_block "TABLE" "BORDER=1 CELLSPACING=0 CELLPADDING=1"
      else
        Html.open_block "TABLE" "CELLSPACING=2 CELLPADDING=0" ;
      open_row() ;
      open_first_col main ;
      main lb in
    new_env env ;
    skip_blanks lexbuf ; lexfun lexbuf}
  |  [' ' '\n']* "\\\\"? [' ' '\n']* "\\end" ' '* ("{tabular}" | "{array}")
      {let lxm = lexeme lexbuf in
      close_last_col main "" ;
      close_row () ;
      let env = env_extract lxm in
      if env = !cur_env then begin
        Html.close_block "TABLE" ;
        restore_array_state () ;
        display := pop stack_display;
        if !display then force_item_display () ;
        close_env env
      end else begin
        error_env env !cur_env ;
      end ;
      main lexbuf}
  | "\\left"
      {if !display then begin
        let _ = end_item_display () in
        let delim = Save.arg lexbuf in
        Html.delay (fun vsize ->
          put_delim delim vsize) ;
        begin_item_display ()
      end ;     
      main lexbuf}
  | "\\right"
      {if !display then begin
        let delim = Save.arg lexbuf in
        let vsize = end_item_display () in
        put_delim delim vsize ;
        Html.flush vsize ;
        begin_item_display ()
      end ;
      skip_blanks lexbuf;
      main lexbuf}
  | "\\over"
      {if !display then begin
        let mods = Html.insert_vdisplay
          (fun () ->
             open_vdisplay () ;
             open_vdisplay_row "ALIGN=center") in
        close_vdisplay_row () ;
        open_vdisplay_row "ALIGN=center" ;
        Html.close_mods () ;
        Html.put "<HR NOSHADE SIZE=1>" ;
        close_vdisplay_row () ;
        open_vdisplay_row "ALIGN=center" ;
        Html.close_mods () ;
        Html.open_mods mods ;
        Html.freeze
          (fun () ->
            close_vdisplay_row () ;
            close_vdisplay ()) ;
        main lexbuf        
      end else begin
        Html.put "/" ;
        main lexbuf
      end}
  |  [' ''\n']* "\\hline" [' ''\n']* ("\\\\"  [' ''\n']*)?
     {if !in_table = Table then
       do_hline main ;
     main lexbuf}
  | [' ''\n']* "&"  [' ''\n']*
     {if is_table !in_table  then begin
        close_col main "&nbsp;"; 
        open_col main
     end ;
     main lexbuf}
  | ['\n'' ']* "\\\\"
      {let _ = parse_args_opt [""] lexbuf in
      if is_table !in_table  then begin
         close_last_col main "&nbsp;" ; close_row () ;
         open_row () ; open_first_col main
      end else if is_tabbing !in_table then begin
        Html.force_block "TD" "&nbsp;";
        Html.close_block "TR" ;
        Html.flush 0;
        Html.delay (fun _ -> ()) ;
        Html.open_block "TR" "" ;
        Html.open_block "TD" ""
      end else begin
        Html.skip_line ()
      end ;
      skip_blanks lexbuf ; main lexbuf}
  | ['\n'' ']* "\\multicolumn" 
      {let n = Save.arg lexbuf in      
      let format = scan_this tformat (Save.arg lexbuf) in
      let n = try 
        my_int_of_string n
        with Failure _ -> raise (Failure "multicolumn") in
      do_multi n (Array.of_list format) main ;
      main lexbuf}
      
      
(* environments *)
|   "\\begin" " "* "{" ['A'-'Z' 'a'-'z']+ '*'?"}"
    {let lxm = lexeme lexbuf in
    let env = env_extract lxm in
    if env = "document" && !prelude then begin
      Image.put "\\begin{document}\n";
      prelude := false ;
      Html.forget_par () ;
      Html.set_out !out_file
   end ;
    let lexfun = match env with
      "program" | "verbatim" ->
         (fun lexbuf -> Html.open_block "PRE" "" ;
         skip_blanks lexbuf ; verbenv lexbuf)
    | _ ->
      let macro = "\\"^env in
      (fun lb ->
         if env = "alltt" then begin
           alltt := true ;
           Html.open_block "PRE" ""
         end else if env <> "document" then
           open_top_group () ;
         save_lexstate ();
         scan_this_may_cont true main lexbuf macro ;
         restore_lexstate ();
         main lb) in
    new_env env ;
    lexfun lexbuf}
|  "\\end" " " * "{" ['A'-'Z' 'a'-'z']+ '*'? "}"
    {let lxm = lexeme lexbuf in
    let env = env_extract lxm in
    scan_this main ("\\end"^env) ;
    if env = "alltt" then  begin
      alltt := false ;
      Html.close_block "PRE"
    end else (if env <> "document" then close_top_group ()) ;
    close_env env ;
    main lexbuf}
| ("\\prog" | "\\verb" | "\\verb*") _
   {let lxm = lexeme lexbuf in
   verb_delim := String.get lxm (String.length lxm-1) ;
   Html.open_group "CODE" ;
   new_env "*verb" ;
   inverb lexbuf}
| "\\item" ' '*
    {let arg = save_opt "" lexbuf in
    Html.item (scan_this main) arg ;
    main lexbuf}
(* Ignore font definitions ... *)
  | "\\font" "\\" ['A'-'Z' 'a'-'z']+ ' '* '='? ' '* ['a'-'z' 'A'-'Z' '0'-'9']+
      {main lexbuf}
(* conditionals *)
  | "\\newif"
      {let arg = Save.arg lexbuf in
      newif arg ;
      main lexbuf}
  | "\\else"  {skip_false lexbuf}
  | "\\fi"    {skip_blanks lexbuf ; main lexbuf}

(* General case for commands *)
  | '#' ['1'-'9']
      {let arg = !stack.(Char.code (lexeme_char lexbuf 1) - Char.code '1') in
      if !verbose > 2 then
        prerr_endline ("Subst arg: <"^arg^">");
      scan_this_may_cont false main lexbuf arg ;
      main lexbuf}
  | "\\" (('@' ? ['A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])
      {let lxm = lexeme lexbuf in
      begin match lxm with
    (* Html primitives *)
       "\\@open" ->
         let tag = save_arg lexbuf in
         let arg = save_arg lexbuf in
         if tag="DISPLAY" then begin
           push stack_display !display;
           display := true ;
           open_display ()
         end else begin
           item_display () ;
           Html.open_block tag arg ;
           open_display ()
         end ;
         main lexbuf
      | "\\@close" ->
         let tag = save_arg lexbuf in
         if tag="DISPLAY" then begin
           close_display ();
           display := pop stack_display
         end else begin
           close_display () ;
           Html.close_block tag;
           item_display ()
         end ;
         main lexbuf
      | "\\@print" ->
         let arg = save_arg lexbuf in
         Html.put arg ;
         main lexbuf
      | "\\@notags" ->
         let arg = save_arg lexbuf in
         let arg = get_this main arg in
         let buff = Lexing.from_string arg in
         Html.put (Save.tagout buff)  ;
         main lexbuf
      | "\\@anti" ->
         let args = Save.cite_arg lexbuf in
         let rec do_rec = function
           [] -> []
         | arg::rest ->
           let r = do_rec rest in
           let env = get_style main arg in
           env @ r in
         let envs = do_rec args in
         Html.erase_mods envs ;
         main lexbuf
      | "\\@style"   ->
         let arg = save_arg lexbuf in
         Html.open_mod (Style arg) ;
         main lexbuf
      | "\\@fontcolor"   ->
         let arg = save_arg lexbuf in
         Html.open_mod (Color arg) ;
         main lexbuf
      | "\\@fontsize"   ->
         let arg = save_arg lexbuf in
         Html.open_mod (Font (my_int_of_string arg)) ;
         main lexbuf
      | "\\@nostyle" ->
         Html.nostyle () ;
         skip_blanks lexbuf ; main lexbuf
      | "\\@clearstyle" ->
         Html.clearstyle () ;
         skip_blanks lexbuf ; main lexbuf
      | "\\@incsize" ->
         let arg = save_arg lexbuf in
         let arg = my_int_of_string arg in
         inc_size arg ;
         main lexbuf
      | "\\htmlcolor" ->
         let arg = save_arg lexbuf in
         let arg = get_this main ("\\@nostyle "^arg) in
         Html.open_mod (Color ("\"#"^arg^"\"")) ;
         main lexbuf
      | "\\@defaultdt" ->
         let arg = save_arg lexbuf in
         Html.set_dt arg ;
         main lexbuf
      | "\\usecounter" ->
         let arg = save_arg lexbuf in
         Html.set_dcount arg ;
         main lexbuf
      | "\\@fromlib" ->
         let arg = save_arg lexbuf in
         start_lexstate ();
         Mylib.put_from_lib arg Html.put;
         restore_lexstate ();
         main lexbuf
      | "\\imageflush" ->
         let arg = save_opt "" lexbuf in
         iput_newpage arg ;
         skip_blanks lexbuf ; main lexbuf
      | "\\textalltt" ->
         let arg = save_arg lexbuf in
         let old = !alltt in
         scan_this main "\\mbox{" ;
         alltt := true ;
         Html.open_group "CODE" ;
         scan_this main arg ;
         Html.close_group () ;
         scan_this main "}" ;
         alltt := old ;
         main lexbuf
(* Bibliographies *)
      | "\\cite" ->
        let opt = save_opt "" lexbuf in
        let args = Save.cite_arg lexbuf in
        put_char '[' ;
        Html.open_group "CITE" ;
        let rec do_rec = function
          [] -> ()
        | [x] -> Html.loc_ref (get_this main (Auxx.bget x)) x
        | x::rest ->
            Html.loc_ref (get_this main (Auxx.bget x)) x ;
            put ", " ;
            do_rec rest in
        do_rec args ;
        if opt <> "" then begin
          Html.put ", " ;
          scan_this main opt ;
        end ;
        close_group () ;
        put_char ']' ;
        main lexbuf
(* Foot notes *)
        | "\\@footnotetext" ->
           start_lexstate () ; 
           let mark = save_arg lexbuf in
           let mark = get_this main ("\\@nostyle "^mark) in
           let text = save_arg lexbuf in
           let text = get_this main ("\\@clearstyle "^text) in
           let anchor = save_arg lexbuf in
           let anchor = get_this main anchor in
           Foot.register
             (my_int_of_string mark)
             (get_this main ("\\@footnotemark{\@fnmarknote}{"^mark^"}"))
             text anchor ;
           restore_lexstate ();
           main lexbuf
        | "\\@footnoteflush" ->
           let sec_here = save_arg lexbuf
           and sec_notes = get_this main "\\@nostyle\\@footnotelevel" in
           start_lexstate () ;
           Foot.flush (scan_this main) sec_notes sec_here ;
           restore_lexstate ();
           main lexbuf
(* Boxes *)
        | "\\newsavebox" ->
            let name = save_arg lexbuf in
            begin try def_macro name 0 [Print ""]
            with Latexmacros.Failed -> () end ;
            main lexbuf
        | "\\savebox" | "\\sbox" ->
            let name = save_arg lexbuf in
            if lxm = "\\savebox" then begin
              if not !silent then begin
                Location.print_pos () ;
                prerr_endline "Warning: savebox"
              end ;
              skip_opt lexbuf ;
              skip_opt lexbuf
            end ;
            let body = save_arg lexbuf in
            redef_macro name 0 [Print (get_this main body)] ;
            main lexbuf
        | "\\usebox" ->
            let arg = save_arg lexbuf in
            scan_this main arg ;
            main lexbuf
(* chars *)
        | "\\char" ->
           let arg = Save.num_arg lexbuf in
           if not !silent && (arg < 32 || arg > 127) then begin
             Location.print_pos () ;
             prerr_endline ("Warning: \\char");
           end ;
           Html.put_char (Char.chr arg) ;
           skip_blanks_pop lexbuf ; main lexbuf
        | "\\symbol" ->
           let arg = save_arg lexbuf in
           scan_this main ("\\char"^arg) ;
           main lexbuf
(* labels *)
        | "\\label" ->
           let save_last_closed = !last_closed in
           let lab = save_arg lexbuf in
           Html.loc_name lab "" ;
           last_closed := save_last_closed ;
           main lexbuf
(* index *)
        | "\\@index" ->
           let save_last_closed = !last_closed in
           let tag = save_opt "default" lexbuf in
           let arg = save_arg lexbuf in
           Index.treat tag arg ;
           last_closed := save_last_closed ;
           main lexbuf
        | "\\@printindex" ->
           start_lexstate () ;
           let tag =  save_opt "default" lexbuf in
           Index.print (scan_this main) tag ;
           restore_lexstate ();
           main lexbuf
        | "\\newindex" |  "\\renewindex" ->
           let tag = save_arg lexbuf in
           let suf = save_arg lexbuf in
           let _   = save_arg lexbuf in
           let name = save_arg lexbuf in
           Index.newindex tag suf name ;
           main lexbuf
(* Counters *)
        | "\\newcounter"  ->
            let name = save_arg lexbuf in
            let within = save_opt "" lexbuf in
            let within = get_this main within in
            Counter.def_counter name within ;
            scan_this main ("\\def\\the"^name^"{\\arabic{"^name^"}}") ;
            main lexbuf
        | "\\addtocounter" ->
           let name = save_arg lexbuf in
           let arg = save_arg lexbuf in
           Counter.add_counter name (my_int_of_string arg) ;
           main lexbuf
        | "\\setcounter" ->
           let name = save_arg lexbuf in
           let arg = save_arg lexbuf in
           let arg = get_this main ("\\@nostyle "^arg) in
           Counter.set_counter name (my_int_of_string arg) ;
           main lexbuf
        | "\\stepcounter" ->
           let name = save_arg lexbuf in
           Counter.step_counter name ;
           main lexbuf
        | "\\refstepcounter" ->
           let name = save_arg lexbuf in
           Counter.step_counter name ;
           Counter.setrefvalue (get_this main ("\\@nostyle\\the"^name)) ;
           main lexbuf
        | "\\value" ->
           let name = save_arg lexbuf in
           Html.put (string_of_int (Counter.value_counter name)) ;
           main lexbuf
(* terminal output *)
        | "\\typeout" ->
           let what = save_arg lexbuf in
           prerr_endline what ;
           main lexbuf
         | "\\warning" ->
           let what = save_arg lexbuf in
           if  not !silent then begin
             Location.print_pos () ;
             prerr_endline ("Warning: "^what)
           end ;
           main lexbuf
      | _ ->
      let rec exec = function
        [] -> ()
      | i::rest -> begin match i with
            Print str -> Html.put str
          | Print_arg i -> scan_this main (!stack.(i))
          | Print_fun (f,i) -> scan_this main (f !stack.(i))
          | Print_count (f,i) ->
              let c = Counter.value_counter !stack.(i) in
              Html.put (f c)
          | Test cell ->
              if not !cell then raise IfFalse
              else
                if !verbose > 2 then
                  prerr_endline "Seen if as true"
          | SetTest (cell,b) -> cell := b
          | ItemDisplay   -> force_item_display ()
          | Subst body ->
              if !verbose > 2 then
                prerr_endline ("user macro: "^body) ;            
              scan_this_may_cont true main lexbuf body
          | IfCond (b,t,f) ->
             if !verbose > 2 then
               prerr_endline ("IfCond: "^if !b then "true" else "false") ;
             if !b then exec t else exec f
          | Br -> Html.skip_line ()
          end ;
        exec rest in

        let name = name_extract lxm in
        let pat,body = find_macro name in
        let args = if name = "\\list" then
          let arg1 = save_arg lexbuf in
          let arg2 = save_quote_arg lexbuf in
          [|arg1 ; arg2 |]
          else make_stack name pat lexbuf in
        push stack_stack !stack ;
        stack := args;
        let is_limit = checklimits lexbuf ||  Latexmacros.limit name in
        if is_limit || Latexmacros.big name then begin
           let sup,sub = get_sup_sub lexbuf in
           let do_what =
             (fun () -> exec body ; stack := pop stack_stack) in
           if !display && is_limit then
             limit_sup_sub main do_what sup sub
           else if !display &&  Latexmacros.int name then
             int_sup_sub main do_what sup sub
           else
             standard_sup_sub main do_what sup sub ;
           main lexbuf
        end else begin
          try
            exec body ;
            stack := pop stack_stack ;
            if (!verbose > 2) then
              prerr_string ("Cont after macro "^name^": ") ;
            if Latexmacros.invisible name ||
              (not !in_math && not !alltt &&
              (pat = ([],[])) && last_letter name && !eat_space) then begin
              if !verbose > 2 then
                prerr_endline "skipping blanks";
              skip_blanks_pop lexbuf ;
              main lexbuf ;
            end else begin
              if !verbose > 2 then
                prerr_endline "not skipping blanks";
              main lexbuf
            end
          with 
           IfFalse -> begin
             stack := pop stack_stack ;
             if (!verbose > 2) then
               prerr_endline ("Cont after iffalse:"^name) ;
             skip_false lexbuf
          end
        end
     end}

| '<'         { Html.put "&lt;"; main lexbuf }
| '>'         { Html.put "&gt;"; main lexbuf }
| '~'         { Html.put "&nbsp;"; main lexbuf }
| '{'
    {open_top_group () ;  main lexbuf}
| '}'
    {close_top_group () ; main lexbuf}
| eof
   {if !verbose > 1 then Printf.fprintf stderr "Eof\n" ; ()}
| '\n'
  {(* if not (is_table  !in_table) then *) begin
    Html.put_char '\n'
  end ;
  main lexbuf}
| ' '+
   {(* if !display || is_table !in_table then
      Html.put "&nbsp;"
   else *)
      Html.put_char ' ';
   main lexbuf}
| ['a'-'z' 'A'-'Z']+
   {let lxm =  lexeme lexbuf in
   if !in_math then begin
      Html.put "<I>";      
      Html.put lxm;
      Html.put "</I>"
    end else
      Html.put lxm ;
    main lexbuf}
| "?`" {Html.put_char '¿' ; main lexbuf}
| "!`" {Html.put_char '¡' ; main lexbuf}
| _ 
   {let lxm = lexeme_char lexbuf 0 in
   Html.put_char lxm ; main lexbuf}

and rawhtml = parse
    "\\end{rawhtml}" { () }
  | _           { Html.put_char(lexeme_char lexbuf 0); rawhtml lexbuf }

and latexonly = parse
    "\\end{latexonly}" { () }
  | '%'+ ' '* ("END"|"end") ' '* ("LATEX"|"latex") [^'\n']* '\n' {()}
  | _           { latexonly lexbuf }

and verbenv = parse
  "\\end" " " * "{" ['a'-'z'] + "}"
    {let lxm = lexeme lexbuf in
    let env = env_extract lxm in
    if env = !cur_env then begin
      Html.close_block "PRE" ;
      close_env env ;
      skip_blanks lexbuf ; main lexbuf
    end else begin
      Html.put lxm ;
      verbenv lexbuf
    end}
| "\\esc" ' '*
    {if !cur_env <> "program" then begin
      Html.put (lexeme lexbuf)
    end else begin
      let arg = Save.arg lexbuf in
      scan_this main ("{"^arg^"}")
    end ;
    verbenv lexbuf}
| "<"         { Html.put "&lt;"; verbenv lexbuf }
| ">"         { Html.put "&gt;"; verbenv lexbuf }
| '\t'
      {for i=1 to !tab_val do
        Html.put_char ' '
      done ;
      verbenv lexbuf}
| eof {()}
| _   { Html.put_char (lexeme_char lexbuf 0) ; verbenv lexbuf}

and inverb = parse
  "<"         { Html.put "&lt;"; inverb lexbuf }
| ">"         { Html.put "&gt;"; inverb lexbuf }
| _
  {let c = lexeme_char lexbuf 0 in
  if c = !verb_delim then begin
    Html.close_group () ;
    close_env "*verb" ;
    main lexbuf
  end else begin
    Html.put_char c ;
    inverb lexbuf
  end}

and image = parse
  ".PE\n"
     {Image.put ".PE\n" ; Image.close_image  () ; main lexbuf}
|  '%'+ ' '* ("END"|"end") ' '+ ("IMAGE"|"image")  [^'\n']* '\n'
     {Image.put_char '\n' ; Image.close_image  () ;
     main lexbuf}
|  "\\end" ' '* "{toimage}"
     {Image.put_char '\n' ; Image.close_image  () ;
     skip_blanks lexbuf ; main lexbuf}
| _
     {let s = lexeme lexbuf in
     Image.put s ;
     image lexbuf}

and mbox_arg = parse
  ' '+ {mbox_arg lexbuf}
| eof
     {if !stack_lexbuf <> [] then begin
     let lexbuf = pop stack_lexbuf in
     eat_space := pop stack_eat ;
     if !verbose > 2 then begin
       prerr_endline "Poping lexbuf in mbox_arg" ;
       pretty_lexbuf lexbuf
     end ;
     mbox_arg lexbuf
   end else failwith "Eof in mbox_arg"}
| '{' | ("\\bgroup" ' '* '\n'? ' '*)
    {let open_fun  () =
       if !display then begin
         item_display () 
       end ;
       push stack_display !display ;
       display := false ;
       Html.open_group "" in
    push stack_table !in_table ;
    in_table := NoTable ;
    if !in_math then begin
       push stack_in_math !in_math ;
       in_math := false ;
       new_env "*mbox" ;
    end else begin
      new_env "*hbox" ;
    end ;
    open_fun() ;
    main lexbuf}

and skip_blanks_pop = parse
  ' '+ {skip_blanks_pop lexbuf}
| '\n' {more_skip_pop lexbuf}
| ""   {()}
| eof
   {if !stack_lexbuf <> [] && !eat_space then begin
     let lexbuf = pop stack_lexbuf in
     eat_space := pop stack_eat ;
     if !verbose > 2 then begin
       prerr_endline "Poping lexbuf in skip_blanks" ;
       pretty_lexbuf lexbuf
     end ;
     skip_blanks_pop lexbuf
   end else ()}

and more_skip_pop = parse
  '\n'+ {top_par ()}
| ""    {skip_blanks_pop lexbuf}
| eof
   {if !stack_lexbuf <> [] && !eat_space then begin
     let lexbuf = pop stack_lexbuf in
     eat_space := pop stack_eat ;
     if !verbose > 2 then begin
       prerr_endline "Poping lexbuf in skip_blanks" ;
       pretty_lexbuf lexbuf
     end ;
     more_skip_pop lexbuf
   end else ()}

and skip_blanks = parse
  ' '+ {skip_blanks lexbuf}
| '\n' {more_skip lexbuf}
| ""   {()}


and more_skip = parse
  '\n'+ {top_par ()}
| ""    {skip_blanks lexbuf}

and skip_spaces_main = parse
  ' ' * {main lexbuf}
| eof   {main lexbuf}


and skip_false = parse
  "\\if" ['a'-'z' 'A'-'Z']+
     {if_level := !if_level + 1 ;
     skip_false lexbuf}
| "\\else" ['a'-'z' 'A'-'Z']+
     {skip_false lexbuf}
| "\\else"
     {if !if_level = 0 then (skip_blanks lexbuf ; main lexbuf)
     else skip_false lexbuf}
| "\\fi" ['a'-'z' 'A'-'Z']+
     {skip_false lexbuf}
| "\\fi"
     {if !if_level = 0 then (skip_blanks lexbuf ; main lexbuf)
     else begin
       if_level := !if_level -1 ;
       skip_false lexbuf
     end}
| _ {skip_false lexbuf}

and checklimits = parse
  "\\limits"   {true}
| "\\nolimits" {false}
| ""           {false}

and comment = parse
  ' '* ("BEGIN"|"begin") ' '+ ("IMAGE"|"image")
    {skip_comment lexbuf ; Image.dump "" image lexbuf}
| ' '* "\\begin" ' '* "{toimage}"
    {Image.dump "" image lexbuf}
| ' '* ("HEVEA"|"hevea") ' '*
   {main lexbuf}
| ' '* ("BEGIN"|"begin") ' '+ ("LATEX"|"latex")
    {skip_comment lexbuf ; latexonly lexbuf ; skip_spaces_main lexbuf}
| ""
    {skip_comment lexbuf ; skip_spaces_main lexbuf}

and skip_comment = parse    
   [^ '\n']* '\n'
   {if !verbose > 1 then
     prerr_endline ("Comment:"^lexeme lexbuf) ;
   if !flushing then Html.flush_out () }
| "" {failwith "Comment not terminated"}

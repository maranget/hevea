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
open Parse_opts
open Lexing
open Myfiles
open Latexmacros
open Html

let header = "$Id: latexscan.mll,v 1.53 1998-10-27 16:22:41 maranget Exp $" 


let prerr_args args =
  prerr_endline "Arguments: " ;
  for i = 0 to Array.length args - 1 do
    prerr_string "\t``" ;
    prerr_string args.(i) ;
    prerr_endline "''"
  done ;
  prerr_endline "End of arguments" ;
;;

let pretty_lexbuf lb =
  let  pos = lb.lex_curr_pos and len = String.length lb.lex_buffer in
  prerr_endline "Buff contents:" ;
  prerr_endline ("<<"^String.sub lb.lex_buffer pos (len-pos)^">>");
  prerr_endline ("curr_pos="^string_of_int lb.lex_curr_pos);
  prerr_endline "End of buff"
;;

let prerr_stack_string s l =
  let rec do_rec = function
    [] -> prerr_endline ">>"
  | [s] -> prerr_string ("``"^s^"''") ; prerr_endline ">>"
  | s::r ->
      prerr_string "``" ;
      prerr_string s ;
      prerr_string "'' " ;
      do_rec r in
        
  prerr_string s ;
  prerr_string ": <<" ;
  do_rec l
;;

let push s e = s := e:: !s
and pop s = match !s with
  [] -> failwith "Empty stack"
| e::rs -> s := rs ; e
and top s = match !s with
  [] -> failwith "Empty stack in top"
| e::_ -> e
;;

(* stack for recoding lexbuf *)
let stack_lexbuf = ref []
;;

(* arguments inside macros*)
let stack = ref [||]
and stack_stack = ref []
and stack_stack_stack = ref []
;;

let eat_space = ref true
and stack_eat = ref []
;;

(* Recoding and restoring lexbufs *)
let record_lexbuf lexbuf eat =
  push stack_eat !eat_space ;
  push stack_stack_stack (!stack,!stack_stack) ;
  push stack_lexbuf lexbuf ;
  eat_space := eat

and previous_lexbuf () =
  let lexbuf = pop stack_lexbuf
  and s,ss = pop stack_stack_stack in
  eat_space := pop stack_eat ;
  stack := s ; stack_stack := ss ;
  lexbuf
;;

(* Saving and restoring lexing status *)
let stack_stack_lexbuf = ref []
;;

let save_lexstate () =
  let old_eat = !stack_eat
  and old_stack = !stack_stack_stack in
  push stack_eat !eat_space ;
  push stack_stack_stack (!stack,!stack_stack) ;
  push stack_stack_lexbuf (!stack_lexbuf,!stack_eat,!stack_stack_stack) ;
  stack_eat := old_eat ; stack_stack_stack := old_stack

and restore_lexstate () =
  let l,s,args = pop stack_stack_lexbuf in
  stack_lexbuf := l ;
  stack_eat := s;
  eat_space := pop stack_eat ;
  stack_stack_stack := args ;
  let s,ss = pop stack_stack_stack in
  stack := s ;
  stack_stack := ss
  
;;

(* Blank lexing status *)
let start_lexstate () =
  save_lexstate () ;
  stack_lexbuf := [] ;
  eat_space := false
;;



let out_file = ref (Out.create_null ())
;;

let prelude = ref true
;;

let flushing = ref false
;;


let my_int_of_string s =
  try int_of_string s with
  Failure m -> raise (Failure (m^": ``"^s^"''"))
;;

let env_extract s =
  let i = String.index s '{'
  and j = String.rindex s '}' in
  String.sub s (i+1) (j-i-1)
;;

let last_letter name =
  let c = String.get name (String.length name-1) in
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
;;

let top_par () =
  if not (!display || !in_math) then Html.par ();
;;

let save_arg lexbuf =

  let rec save_rec lexbuf =
    try Save.arg lexbuf
    with Save.BadParse "EOF" -> begin
        if !stack_lexbuf = [] then failwith "Eof while looking for arg";
        let lexbuf = previous_lexbuf () in
        if !verbose > 2 then begin
          prerr_endline "popping stack_lexbuf in save_arg";
          pretty_lexbuf lexbuf ;
          prerr_args !stack
        end;
        save_rec lexbuf end in

  Save.seen_par := false ;
  save_lexstate () ;
  let arg = save_rec lexbuf in
  restore_lexstate () ;
  if !verbose > 2 then
    prerr_endline ("Arg parsed: ``"^arg^"''") ;
  arg
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


let parse_quote_arg_opt def lexbuf =

  let rec save_rec lexbuf = 
    try Yes (Save.opt lexbuf) with
      Save.NoOpt -> No def
    | Save.BadParse "EOF" -> begin
        if !stack_lexbuf = [] then No def
        else let lexbuf = previous_lexbuf () in
        if !verbose > 2 then begin
          prerr_endline "poping stack_lexbuf in parse_quote_arg_opt";
          pretty_lexbuf lexbuf
        end;
        save_rec lexbuf end in
  
  save_lexstate () ;
  let r = save_rec lexbuf in
  restore_lexstate () ;
  if !verbose > 2 then begin
     Printf.fprintf stderr "Parse opt : %s" (pretty_ok r) ;
     prerr_endline ""
  end ;
  Save.seen_par := false ;
  r
;;

let rec parse_args_norm pat lexbuf = match pat with
  [] -> []
| s :: pat ->
    let arg = save_arg lexbuf in
    let r = parse_args_norm pat lexbuf in
     arg :: r
;;


let parse_arg_opt def lexbuf =
  let arg = parse_quote_arg_opt def lexbuf in
(*
   (match arg with Yes s -> Yes (subst_arg s)
           | No s -> No (subst_arg s))
*)
  arg
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
  let args = Array.of_list (List.map from_ok opts@args) in
  if !verbose > 2 then begin
    Printf.fprintf stderr "make_stack for macro: %s "  name ;
    Latexmacros.pretty_pat pat ;
    prerr_endline "";
    for i = 0 to Array.length args-1 do
      Printf.fprintf stderr "\t#%d = %s\n" (i+1) args.(i)
    done ;
    prerr_args !stack
  end ;
  args
;;


let rec pretty_opts = function
  [] -> ""
| Save.NoMath::r -> "{NoMath}"^pretty_opts r
| Save.Wrap::r -> "{Wrap}"^pretty_opts r
;;

let pretty_format = function
  Save.Align (s,o) -> s^pretty_opts o
| Save.Inside s -> "@{"^s^"}"
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

let top_open_display () =
  if !display then begin
    if !verbose > 1 then
       prerr_endline "open display" ;
    Html.open_display (display_arg !verbose)
  end

and top_item_display () =
  if !display then begin
    Html.item_display ()
  end
;;

let top_close_display () =
  if !display then begin
    Html.close_display ()
  end

and top_erase_pending_display () =
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
  Html.close_block "TABLE"

and open_vdisplay_row s =
  if !verbose > 1 then
    prerr_endline "open_vdisplay_row";
  Html.open_block "TR" "" ;
  Html.open_block "TD" s ;
  Html.open_display (display_arg !verbose)

and close_vdisplay_row () =
  if !verbose > 1 then
    prerr_endline "close_vdisplay_row";
  Html.close_display () ;
  Html.force_block "TD" "&nbsp;" ;
  Html.close_block "TR"
;;


let open_center () =  Html.open_block "DIV" "ALIGN=center"
and close_center () = Html.close_block "DIV"
;;

(* Latex environment stuff *)
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
  prerr_endline ("Latex env error: ``"^close_e^"'' closes ``"^open_e^"''") ;
  prerr_stack_string "Error: env stack is"
    (List.map (fun (x,_) -> x) !stack_env) ;
  raise (Failure "Unbalanced Latex env")
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
  record_lexbuf lexbuf eat;

  let lexer = Lexing.from_string s in
  let r = lexfun lexer in

  restore_lexstate ();
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_may_cont : over" ;
    prerr_endline ""
  end ;
  r
;;

let scan_this_arg old_stack lexfun lexbuf s =
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_arg : [%s]" s ;
    prerr_endline "" ;
    if !verbose > 2 then begin
      prerr_endline "Pushing lexbuf" ;
      pretty_lexbuf lexbuf
    end
  end ;
  save_lexstate ();

  push stack_stack !stack ;
  stack := old_stack ;
  record_lexbuf lexbuf false;
  stack := pop stack_stack ;

  let lexer = Lexing.from_string s in
  let r = lexfun lexer in

  restore_lexstate ();
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_arg : over" ;
    prerr_endline ""
  end ;
  r
;;

let get_this lexfun s =
  start_lexstate ();
  push stack_display !display;
  display := false;
  if !verbose > 1 then
    prerr_endline ("get_this : ``"^s^"''") ;
  let lexer = Lexing.from_string ("\\mbox{"^s^"}") in
  let r = Html.to_string (fun () -> lexfun lexer) in
  if !verbose > 1 then begin
    prerr_endline ("get_this ``"^s^"'' -> ``"^r^"''")
  end ;
  display := pop stack_display;
  restore_lexstate();
  r
;;

let subst_buff = Out.create_buff ()
;;

let subst_this subst arg =
try
  let _ = String.index arg '#' in
  if !verbose > 1 then begin
    Printf.fprintf stderr "subst_this : [%s]\n" arg ;
    prerr_args !stack
  end ;
  subst (Lexing.from_string arg) ;
  let r = Out.to_string subst_buff in
  if !verbose > 1 then
    prerr_endline ("subst_this ["^arg^"] = "^r);
  r
with Not_found -> arg
;;

let subst_arg subst lexbuf = subst_this subst (save_arg lexbuf)  
and subst_opt def subst lexbuf = subst_this subst (save_opt def lexbuf)  
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
    Html.begin_item_display (fun () -> ()) false ;
    Symb.put_delim Html.skip_line Html.put delim i ;
    let _ = Html.end_item_display () in ()
  end
;;

let default_format = Save.Align ("left",[])
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
| Save.NoMath::_ -> false
| _::r -> keep_math r

let rec is_par = function
  [] -> false
| Save.Wrap::_ -> true
| _::r -> is_par r
;;

let is_inside = function
  Save.Inside _ -> true
| _ -> false

and as_inside = function
  Save.Inside s -> s
| _        -> ""

and as_align = function
  Save.Align (s,o) ->
    if is_par o then "VALIGN=top ALIGN="^s else "NOWRAP ALIGN="^s
| _       -> failwith "as_align"

and is_display = function
  Save.Align (_,o) -> keep_math o
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
      Save.Inside s ->
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
  top_open_display ();
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
  top_erase_pending_display () ;
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
  top_erase_pending_display () ;
  Html.change_block "TD" args  ;
  top_open_display ()
;;

let do_multi n format main =
  if !verbose > 1 then begin
    prerr_string
      ("multicolumn: n="^string_of_int n^" format:") ;
    pretty_formats format ;
    prerr_endline ""
  end ;

  if !first_col then begin
    top_close_display () ;
    Html.close_block "TD" ;
    Html.erase_block "" ;
    Html.open_group "" ;
    Html.open_block "TD" "" ;
    top_open_display ()
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
  top_close_display ();
  Html.force_block "TD" content ;
  if !first_col then begin
    first_col := false ;
    Html.close_group ()
  end ;
  cur_col := !cur_col + 1;
  cur_col := show_inside main !cur_format !cur_col
;;

let close_last_col main content =
  let old_format = get_col !cur_format !cur_col in
  if not (is_display old_format) then begin
    in_math := pop stack_in_math
  end ;
  top_close_display ();
  if !first_col && Html.is_empty () then begin
    Html.erase_block "TD" ;
    Html.close_group () ;    
    cur_col := !cur_col + 1
  end else begin
    Html.force_block "TD" content ;
    if !first_col then begin
      first_col := false ;
      Html.close_group ()
    end ;
    cur_col := !cur_col + 1;
    cur_col := show_inside main !cur_format !cur_col
  end

and close_last_row () =
  if !first_col then
    Html.erase_block "TR"
  else
    Html.close_block "TR"
;;

      
(* Top functions for blocks *)
let no_display = function
  "TABLE" | "TR" | "TD" | "DISPLAY" -> true
|  _ -> false
;;

let top_open_block block args =
  if !verbose > 2 then prerr_endline ("Top open: "^block);
  push stack_table !in_table ;
  in_table := NoTable ;
  begin match block with
    "PRE" ->
      push stack_display !display ;
      if !display then begin
        Html.item_display () ;
        display := false
      end ;
      Html.open_block "PRE" args
  | _ ->
      if !display then begin
        Html.item_display () ; Html.open_block block args ;
        Html.open_display (display_arg !verbose)
      end else
        Html.open_block block args
  end
and top_close_block block =
  if !verbose > 2 then prerr_endline ("Top close: "^block) ;
  in_table := pop stack_table ;
  begin match block with
    "PRE" ->
      display := pop stack_display ;
      Html.close_block block ;
      if !display then Html.item_display ()
  | _ ->
      if !display then begin
        Html.close_display () ; Html.close_block block ; Html.item_display ()
      end else
        Html.close_block block
  end
;;

let top_open_group () =
  top_open_block "" "" ; new_env ""

and top_close_group () =
  if !cur_env = "*mbox" then begin
    top_close_block "" ;
    in_math := pop stack_in_math ; display := pop stack_display ;
    if !display then Html.item_display () ;
    close_env "*mbox"
  end else begin
    top_close_block "" ;
    close_env ""
  end
;;

(* Image stuff *)

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


let stack_entry = ref []
and stack_out = ref []
;;

let start_other_scan env lexfun lexbuf =
  if !verbose > 1 then begin
    prerr_stack_string ("Start other scan ("^env^"), env stack is")
      (List.map (fun (x,_) -> x) !stack_env) ;
    prerr_endline ("Current env is: ``"^ !cur_env^"''") ;
    prerr_stack_string "Entry stack is" !stack_entry
  end;
  save_lexstate () ;
  push stack_entry env ;
  stack_entry := List.rev !stack_entry ;
  lexfun lexbuf
;;

let start_image_scan s image lexbuf =
  start_other_scan "toimage" (fun b -> Image.dump s image b) lexbuf
;;

let complete_scan main lexbuf =
  main lexbuf ;
  close_env (pop stack_out) ;
  top_close_block "" ;
  if !verbose > 1 then begin
    prerr_stack_string "Complete scan: env stack is"
      (List.map (fun (x,_) -> x) !stack_env) ;
    prerr_endline ("Current env is: ``"^ !cur_env^"''")
  end
;;


let stop_other_scan main lexbuf =
  if !verbose > 1 then begin
    prerr_stack_string "Stop image: env stack is"
      (List.map (fun (x,_) -> x) !stack_env) ;
    prerr_endline ("Current env is: ``"^ !cur_env^"''")
  end;
  let _ = pop stack_entry in
  if !stack_out <> []  then begin
    complete_scan main lexbuf ;
    while !stack_out <> []  do
      let lexbuf = previous_lexbuf () in
      complete_scan main lexbuf
    done
  end ;
  restore_lexstate ()
;;


(* maths *)
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
      open_vdisplay_row "NOWRAP" ;
      open_script_font () ;
      scan_this main sup ;
      close_vdisplay_row ()
    end ;           
    open_vdisplay_row "" ;
    what ();
    close_vdisplay_row () ;
    if sub <> "" then begin
      open_vdisplay_row "NOWRAP" ;
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

let int_sup_sub something vsize main what sup sub =
    if something then begin
      force_item_display () ;
      what () ;
      force_item_display ()
    end ;
    open_vdisplay () ;
    open_vdisplay_row "ALIGN=left" ;
    open_script_font () ;
    scan_this main sup ;
    close_vdisplay_row () ;
    open_vdisplay_row "ALIGN=left" ;
    for i = 2 to vsize do
      Html.skip_line ()
    done ;
    close_vdisplay_row () ;
    open_vdisplay_row "ALIGN=left" ;
    open_script_font () ;
    scan_this main sub ;
    close_vdisplay_row () ;
    close_vdisplay () ;
    force_item_display ()
;;

let includes_table = Hashtbl.create 17
and check_includes = ref false
;;

let add_includes l =
  check_includes := true ;
  List.iter (fun x -> Hashtbl.add includes_table x ()) l
;;


let check_include s =
  not !check_includes ||
  begin  try
    Hashtbl.find includes_table s ; true
  with Not_found -> false
  end
;;

let input_file loc_verb main filename =
  try
    let filename,input = Myfiles.open_tex filename in
    if !verbose > 0 then
      prerr_endline ("Input file: "^filename) ;
    let buf = Lexing.from_channel input in
    Location.set filename buf ;
    new_env "*input" ;
    let old_verb = !verbose in
    verbose := loc_verb ;
    main buf ;
    verbose := old_verb ;
    Location.restore () ;
    close_env "*input"
  with Myfiles.Except -> begin
    if !verbose > 0 then
      prerr_endline ("Not opening file: "^filename) ;
    raise  Myfiles.Except
  end
 | Myfiles.Error m as x -> begin
     warning m ;
     raise x
 end
;;

let no_prelude () =
  flushing := true ;
  prelude := false ;
  Html.forget_par () ;
  Html.set_out !out_file
;;

let r_quote = String.create 1
;;

let quote_char = function
  '<' -> "&lt;"
| '>' -> "&gt;"
| '&' -> "&amp;"
| c   -> (r_quote.[0] <- c ; r_quote)
;;

} 

let command_name = '\\' (('@' ? ['A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])

rule  main = parse
(* comments *)
   '%'+ {comment lexbuf}
(* Styles and packages *)
| "\\documentstyle"  | "\\documentclass"
    {let command = lexeme lexbuf in
    Save.start_echo () ;
    let opt = parse_quote_arg_opt "" lexbuf in
    let arg =  save_arg lexbuf in
    let echo_args = Save.get_echo () in
    begin try if not !Latexmacros.styleloaded then
      input_file 0 main (arg^".sty") with
    Myfiles.Except | Myfiles.Error _ ->
      warning "no base style file"
    end ;
    Image.start () ;
    Image.put command ;
    Image.put echo_args ;
    Image.put "\n" ;
    main lexbuf}
(* Paragraphs *)
  | "\n\n" '\n' *
    {if !alltt then begin
      Html.put (lexeme lexbuf)
    end else  top_par () ;
    main lexbuf }
| "\\input" | "\\include" | "\\bibliography"
     {let lxm = lexeme lexbuf in
     Save.start_echo () ;
     let arg = Save.input_arg lexbuf in
     let echo_arg = Save.get_echo () in
     if lxm <> "\\include" || check_include arg then begin
       let filename =
         if lxm = "\\bibliography" then Location.get_base ()^".bbl"
         else arg in
       begin try input_file !verbose main filename
       with Myfiles.Except ->
           Image.put lxm ;
           Image.put echo_arg ;
           Image.put "\n" ;
       | Myfiles.Error _ -> ()           
       end
     end;
     main lexbuf}
| "\\verbatiminput"
      {let lxm = lexeme lexbuf in
      let tabs = my_int_of_string (save_opt "8" lexbuf) in      
      let arg = save_arg lexbuf in
      let old_tabs = !tab_val in
      tab_val := tabs ;
      top_open_block "PRE" "" ;
      begin try
        input_file !verbose verbenv arg ;
      with
        Myfiles.Except | Myfiles.Error _ -> ()
      end ;
      top_close_block "PRE" ;
      tab_val := old_tabs ;
      main lexbuf}
| "\\usepackage"
      {let lxm = lexeme lexbuf in
      Save.start_echo () ;
      let _ = save_opt "" lexbuf in
      let _ = save_arg lexbuf in
      Image.put lxm ;
      Image.put (Save.get_echo ()) ;
      Image.put "\n" ;
      main lexbuf}
(* subscripts and superscripts *)
  | ('_' | '^')
     {let lxm = lexeme lexbuf in
     if !alltt then Html.put lxm
     else begin
       let sup,sub = match lxm with
         "^" ->
           let sup = Save.arg lexbuf in
           let sub = Save.get_sub lexbuf in
           sup,sub
       | _   ->
           let sub = Save.arg lexbuf in
           let sup = Save.get_sup lexbuf in
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
           Html.open_display (display_arg !verbose)
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
     let args_pat = Save.defargs lexbuf in
     let body = save_arg lexbuf in
     if !env_level = 0 || lxm <> "\\def" then
       Image.put
         (lxm^name^
         (List.fold_right (fun s r -> s^r) args_pat ("{"^body^"}\n"))) ;
     begin try
       def_macro_pat name ([],args_pat) [Subst body] ;
       if lxm = "\\def" then macro_register name
     with Latexmacros.Failed -> () end ;
     main lexbuf}
  | "\\renewcommand" | "\\newcommand" | "\\providecommand"
    {let lxm = lexeme lexbuf in
    Save.start_echo () ;
    let name = Save.csname lexbuf in
    let nargs = parse_args_opt ["0" ; ""] lexbuf in
    let body = save_arg lexbuf in
    if (!env_level = 0) then
      Image.put
        (lxm^Save.get_echo ()^"\n") ;
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
  | "\\newenvironment" | "\\renewenvironment"
     {let lxm = lexeme lexbuf in
     Save.start_echo () ;
     let name = save_arg lexbuf in
     let nargs = parse_quote_arg_opt "0" lexbuf in
     let optdef = parse_quote_arg_opt "" lexbuf in
     let body1 = save_arg lexbuf in
     let body2 = save_arg lexbuf in
     if !env_level = 0 then
       Image.put (lxm^Save.get_echo ()^"\n") ;
     begin try
       (match lxm with
          "\\newenvironment" -> def_env_pat
       |  _ -> redef_env_pat) name
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
      Save.start_echo () ;
      let name = save_arg lexbuf in
      let numbered_like = parse_quote_arg_opt "" lexbuf in
      let caption = save_arg lexbuf in
      let within = parse_quote_arg_opt "" lexbuf in
      if !env_level = 0 then
        Image.put (lxm^Save.get_echo ()^"\n") ;
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
     Save.skip_equal lexbuf ;
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
(* opening and closing environments *)
| "\\begin"
   {let env = save_arg lexbuf in
   begin match env with
     "rawhtml" -> rawhtml lexbuf; main lexbuf
   |  "verblatex" ->
       verblatex lexbuf ; main lexbuf
   | "latexonly" ->
       start_other_scan "latexonly" latexonly lexbuf ;
       main lexbuf
   | "verbimage" ->
       verbimage lexbuf ; main lexbuf
   | "toimage" ->
       start_image_scan "" image lexbuf ;
       main lexbuf
   | "tabbing" ->
        let lexfun lb =
          Html.open_block "TABLE" "CELLSPACING=0 CELLPADDING=0" ;
          Html.open_block "TR" "" ;
          Html.open_block "TD" "" ;
          main lb in
        push stack_table !in_table ;
        in_table := Tabbing ;
        new_env "tabbing" ;
        lexfun lexbuf
   |  "tabular" | "array" ->
        save_array_state ();
        Save.border := false ;
        skip_opt lexbuf ;
        let format = save_arg lexbuf in
        let format = Array.of_list (scan_this Save.tformat format) in
        cur_format := format ;
        in_table := (if !Save.border then Border else Table);
        let lexfun lb =
          if !display then item_display () ;
          push stack_display !display ;
          display := true ;
          if !Save.border then
            Html.open_block
              "TABLE" "BORDER=1 CELLSPACING=0 CELLPADDING=1"
          else
            Html.open_block "TABLE" "CELLSPACING=2 CELLPADDING=0" ;
          open_row() ;
          open_first_col main ;
          main lb in
        new_env env ;
        skip_blanks lexbuf ;
        lexfun lexbuf
    |  _ ->
        if env = "document" && !prelude then begin
          Image.put "\\pagestyle{empty}\n\\begin{document}\n";
          prelude := false ;
          Html.forget_par () ;
          Html.set_out !out_file
        end ;
        let lexfun = match env with
          "program" | "verbatim" ->
            (fun lexbuf -> top_open_block "PRE" "" ;
              verbenv lexbuf)
        | _ ->
            let macro = "\\"^env in
            (fun lb ->
              if env = "alltt" then begin
                alltt := true ;
                top_open_block "PRE" ""
              end else if env <> "document" then
                top_open_block "" "" ;
              let old_envi = !stack_entry in
              push stack_entry env ;
              scan_this_may_cont true main lexbuf macro ;
              stack_entry := old_envi ;
              main lb) in
            new_env env ;
            lexfun lexbuf
    end}
| "\\end"
    {let env = save_arg lexbuf in
    begin match env with
      "tabbing" ->
        Html.close_block "TD" ;
        Html.close_block "TR" ;
        Html.close_block "TABLE" ;
        in_table := pop stack_table ;
        close_env "tabbing" ;
        main lexbuf
    | "array" | "tabular" ->
        close_last_col main "" ;
        close_last_row () ;
        if env = !cur_env then begin
          Html.close_block "TABLE" ;
          restore_array_state () ;
          display := pop stack_display;
          if !display then item_display () ;
          close_env env
        end else begin
          error_env env !cur_env ;
        end ;
        main lexbuf
    | _ ->
        scan_this main ("\\end"^env) ;
        if env = "alltt" then  begin
          alltt := false ;
          top_close_block "PRE"
        end else (if env <> "document" then top_close_block "") ;
        close_env env ;
        if env <> "document" then main lexbuf
    end}
(* inside tabbing *)
 | [' ''\n']* ("\\>" | "\\=")  [' ''\n']*
    {if is_tabbing !in_table then begin
      Html.force_block "TD" "&nbsp;";
      Html.open_block "TD" ""
    end ;
    main lexbuf}
 |  [' ''\n']* "\\kill"  [' ''\n']*
    {if is_tabbing !in_table then begin
      Html.force_block "TD" "&nbsp;";
      Html.erase_block "TR" ;
      Html.open_block "TR" "" ;
      Html.open_block "TD" ""
    end ;
    main lexbuf}
(* inside tables and array *)
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
         close_col main "&nbsp;" ; close_row () ;
         open_row () ; open_first_col main
      end else if is_tabbing !in_table then begin
        Html.force_block "TD" "&nbsp;";
        Html.close_block "TR" ;
        Html.open_block "TR" "" ;
        Html.open_block "TD" ""
      end else begin
        Html.skip_line ()
      end ;
      skip_blanks lexbuf ; main lexbuf}
  | ['\n'' ']* "\\multicolumn" 
      {let n = save_arg lexbuf in      
      let format = scan_this Save.tformat (save_arg lexbuf) in
      let n = try 
        my_int_of_string n
        with Failure _ -> raise (Failure "multicolumn") in
      do_multi n (Array.of_list format) main ;
      main lexbuf}
  | "\\left"
      {if !display then begin
        let _,f,is_freeze = Html.end_item_display () in
        let delim = save_arg lexbuf in
        Html.delay (fun vsize ->
          put_delim delim vsize) ;
        begin_item_display f is_freeze
      end ;     
      main lexbuf}
  | "\\right"
      {if !display then begin
        let delim = save_arg lexbuf in
        let vsize,f,is_freeze = end_item_display () in
        put_delim delim vsize;
        Html.flush vsize ;
        begin_item_display f is_freeze ;
        let sup,sub = Save.get_sup_sub lexbuf in
        if sup <> "" || sub <> "" then
          let do_what = (fun () -> ()) in
          int_sup_sub false vsize main do_what sup sub
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
(* list items *)
| "\\item"
    {let arg = save_opt "" lexbuf in
    Html.item (scan_this main) arg ;
    main lexbuf}
(* in-text verbatim *)
| ("\\prog" | "\\verb" | "\\verb*") _
   {let lxm = lexeme lexbuf in
   verb_delim := String.get lxm (String.length lxm-1) ;
   Html.open_group "CODE" ;
   new_env "*verb" ;
   inverb lexbuf}
(* TeX conditionals *)
  | "\\newif"
      {let arg = save_arg lexbuf in
      newif arg ;
      main lexbuf}
  | "\\else"  {skip_false lexbuf}
  | "\\fi"    {skip_blanks lexbuf ; main lexbuf}

(* Substitution  *)
  | '#' ['1'-'9']
      {let lxm = lexeme lexbuf in
      let i = Char.code (lxm.[1]) - Char.code '1' in
      if i >= Array.length !stack then
        raise (Failure "Top level argument");
      let arg = !stack.(i) in
      if !verbose > 2 then
        prerr_endline ("Subst arg: ``"^arg^"''") ;
      let old_args = !stack in
      stack := pop stack_stack ;
      if !verbose > 2 then
        prerr_args !stack;
      scan_this_arg old_args main lexbuf arg ;
      push stack_stack !stack ;
      stack := old_args ;
      main lexbuf}
(* Commands *)
  | command_name
      {let name = lexeme lexbuf in
      begin match name with
    (* Html primitives *)
        "\\@open" ->
          let tag = save_arg lexbuf in
          let arg = save_arg lexbuf in
          if no_display tag then begin
            if tag="DISPLAY" then begin
              push stack_display !display;
              display := true ;
              Html.open_display (display_arg !verbose)
            end else begin
                warning ("direct opening of "^tag);
              top_open_block tag arg
            end
          end else
            top_open_block tag arg ;
          main lexbuf
      | "\\@close" ->
          let tag = save_arg lexbuf in
          if no_display tag then begin
            if tag="DISPLAY" then begin
              Html.close_display ();
              display := pop stack_display
            end else begin
              warning ("direct closing of "^tag);
              top_close_block tag
            end
          end else
            top_close_block tag ;
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
          let arg = save_arg lexbuf in
          let envs = get_style main arg in
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
          let arg = subst_arg subst lexbuf in
          Html.set_dt arg ;
          main lexbuf
      | "\\usecounter" ->
          let arg = subst_arg subst lexbuf in
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
(* Color package *)
      |  "\\definecolor" ->
          let clr = subst_arg subst lexbuf in
          let mdl = subst_arg subst lexbuf in
          let value = subst_arg subst lexbuf in
          Color.define clr mdl value ;
          main lexbuf
      | "\\color" ->
          let clr = subst_arg subst lexbuf in
          let htmlval = Color.retrieve clr in
          Html.open_mod (Color ("\""^htmlval^"\"")) ;
          skip_blanks lexbuf ;
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
(* Includes *)
      | "\\includeonly" ->
          let arg = Save.cite_arg lexbuf in
          add_includes arg ;
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
          let sec_here = subst_arg subst lexbuf
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
          if name = "\\savebox" then begin
            warning "savebox";
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
          Html.put (quote_char (Char.chr arg)) ;
          skip_blanks_pop lexbuf ; main lexbuf
      | "\\symbol" ->
          let arg = save_arg lexbuf in
          scan_this main ("\\char"^arg) ;
          main lexbuf
(* labels *)
      | "\\label" ->
          let save_last_closed = Html.get_last_closed () in
          let lab = subst_arg subst lexbuf in
          Html.loc_name lab "" ;
          Html.set_last_closed save_last_closed ;
          main lexbuf
      |  "\\ref" ->
          let lab = subst_arg subst lexbuf in 
          Html.loc_ref (Auxx.rget lab) lab ;
          main lexbuf
      |  "\\pageref" ->
          let lab = subst_arg subst lexbuf in
          Html.loc_ref "X" lab ;
          main lexbuf
(* index *)
      | "\\@index" ->
          let save_last_closed = Html.get_last_closed () in
          let tag = subst_opt "default" subst lexbuf in
          let arg = subst_arg subst lexbuf in
          Index.treat tag arg ;
          Html.set_last_closed save_last_closed ;
          main lexbuf
      | "\\@printindex" ->
          start_lexstate () ;
          let tag =  subst_opt "default" subst lexbuf in
          Index.print (scan_this main) tag ;
          restore_lexstate ();
          main lexbuf
      | "\\newindex" |  "\\renewindex" ->
          let tag = subst_arg subst lexbuf in
          let suf = subst_arg subst lexbuf in
          let _   = save_arg lexbuf in
          let name = subst_arg subst lexbuf in
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
          warning what ;
          main lexbuf
(* spacing *)
      |  "\\hspace"|"\\vspace" ->
          let vert = name = "\\vspace" in           
          let arg = subst_arg subst lexbuf in
          begin try
            let n = Length.main (Lexing.from_string arg) in
            if vert then
              for i=1 to n do
                Html.skip_line ()
              done
            else
              for i=1 to n do
                Html.put "&nbsp;"
              done
          with Length.No ->
            warning (name^" with arg ``"^arg^"''")
          end ;
          main lexbuf
            
(* user macros *)
      | _ ->
          let rec exec = function
              [] -> ()
            | i::rest -> begin match i with
                Print str -> Html.put str
              | Print_arg i ->
                  let arg = !stack.(i) in
                  let old_stack = !stack in
                  stack := pop stack_stack ;
                  scan_this main arg ;
                  push stack_stack !stack ;
                  stack := old_stack
              | Print_fun (f,i) ->
                  let arg = !stack.(i) in
                  let old_stack = !stack in
                  stack := pop stack_stack ;
                  scan_this main (f (subst_this subst arg)) ;
                  push stack_stack !stack ;
                  stack := old_stack
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
          
          let pat,body = find_macro name in
          let args = make_stack name pat lexbuf in
          let saw_par = !Save.seen_par in
          push stack_stack !stack ;
          stack := args;
          let is_limit = checklimits lexbuf ||  Latexmacros.limit name in
          if is_limit || Latexmacros.big name then begin
            let sup,sub = Save.get_sup_sub lexbuf in
            let do_what =
              (fun () -> exec body ; stack := pop stack_stack) in
            if !display && is_limit then
              limit_sup_sub main do_what sup sub
            else if !display &&  Latexmacros.int name then
              int_sup_sub true 3 main do_what sup sub
            else
              standard_sup_sub main do_what sup sub ;
            main lexbuf
          end else begin
            try
              exec body ;
              stack := pop stack_stack ;
              if (!verbose > 2) then
                prerr_string ("Cont after macro "^name^": ") ;
              if saw_par then top_par () 
              else if
                Latexmacros.invisible name ||
                (not !in_math && not !alltt &&
                 (pat = ([],[])) && last_letter name && !eat_space)
              then begin
                 if !verbose > 2 then
                   prerr_endline "skipping blanks";
                 skip_blanks_pop lexbuf
              end else begin
                if !verbose > 2 then
                  prerr_endline "not skipping blanks";
              end ;
              main lexbuf
            with 
              IfFalse -> begin
                stack := pop stack_stack ;
                if (!verbose > 2) then
                  prerr_endline ("Cont after iffalse:"^name) ;
                skip_false lexbuf
              end
          end
      end}
(* Html specials *)
| '<'         { Html.put "&lt;"; main lexbuf }
| '>'         { Html.put "&gt;"; main lexbuf }
| '~'         { Html.put "&nbsp;"; main lexbuf }
| '{'
    {top_open_group () ;  main lexbuf}
| '}'
    {top_close_group () ; main lexbuf}
| eof
   {if !verbose > 1 then Printf.fprintf stderr "Eof\n" ; ()}
| '\n'
  {(* if not (is_table  !in_table) then *) begin
    Html.put_char '\n'
  end ;
  main lexbuf}
| ' '+
   {if !alltt then
     let lxm = lexeme lexbuf in Html.put lxm
   else
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

and verbenv = parse
  "\\end" " " * "{" ['a'-'z'] + "}"
    {let lxm = lexeme lexbuf in
    let env = env_extract lxm in
    if env = !cur_env then begin
      top_close_block "PRE" ;
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
      let arg = save_arg lexbuf in
      scan_this main ("{"^arg^"}")
    end ;
    verbenv lexbuf}
| '\t'
      {for i=1 to !tab_val do
        Html.put_char ' '
      done ;
      verbenv lexbuf}
| eof {()}
| _   { Html.put (quote_char (lexeme_char lexbuf 0)) ; verbenv lexbuf}

and inverb = parse
 _
  {let c = lexeme_char lexbuf 0 in
  if c = !verb_delim then begin
    Html.close_group () ;
    close_env "*verb" ;
    main lexbuf
  end else begin
    Html.put (quote_char c) ;
    inverb lexbuf
  end}

and verblatex = parse
  "\\end"
    {let lxm = lexeme lexbuf in
    let env = save_arg lexbuf in
    if env = "verblatex" then
      Image.put_char '\n'
    else begin
      verblatex lexbuf
    end}
|  _ 
    {verblatex lexbuf}
|  eof {failwith "EOF in verblatex"}

and latexonly = parse
   '%'+ ' '* ("END"|"end") ' '+ ("LATEX"|"latex")  [^'\n']* '\n'
     {stop_other_scan main lexbuf}
|  '%'+ ' '* ("HEVEA"|"hevea") ' '*
     {latexonly lexbuf}
|  '%'
     {latex_comment lexbuf ; latexonly lexbuf}
|  "\\end"
     {let arg = save_arg lexbuf in
     if arg = "latexonly" then begin
       stop_other_scan main lexbuf
     end else if arg = top stack_entry then begin
       let _ = pop stack_entry in
       push stack_out arg ;
       begin match find_macro ("\\end"^arg) with
         _,[Subst body] ->
           scan_this_may_cont false latexonly lexbuf body
       |  _,_ -> failwith ("Bad closing macro in latexonly: ``"^arg^"''")
       end
     end else
       latexonly lexbuf}
|  command_name  | _ {latexonly lexbuf}
| eof
    {if !stack_lexbuf = [] then ()
    else begin
      let lexbuf = previous_lexbuf () in
      latexonly lexbuf
    end}


and latex_comment = parse
  '\n' | eof  {()}
| [^'\n']+    {latex_comment lexbuf}

and verbimage = parse
  "\\end"
    {let lxm = lexeme lexbuf in
    Save.start_echo () ;
    let env = save_arg lexbuf in
    let true_env = Save.get_echo () in
    if env = "verbimage" then
      Image.put_char '\n'
    else begin
      Image.put lxm ;
      Image.put true_env ;
      verbimage lexbuf
    end}
|  _
    {let lxm = lexeme_char lexbuf 0 in
    Image.put_char lxm ;
    verbimage lexbuf}
|  eof {failwith "EOF in verbimage"}

and image = parse
   '%'+ ' '* ("END"|"end") ' '+ ("IMAGE"|"image")  [^'\n']* '\n'
     {Image.put_char '\n' ; stop_other_scan main lexbuf}
|  '%'+ ' '* ("HEVEA"|"hevea") ' '*
     {image lexbuf}
|  '%'
     {let lxm = lexeme lexbuf in
     Image.put lxm ;
     image_comment lexbuf ;
     image lexbuf}
(* Substitution in image *)
| '#' ['1'-'9']
    {let lxm = lexeme lexbuf in
    let i = Char.code (lxm.[1]) - Char.code '1' in
    if i >= Array.length !stack then
      raise (Failure "Top level argument in image");
    let arg = !stack.(i) in
    if !verbose > 2 then
      prerr_endline ("Subst arg: ``"^arg^"''");
    let old_args = !stack in
    stack := pop stack_stack ;
    if !verbose > 2 then
      prerr_args !stack;
    scan_this image arg ;
    push stack_stack !stack ;
    stack := old_args ;
    image lexbuf}
(* Definitions of  simple macros *)
| "\\def" | "\\gdef"
   {let lxm = lexeme lexbuf in
   Save.start_echo () ;
   let _ = Save.csname lexbuf in
   let _ = Save.defargs lexbuf in
   let _ = save_arg lexbuf in
   Image.put lxm ;
   Image.put (Save.get_echo ()) ;
   image lexbuf}
| "\\renewcommand" | "\\newcommand" | "\\providecommand"
  {let lxm = lexeme lexbuf in
  Save.start_echo () ;
  let _ = Save.csname lexbuf in
  let _ = parse_args_opt ["0" ; ""] lexbuf in
  let _ = save_arg lexbuf in
  Image.put lxm ;
  Image.put (Save.get_echo ()) ;
  image lexbuf}
| "\\newenvironment" | "\\renewenvironment"
   {let lxm = lexeme lexbuf in
   Save.start_echo () ;
   let _ = save_arg lexbuf in
   let _ = parse_quote_arg_opt "0" lexbuf in
   let _ = parse_quote_arg_opt "" lexbuf in
   let _ = save_arg lexbuf in
   let _ = save_arg lexbuf in
   Image.put lxm ;
   Image.put (Save.get_echo ()) ;
   image lexbuf}
|  "\\end"
     {let lxm = lexeme lexbuf in
     Save.start_echo () ;
     let arg = save_arg lexbuf in
     let true_arg = Save.get_echo () in
     if arg = "toimage" then begin
       Image.put_char '\n' ;
       stop_other_scan main lexbuf
     end else if arg = top stack_entry then begin
       let _ = pop stack_entry in
       push stack_out arg ;
       begin match find_macro ("\\end"^arg) with
         _,[Subst body] ->
           scan_this_may_cont false image lexbuf body
       |  _,_ -> failwith ("Bad closing macro in image: ``"^arg^"''")
       end
     end else begin
       Image.put lxm ; Image.put true_arg ;
       image lexbuf
     end}
|  command_name
    {Image.put (lexeme lexbuf) ; image lexbuf}
| _
     {let s = lexeme lexbuf in
     Image.put s ;
     image lexbuf}
| eof
    {if !stack_lexbuf = [] then ()
    else begin
      let lexbuf = previous_lexbuf () in
      image lexbuf
    end}


and image_comment = parse
  '\n' {Image.put_char '\n'}
| eof  {()}
| [^'\n']+
    {let lxm = lexeme lexbuf in
    Image.put lxm ;
    image_comment lexbuf}

and mbox_arg = parse
  ' '+ {mbox_arg lexbuf}
| eof
     {if !stack_lexbuf <> [] then begin
     let lexbuf = previous_lexbuf () in
     if !verbose > 2 then begin
       prerr_endline "Poping lexbuf in mbox_arg" ;
       pretty_lexbuf lexbuf
     end ;
     mbox_arg lexbuf
   end else failwith "Eof in mbox_arg"}
| '{' | ("\\bgroup" ' '* '\n'? ' '*)
    {push stack_table !in_table ; in_table := NoTable ;
    push stack_in_math !in_math ; in_math := false ;
    if !display then Html.item_display () ;
    push stack_display !display ; display := false ;
    Html.open_block "" "" ;
    new_env "*mbox" ;
    main lexbuf}

and skip_blanks_pop = parse
  ' '+ {skip_blanks_pop lexbuf}
| '\n' {more_skip_pop lexbuf}
| ""   {()}
| eof
   {if !stack_lexbuf <> [] && !eat_space then begin
     let lexbuf = previous_lexbuf () in
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
     let lexbuf = previous_lexbuf () in
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
  '%' [^'\n']* '\n'
     {skip_false lexbuf}
|  "\\if" ['a'-'z' 'A'-'Z']+
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
     {if !if_level = 0 then begin
        skip_blanks lexbuf ; main lexbuf
     end else begin
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
    {skip_comment lexbuf ; start_image_scan "" image lexbuf ; main lexbuf}
| ' '* ("HEVEA"|"hevea") ' '*
   {main lexbuf}
| ' '* ("BEGIN"|"begin") ' '+ ("LATEX"|"latex")
    {skip_comment lexbuf ;
    start_other_scan "latexonly" latexonly lexbuf ;
    skip_spaces_main lexbuf}
| ""
    {skip_comment lexbuf ; more_skip lexbuf ; main lexbuf}

and skip_comment = parse    
   [^ '\n']* '\n'
   {if !verbose > 1 then
     prerr_endline ("Comment:"^lexeme lexbuf) ;
   if !flushing then Html.flush_out () }
| "" {failwith "Comment not terminated"}

and subst = parse
'#' ['1'-'9']
    {let lxm = lexeme lexbuf in
    let i = Char.code (lxm.[1]) - Char.code '1' in
    if i >= Array.length !stack then
      raise (Failure "Primitive argument");
    let arg = !stack.(i) in
    if !verbose > 2 then
      prerr_endline ("Subst arg in subst: "^lxm^" -> <"^arg^">");
    let old_args = !stack in
    stack := pop stack_stack ;
    if !verbose > 2 then
      prerr_args !stack;
    subst (Lexing.from_string arg) ;
    push stack_stack !stack ;
    stack := old_args ;
    subst lexbuf}
|  "\\#" | '\\' | [^'\\' '#']+
    {Out.put subst_buff (lexeme lexbuf) ; subst lexbuf}
|  eof {()}

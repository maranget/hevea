(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: verb.mll,v 1.94 2009-09-15 15:33:13 maranget Exp $            *)
(***********************************************************************)
{
exception VError of string

module Make
  (Dest : OutManager.S) (Image : ImageManager.S)
  (Scan : Latexscan.S) : Misc.Rien =
struct
open Printf
open Misc
open Lexing
open Save
open Lexstate
open Latexmacros
open MyStack
open Scan
open Subst

exception Eof of string
exception ParseError

(* For file verbatim scanning *)
let input_verb = ref false
;;

(* For scanning by line *)
let line_buff = Out.create_buff ()
;;

(* Do not output an possible first empty line *)
let wrap_eat_fst_nl process =
  let first_line = ref true in
  (fun () ->
    if not (!first_line && Out.is_empty line_buff) then process () ;
    first_line := false)

(* For scanning the ``listings'' way *)

let lst_process_error _ lxm =
   warning ("listings, unknown character: '"^Char.escaped lxm^"'")

let lst_char_table = Array.make 256 lst_process_error
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

    
(* Parameters *)
type line_limit =
  | LineNumber of int
  | Marker of string

let pmark = function
  | LineNumber x -> string_of_int x
  | Marker tok -> "«"^tok^"»"

let lst_gobble  = ref 0
and lst_nlines  = ref 0
and lst_first   = ref 1
and lst_last    = ref 99999 (* infinity *)
and lst_linerange = ref []
and lst_print   = ref true
and lst_includerangemarker = ref true
and lst_string_spaces = ref true
and lst_texcl   = ref false
and lst_extended = ref false
and lst_sensitive = ref true
and lst_mathescape = ref false
and lst_directives = ref false
and lst_showlines = ref false
and lst_tabsize = ref 8
and lst_col = ref 0

let lst_showspaces = ref false (* false => spaces are spaces *)
and lst_showtabs = ref false 
and lst_save_spaces  = ref false


(* Output functions *)

let lst_buff = Out.create_buff ()

let lst_last_char = ref ' '

let lst_put c =
  incr lst_col ;
  lst_last_char := c ;
  Out.put_char lst_buff c

type lst_scan_mode =
  | Letter | Other | Empty | Start
  | Directive of bool (* bool flags some letter read *)

(*
let show_mode = function
  | Letter -> "Letter" | Other -> "Other" | Empty -> "Empty" | Start -> "Start"
  | Directive _ -> "Directive"
*)

let lst_scan_mode = ref Empty

type comment_type =
  | Nested of int
  | Balanced of (char -> string -> bool)
  | Line

type lst_top_mode =
  | Skip of lst_top_mode
  | StartNextLine of lst_top_mode * (bool ref)
(*  | EndNextLine of lst_top_mode *)
  | String of (char * (char * (Lexing.lexbuf -> char -> unit)) list)
  | Normal | Comment of string * comment_type
  | Delim of int * (char * (Lexing.lexbuf -> char -> unit)) list
  | Gobble of lst_top_mode * int
  | Escape of lst_top_mode * char * bool (* bool flags mathescape *)

let is_normal = function
| Normal -> true
| _ -> false

let is_outputing = function
  | Skip _ -> false
  | _    -> true

let string_of_top_mode = function
   | Delim (i,_) -> sprintf "Delim: %i" i
  | Skip _ -> "Skip"
  | StartNextLine (_,_) -> "StartNextLine"
(*
  | EndNextLine (mode) ->
      sprintf "EndNextLine (%s)" (string_of_top_mode mode)
*)
  | Comment (_, Balanced _) -> "Balanced"
  | Comment (_, Nested n)   -> "(Nested "^string_of_int n^")"
  | Comment (_, Line) -> "Line"
  | String _  -> "String"
  | Normal -> "Normal"
  | Gobble (_,_) -> "Gobble"
  | Escape (_,_,_) -> "Escape"


let lst_top_mode = ref (Skip Normal)

(*
let lst_ptok s =  prerr_endline (s^": "^Out.to_string lst_buff)
*)

(* Final ouput, with transformations *)
let dest_string = Scan.translate_put_unicode_string

(* Echo, with case change *)
let dest_case s =
  Dest.put
    (match !case with
    | Upper -> String.uppercase s
    | Lower -> String.lowercase s
    | _     -> s)

(* Keywords *)

let def_print s =
  Latexmacros.def "\\@tmp@lst" zero_pat
    (CamlCode (fun _ ->  dest_case s)) ;
  Latexmacros.def "\\@tmp@lst@print" zero_pat
    (CamlCode (fun _ ->  dest_string s))
;;

let lst_output_com com =
  let com = com^"{\\@tmp@lst}{\\@tmp@lst@print}" in
  fun () ->
    if not (Out.is_empty lst_buff) then begin
      let arg = Out.to_string lst_buff in
      match !lst_top_mode with
      | Normal ->
          def_print arg ;
          scan_this Scan.main com
      | Skip _|StartNextLine (_,_)
      | Gobble (_,_)
      | Escape (_,_,_) ->
          assert false
      | Delim (_, _)
      | Comment (_,_)|String _
(*    | EndNextLine _ *)
        ->
          scan_this main "\\@NewLine" ;
          dest_string arg
    end

let lst_output_other = lst_output_com "\\lst@output@other"
and lst_output_letter = lst_output_com "\\lst@output"
and lst_output_directive = lst_output_com "\\lst@output@directive"

let lst_output_token () =
  match !lst_scan_mode with
  | Letter -> lst_output_letter ()
  | Other  -> lst_output_other ()
  | Directive _ -> lst_output_directive ()
  | Empty|Start  -> scan_this main "\\@NewLine"



(*********************)
(* Delay some action *)
(*********************)

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

let restore_char_table to_restore =
  let rec do_rec = function
    | [] -> ()
    | (c,f)::rest ->
        lst_init_char c f ;
        do_rec rest in
  do_rec to_restore

let do_eat_delim visi k new_mode old_process lb c s =
  let chars = chars_string c s in
  let wrapper old_process lb c = match !lst_top_mode with
  | Delim (n,to_restore) ->
      if visi then old_process lb c ;
      if n = 1 then begin
        lst_output_token () ;
        lst_top_mode := new_mode ;
        restore_char_table to_restore ;
        k ()
      end else
        lst_top_mode := Delim (n-1,to_restore)
  | _ -> assert false in
  let to_restore = init_char_table_delim chars wrapper in
  lst_top_mode := Delim (1+String.length s, to_restore) ;
  wrapper old_process lb c 

let eat_delim = do_eat_delim true

(* Typical overkill: processesors are restored/saved
   at each step *)
let delay_action  k rmode old_process lb c s =
  let chars = chars_string c s in
  let rec wrapper old_process lb c = match !lst_top_mode with
  | Delim (n,to_restore) ->
      restore_char_table to_restore ;
      lst_top_mode := !rmode ;
      old_process lb c ;
      rmode := !lst_top_mode ;
      ignore (init_char_table_delim chars wrapper) ;
      if n = 1 then begin
        restore_char_table to_restore ;
        k ()
      end else
        lst_top_mode := Delim (n-1,to_restore)
  | _ -> assert false in
  let to_restore = init_char_table_delim chars wrapper in
  lst_top_mode := Delim (1+String.length s, to_restore) ;
  wrapper old_process lb c


(* Process functions *)
let lst_do_gobble mode n =
  if n > 1 then
    lst_top_mode := Gobble (mode,n-1)
  else
    lst_top_mode := mode

let lst_do_escape mode endchar math _lb lxm =
  if lxm = endchar then begin
    scan_this main "\\begingroup\\lst@escapebegin" ;
    if math then scan_this main "$" ;
    scan_this main (Out.to_string lst_buff) ;
    if math then scan_this main "$" ;
    scan_this main "\\lst@escapeend\\endgroup" ;
    lst_top_mode := mode
  end else
    Out.put_char lst_buff lxm


(*
let debug_curline msg =
  eprintf "%s: %s\n" msg (Scan.get_this_main "\\usebox{\\@curline}")
*)

(* We put those here since newlines may terminate comments *)
let begin_comment () =
  lst_output_token () ;
  scan_this Scan.main "\\begin{lrbox}{\\lst@box}"

and end_comment sty () =
  scan_this Scan.main
    (Printf.sprintf "\\end{lrbox}{%s{\\lst@box}}" sty)

let end_string to_restore =
  scan_this Scan.main "\\end{lrbox}{\\lst@stringstyle{\\lst@box}}" ;
  restore_char_table to_restore ;
  lst_showspaces := !lst_save_spaces

let rec end_mode mode = match mode with
| Comment (style,_) -> end_comment style ()
| String (_,to_restore) -> end_string to_restore
| Skip m|StartNextLine (m,_) -> end_mode m
| _ -> ()


(* Utilities *)
let zyva_all xs ys zyva =
  List.iter
    (fun x -> List.iter (fun y -> zyva x y) ys)
    xs

let gettoks cmd = match Latexmacros.find_fail cmd with
| _,Toks r-> r
| _ -> assert false

  
(* Number of (printed) source blocks in  a listing *)
let lst_nblocks = ref 0

(* Note, at the moment lst_process_newline is the only
   processor that can start/stop output,
   1. Process_newline is fooled by using lst_first/lst_last variables
   2. This process_newlines must sometime not increase line numbers,
      ie, when there is in fact no eol *)


let is_comment_line = function
  | Comment (_, Line) -> true
  | _ -> false
      
let rec lst_process_newline real_eol lb c =
if !verbose > 1 then
  fprintf stderr "lst_process_newline: mode=%s first=%i last=%i nlines=%i\n"
    (string_of_top_mode !lst_top_mode)
    !lst_first !lst_last !lst_nlines ;
match !lst_top_mode with
| Skip newmode ->
    if !lst_nlines = !lst_first - 1 then begin
      lst_top_mode := newmode ;
      lst_process_newline real_eol lb c ;
      if !lst_nblocks = 0 then
        scan_this Scan.main
          (if !lst_first=1 then "\\let\\lst@br\\lst@@@br"
          else "\\let\\lst@br\\lst@@br")
    end else begin
      if real_eol then begin
        incr lst_nlines ;
        scan_this Scan.main "\\lsthk@InitVarEOL"
      end
    end
| StartNextLine (newmode,to_activate) ->
    lst_first := !lst_nlines+1 ;
    lst_top_mode := Skip newmode ;
    lst_process_newline real_eol lb c ;
    to_activate := true
(*
| EndNextLine (mode) ->
    lst_last := !lst_nlines ;
    lst_top_mode := mode ;
    lst_process_newline real_eol lb c
*)
| Gobble (mode,_) ->
    lst_top_mode := mode ;
    lst_process_newline real_eol lb c
| Escape (_mode,cc,math) ->
    lst_do_escape (Comment ("\\@empty",Line)) cc math lb c ;
    if is_comment_line !lst_top_mode  then
      lst_process_newline real_eol lb c
| Comment (style, Line) ->
    lst_output_token () ;
    end_comment style () ;
    lst_top_mode := Normal ;
    lst_process_newline real_eol lb c
| Delim (_, _) -> assert false
| String _|Normal|Comment (_,(Balanced _|Nested _)) as mode ->
    if real_eol then
      scan_this Scan.main "\\lsthk@InitVarEOL\\lsthk@EOL" ;
    begin match !lst_scan_mode with
    | Empty -> lst_scan_mode := Start
    | Start -> ()
    | _ ->
        lst_output_token () ;
        lst_scan_mode := Start
    end ;
    let next_line = !lst_nlines+1 in
    if next_line <= !lst_last then begin
      lst_col := 0 ;
      scan_this Scan.main
        "\\lsthk@InitVarBOL\\lsthk@EveryLine" ;
      if !lst_gobble > 0 then
        lst_top_mode := Gobble (mode,!lst_gobble)
    end else begin
      incr lst_nblocks ;
      lst_col := 0 ;
      scan_this Scan.main
        "\\lsthk@InitVarBOL\\lsthk@LastLine" ;
      set_next_linerange mode
    end ;
    if real_eol then lst_nlines := next_line


(***************)
(* Line ranges *)
(***************)


and process_EMark active rest_E old_process lb c =
  if !active && if_next_string rest_E lb then begin
    active := false ;
    let zyva x () =
      lst_last := x ;
      lst_process_newline false lb c in
    if !lst_includerangemarker then begin
      delay_action (zyva (!lst_nlines-1))
	(ref !lst_top_mode) old_process lb c rest_E
    end else begin
      zyva (!lst_nlines-1) () ;
      old_process lb c
    end
  end else
    old_process lb c

and lst_process_BMark active to_activate mark_start old_process lb c =
  if !active  then begin
    match !lst_top_mode with
    | Skip newmode when if_next_string mark_start lb  ->
        active := false ; lst_last := 99999 ;
        if !lst_includerangemarker then begin
          lst_first := !lst_nlines+1 ;
          lst_process_newline false lb c ;
          delay_action
            (fun () -> to_activate := true)
            (ref newmode) old_process lb c mark_start
        end else begin
          lst_top_mode := StartNextLine (newmode, to_activate) ;
          old_process lb c
        end
    | Escape (_, _, _)|Gobble (_, _)|Delim (_, _)|
      Comment (_,_)|String _| (* EndNextLine _| *)
      StartNextLine (_,_)|Skip _|Normal ->
        old_process lb c
  end else
     old_process lb c
        
and init_marker mark_start activate_end =
  let head_S = mark_start.[0]
  and rest_S = String.sub mark_start 1 (String.length mark_start-1) in
  lst_init_save_char head_S 
    (lst_process_BMark (ref true) activate_end rest_S)

and set_next_linerange mode = match !lst_linerange with
| [] ->
    lst_first := 99999 ;
    lst_top_mode := Skip mode
| (fst, lst)::rem ->
    if !verbose > 1 then
      eprintf "SET LINERANGE: %s %s\n" (pmark fst) (pmark lst) ;
    lst_linerange := rem ;
    let activate_end = ref false in
    begin match lst with
    | LineNumber x ->
        lst_last := x
    | Marker all_E ->
        lst_last := 99999 ;
	let zyva pref suf =
	  let all_E = pref ^ all_E ^ suf in
          let head_E = all_E.[0]
          and rest_E =
            String.sub all_E 1 (String.length all_E-1) in
          lst_init_save_char head_E (process_EMark activate_end rest_E)	in 
	zyva_all
	  (gettoks "\\lst@rangeendprefix")
	  (gettoks "\\lst@rangeendprefix")
	  zyva
    end ;

    begin match fst with
    | LineNumber x ->
        if !verbose > 1 then eprintf "fst=%i, nlines=%i\n" x !lst_nlines ;
        lst_first := x ;
        if !lst_nlines+1 < x then begin
           lst_top_mode := Skip mode
        end else begin
          if !lst_nblocks = 0 then
            scan_this Scan.main "\\let\\lst@br\\lst@@@br" ;
          lst_top_mode := mode
        end
    | Marker tok ->
	let zyva pref suf =
	  let tok = pref ^ tok ^ suf in
          lst_first := -1 ;
          lst_top_mode := Skip mode ;
          init_marker tok activate_end in
	zyva_all
	  (gettoks "\\lst@rangebeginprefix")
	  (gettoks "\\lst@rangebeginsuffix")
	  zyva
    end

let lst_process_letter lb lxm =
if !verbose > 1 then  fprintf stderr "lst_process_letter: %c\n" lxm ;
match !lst_top_mode with
| Skip _|StartNextLine (_,_) -> ()
| Gobble (mode,n) -> lst_do_gobble mode n
| Escape (mode,c,math) -> lst_do_escape mode c math lb lxm
| _ -> match !lst_scan_mode with
  | Letter -> lst_put lxm
  | Directive true ->
      lst_put lxm
  | Directive false ->
      lst_scan_mode := Directive true ;
      lst_put lxm
  | Empty|Start ->
      lst_scan_mode := Letter ;
      lst_put lxm
  | Other  ->
      lst_output_other () ;
      lst_scan_mode := Letter ;
      lst_put lxm

let lst_process_digit lb lxm =
if !verbose > 1 then
 fprintf stderr "lst_process_digit: %c\n" lxm ;
match !lst_top_mode with
| Skip _|StartNextLine (_,_) -> ()
| Gobble (mode,n) -> lst_do_gobble mode n
| Escape (mode,c,math) -> lst_do_escape mode c math lb lxm
| _ ->  match !lst_scan_mode with
  | Letter|Other -> lst_put lxm
  | Directive _ ->
      lst_output_directive () ;
      lst_scan_mode := Other ;
      lst_put lxm
  | Empty|Start  ->
      lst_scan_mode := Other ;
      lst_put lxm

let lst_process_other lb lxm =
if !verbose > 1 then
  fprintf stderr "process_other: %c\n" lxm ;
match !lst_top_mode with
| Skip _|StartNextLine (_,_) -> ()
| Gobble (mode,n) -> lst_do_gobble mode n
| Escape (mode,c,math) -> lst_do_escape mode c math lb lxm
|  _ -> match !lst_scan_mode with
    | Other -> lst_put lxm
    | Empty|Start ->
        lst_scan_mode := Other ;
        lst_put lxm        
    | Directive _ ->
        lst_output_directive () ;
        lst_scan_mode := Other ;
        lst_put lxm
    | Letter ->
        lst_output_letter () ;
        lst_scan_mode := Other ;
        lst_put lxm

(*  Caml code for \stepcounter{lst@space}  *)
let lst_output_space () =
  incr lst_col ;
  Counter.step_counter "lst@spaces"

let lst_process_space lb lxm =
if !verbose > 1 then
 fprintf stderr "process_space: '%s'\n" (Char.escaped lxm) ;
match !lst_top_mode with
| Skip _ |StartNextLine (_,_)-> ()
| Gobble (mode,n) -> lst_do_gobble mode n
| Escape (mode,c,math) -> lst_do_escape mode c math lb lxm
| _ ->
    begin match !lst_scan_mode with
    | Other ->
        lst_output_other ()  ;
        lst_scan_mode := Empty
    | Letter|Directive true ->
        lst_output_token () ;
        lst_scan_mode := Empty
    | Empty|Directive false -> ()
    | Start ->
        lst_scan_mode := Empty
    end ;
    lst_output_space ()

let lst_process_tab lb lxm =
  if !verbose > 1 then
    fprintf stderr "process_tab: '%s'\n" (Char.escaped lxm) ;
  let n = !lst_tabsize - (!lst_col mod !lst_tabsize) in
  if !lst_showtabs && is_normal !lst_top_mode then begin
    lst_output_token () ;
    if Latexmacros.exists "\\lst@tab" then begin
      for _i = 1 to n-1 do
        lst_process_space lb lxm
      done ;
      let save = !lst_showspaces in
      lst_showspaces := false ;
      lst_output_token () ;
      lst_showspaces := save ;
(* Assumes that the tab equivalent is one-char wide *)
      incr lst_col ;
      scan_this main "{\\lst@tab}"
    end else begin
      for _i = 1 to n do
        lst_process_space lb lxm
      done ;
      let save = !lst_showspaces in
      lst_showspaces := true ;
      lst_output_token () ;
      lst_showspaces := save      
    end
  end else begin
    for _i = 1 to n do
      lst_process_space lb lxm
    done
  end

let lst_process_start_directive  old_process lb lxm =
  match !lst_top_mode with
  | Normal -> begin match !lst_scan_mode with
    | Start ->
        lst_scan_mode := Directive false
    | _ -> old_process lb lxm
  end
  | _ ->  old_process lb lxm
      
  

exception EndVerb

let lst_process_end  endstring old_process lb lxm =
if !verbose > 1 then
 fprintf stderr "process_end: «%c»\n" lxm ;
  if
    (not !input_verb || MyStack.empty stack_lexbuf)
      && if_next_string endstring lb then begin
    Save.skip_delim endstring lb ;
    raise EndVerb
  end else
    old_process lb lxm

let lst_init_char_table inline =
  lst_init_chars
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@_$"
    lst_process_letter ;
  lst_init_chars "!\"#%&'()*+,-./:;<=>?[\\]^{}|`~" lst_process_other ;
  lst_init_chars "0123456789" lst_process_digit ;
  lst_init_char ' ' lst_process_space ;
  lst_init_char '\t' lst_process_tab ;
  if inline then
    lst_init_char '\n' lst_process_space
  else
    lst_init_char '\n' (lst_process_newline true)
;;

(* TeX escapes *)
let start_escape mode endchar math =
  lst_output_token () ;
  lst_top_mode := Escape (mode, endchar, math)

let lst_process_escape math ec old_process lb lxm =
if !verbose > 1 then
 fprintf stderr "lst_process_escape: %c\n" lxm ;
match !lst_top_mode with
| Skip _|StartNextLine (_,_) -> ()
| Gobble (mode,n) -> lst_do_gobble mode n
| Escape _        -> old_process lb lxm
| mode            -> start_escape mode ec math

  
(* Strings *)

let lst_bs_string process_c c old_process lb lxm =
  if !verbose > 1 then begin
    eprintf "lst_bs_string: «%c»\n" lxm
  end ;

(* Processs backslash, as usual *)
  old_process lb lxm ;

(* Change char_tables for backshalsh and c *)
  let saved = Array.copy lst_char_table in

  let restore_lst_char_table () =
    Array.blit saved 0 lst_char_table 0 (Array.length saved)  in

  lst_char_table.(Char.code lxm) <- old_process ;
  lst_char_table.(Char.code c) <- process_c ;
(* Change all entries in  char_table, -> restore table once the
   backslashed char is processed *)
   for i = 0 to Array.length lst_char_table-1 do
     let do_process = lst_char_table.(i) in
     lst_char_table.(i) <-
        (fun lb lxm ->
          do_process lb lxm ;
          restore_lst_char_table ())
   done
  

      
(* c is the character to be quoted *)
let lst_init_quote old_process c s =
  let r = ref [] in
  for i = 0 to String.length s-1 do
    if s.[i] = 'b' then begin
      r := ('\\',lst_char_table.(Char.code '\\')) :: !r ;
      lst_init_save_char '\\' (lst_bs_string old_process c)
    end
  done ;
  !r 


let lst_process_stringizer quote old_process lb lxm = match !lst_top_mode with
  | Normal ->
      lst_output_token () ;
      let to_restore = lst_init_quote old_process lxm quote in
      lst_top_mode := String (lxm, to_restore) ;
      lst_save_spaces := !lst_showspaces ;
      lst_showspaces := !lst_string_spaces ;
      scan_this Scan.main "\\begin{lrbox}{\\lst@box}" ;
      old_process lb lxm
  | String (c,to_restore) when lxm = c ->
      old_process lb lxm ;
      lst_output_token () ;
      end_string to_restore ;
      lst_top_mode := Normal
  | _ -> old_process lb lxm



(* Comment *)

let lst_process_BNC visi sty _ s old_process lb c =  match !lst_top_mode with
| Normal when if_next_string s lb -> 
    begin_comment () ;
    do_eat_delim
      visi
      (fun () -> ()) (Comment (sty,Nested 0)) old_process lb c s
| Comment (sty,Nested n) when if_next_string s lb ->
    eat_delim (fun () -> ()) (Comment (sty,Nested (n+1))) old_process lb c s
| _ -> old_process lb c

and lst_process_ENC visi s old_process lb c = match !lst_top_mode with
| Comment (sty,Nested 0) when if_next_string s lb ->
    do_eat_delim
      visi
      (end_comment sty)
      Normal
      old_process
      lb c s
|  Comment (sty,Nested n) when if_next_string s lb ->
    eat_delim
      (fun () -> ())
      (Comment (sty,Nested (n-1)))
      old_process lb c s
| _ -> old_process lb c

let lst_process_BBC visi sty check s old_process lb c =
  match !lst_top_mode with
| Normal when if_next_string s lb ->
    begin_comment () ;
    do_eat_delim visi
      (fun () -> ())
      (Comment (sty, Balanced check))
      old_process lb c s
| _ -> old_process lb c

and lst_process_EBC visi s old_process lb c = match !lst_top_mode with
| Comment (sty,Balanced check) when
  check c s && if_next_string  s lb ->
     do_eat_delim visi
      (end_comment sty)
      Normal
      old_process
      lb c s
| _ -> old_process lb c

let lst_process_LC visi sty s old_process lb c  =
  match !lst_top_mode with
| Normal when if_next_string s lb ->
    begin_comment () ;
    do_eat_delim visi
      (fun () -> ())
      (if !lst_texcl then Escape (Normal,'\n', false) else Comment (sty,Line))
      old_process lb c s
| _ ->
    old_process lb c

} 


let command_name =
  '\\' (( ['@''A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'] | "\\*")

rule inverb verb_delim put = parse
|  (_ as c)
    {if c = verb_delim then begin
      Dest.close_block "code";
    end else begin
      put c (fun () -> read_lexbuf lexbuf) ;
      inverb verb_delim put lexbuf
    end}
| eof
    {if not (empty stack_lexbuf) then
      let lexbuf = previous_lexbuf () in
      inverb verb_delim put lexbuf
    else
      raise (VError ("End of file after \\verb"))}

and start_inverb put = parse
| (_ as c) {inverb c put lexbuf}
| eof
    {if not (empty stack_lexbuf) then
        let lexbuf = previous_lexbuf () in
        start_inverb put lexbuf
      else
        raise (VError ("End of file after \\verb"))}

and scan_byline process finish = parse
|  "\\end" [' ''\t']* '{' ([^'}']+ as env) '}' as lxm
    { (* eprintf "by_line: lxm='%s' cur_env=%s\n" lxm !Scan.cur_env ; *)
     if (not !input_verb || MyStack.empty stack_lexbuf)
        && env = !Scan.cur_env then begin
      finish () ;
      scan_this Scan.main ("\\end"^env) ;
      Scan.top_close_block "" ;
      Scan.close_env !Scan.cur_env ;
      ()
    end else begin
      Out.put line_buff lxm ;
      scan_byline process finish lexbuf
    end}
| '\n'
    {process () ; scan_byline process finish lexbuf}
| _ as lxm 
    {Out.put_char line_buff lxm ;
    scan_byline process finish lexbuf}
| eof
    {if not (MyStack.empty stack_lexbuf) then begin
      let lexbuf = previous_lexbuf () in
      scan_byline process finish lexbuf
    end else begin
      finish () ;
      raise (Eof "scan_byline")
    end} 

and scan_bycommand out is_cmd = parse
|  "\\end" [' ''\t']* '{' ([^'}']+ as env) '}' as lxm
    {if env = !Scan.cur_env then begin
        Latexmacros.def "\\@tmp@scanned"
          zero_pat (Toks [Out.to_string out]) ;
        lxm
    end else begin
      Out.blit out lexbuf ;
      scan_bycommand out is_cmd lexbuf
    end}
| "\\verb"
  {Out.blit out lexbuf ;
   Save.start_echo () ;
   ignore (arg_verbatim lexbuf) ;
   let a = Save.get_echo () in
   Out.put out a ;
   scan_bycommand out is_cmd lexbuf}
| command_name as lxm
    {if is_cmd lxm then begin
      Latexmacros.def "\\@tmp@scanned" zero_pat (Toks [Out.to_string out]) ;
      Scan.expand_command lxm lexbuf ;
      Out.reset out
    end else begin
      Out.blit out lexbuf ;
    end ;
    scan_bycommand out is_cmd lexbuf}
| ('%' [^'\n']* '\n'?) | _
   { Out.blit out lexbuf ;
     scan_bycommand out is_cmd lexbuf }
| eof
   {if not (MyStack.empty stack_lexbuf) then begin
      let lexbuf = previous_lexbuf () in
      scan_bycommand out is_cmd lexbuf
    end else begin
      raise
        (Eof "scan_bycommand")
    end} 

and listings = parse
|  eof
    {if not (MyStack.empty stack_lexbuf) then begin
      let lexbuf = previous_lexbuf () in
      listings lexbuf
    end else begin
      raise
        (Eof "listings")
    end}
| (_ as lxm)
    {lst_char_table.(Char.code lxm) lexbuf lxm ;
    listings lexbuf}

and eat_line = parse
| eof
    {if not (MyStack.empty stack_lexbuf) then begin
      let lexbuf = previous_lexbuf () in
      eat_line lexbuf
    end else begin
      raise
        (Eof "eat_line")
    end}
| [^'\n']  {eat_line lexbuf}
| '\n'     {lst_process_newline true lexbuf '\n'}

and get_line = parse
|  eof
    {if not (MyStack.empty stack_lexbuf) then begin
      let lexbuf = previous_lexbuf () in
      get_line lexbuf
    end else begin
      raise
        (Eof "get_line")
    end}
| [^'\n']
    {let lxm = lexeme_char lexbuf 0 in
    Out.put_char line_buff lxm ;
    get_line lexbuf}
| '\n'     {Out.to_string line_buff}

and do_escape = parse
| eof {()}
| "\\esc"
    {let arg = save_arg lexbuf in
    scan_this main "\\mbox{" ;
    scan_this_arg Scan.main arg ;
    scan_this main "}" ;
    do_escape lexbuf}
| _ as lxm
    {Scan.translate_put_unicode lxm (fun () -> read_lexbuf lexbuf) ;
    do_escape lexbuf}

and read_lexbuf = parse
| eof    { -1 }
| _ as c { Char.code c }

and lst_linearg = parse
| '-'? (['0'-'9']+ as n)   {LineNumber (int_of_string n)}
| '-'? ([^'-'',']+ as tok) {Marker tok}
| "" {raise ParseError}

and lst_parse_linerange = parse
| eof {[]}
| ","?
    {let fst = lst_linearg lexbuf in
    let lst = lst_linearg lexbuf in
    (fst,lst)::lst_parse_linerange lexbuf}

{

let _ = ()
;;

let put_char_star c next = match c with
  | ' ' | '\t' -> Dest.put (Scan.get_prim "\\textvisiblespace")
  | c -> Scan.translate_put_unicode c next

and put_char c next = match c with
  | '\t' -> Dest.put_char ' '
  | c -> Scan.translate_put_unicode c next
;;


let getclass env = Scan.get_prim (Printf.sprintf "\\envclass@attr{%s}" env)

let open_verb put lexbuf =
  Dest.open_block ~force_inline:true "code" (getclass "verb");
  start_inverb put lexbuf
;;

def_code "\\verb" (open_verb put_char) ;
def_code "\\verb*" (open_verb put_char_star);
();;

let put_line_buff_verb () =
  Out.iter_next put_char line_buff ;
  Out.reset line_buff

and put_line_buff_verb_star () =
  Out.iter_next put_char_star line_buff ;
  Out.reset line_buff  
;;

let noeof lexer lexbuf =
  try lexer lexbuf
  with
  | Eof s ->
    raise
        (Misc.Close
           ("End of file in environment: ``"^ !Scan.cur_env^"'' ("^s^")"))
  | EndVerb -> ()

let open_verbenv star =
  Scan.top_open_block "pre" (getclass "verbatim") ;
  let process =
    wrap_eat_fst_nl
      (if star then
        (fun () -> put_line_buff_verb_star () ; Dest.put_char '\n')
      else
        (fun () -> put_line_buff_verb () ; Dest.put_char '\n'))
  and finish =
     if star then
       put_line_buff_verb_star
     else
       put_line_buff_verb in
  process, finish

and close_verbenv _ = Scan.top_close_block "pre"

let put_html () =
  Out.iter (fun c -> Dest.put_char c) line_buff ;
  Out.reset line_buff
;;

let put_subst () =
  let line =  Out.to_string line_buff in
(*  eprintf "put_subst: '%s'\n" line ; *)
  Out.reset line_buff ;
  Dest.put (Subst.subst_this line) ;
  ()
;;

let open_forget lexbuf =
  let process = (fun () -> Out.reset line_buff)
  and finish = (fun () -> Out.reset line_buff) in
  noeof (scan_byline process finish) lexbuf

let open_raw lexbuf =
  let process = (fun () -> put_html () ; Dest.put_char '\n')
  and finish = put_html in
  noeof (scan_byline process finish) lexbuf

let open_subst lexbuf  =
  let process = (fun () -> put_subst () ; Dest.put_char '\n')
  and finish = put_subst in
  noeof (scan_byline process finish) lexbuf
       
let open_rawhtml lexbuf = match !Parse_opts.destination with
    | Parse_opts.Html -> open_raw lexbuf
    | _ -> open_forget lexbuf

let open_rawtext lexbuf = match !Parse_opts.destination with
    | Parse_opts.Text|Parse_opts.Info -> open_raw lexbuf
    | _ -> open_forget lexbuf

let  close_nothing _ = ()

let open_tofile chan lexbuf =
  let process =
     (fun () ->
       output_string chan (Out.to_string line_buff) ;
       output_char chan '\n')
  and finish =
     (fun () ->
       output_string chan (Out.to_string line_buff) ;
       close_out chan) in
  noeof (scan_byline process finish) lexbuf

(*
and close_tofile _lexbuf = ()
*)

let put_line_buff_image () =
  Out.iter (fun c -> Image.put_char c) line_buff ;
  Out.reset line_buff

let open_verbimage lexbuf =
  let process = (fun () -> put_line_buff_image () ; Image.put_char '\n')
  and finish = put_line_buff_image in
  noeof (scan_byline process finish) lexbuf

(*
and close_verbimage _ = ()
*)
;;


def_code "\\verbatim"
    (fun lexbuf ->
      let p,f = open_verbenv false in
      noeof (scan_byline p f) lexbuf) ;
def_code "\\endverbatim" close_verbenv ;


def_code "\\verbatim*"
    (fun lexbuf ->
      let p, f = open_verbenv true in
      noeof (scan_byline p f) lexbuf) ;        
def_code "\\endverbatim*" close_verbenv ;

def_code "\\rawtext" open_rawtext ;
def_code "\\endrawtext" close_nothing ;
def_code "\\rawhtml" open_rawhtml ;
def_code "\\endrawhtml" close_nothing ;
def_code "\\raw" open_raw ;
def_code "\\endraw" close_nothing ;

def_code "\\verblatex" open_forget ; 
def_code "\\endverblatex" Scan.check_alltt_skip ;
def_code "\\verbimage" open_verbimage ; 
def_code "\\endverbimage" Scan.check_alltt_skip ;
()
;;

let init_verbatim () =
(* comment clashes with the ``comment'' package *)
  Latexmacros.def "\\comment"  zero_pat (CamlCode open_forget) ;
  Latexmacros.def "\\endcomment" zero_pat (CamlCode Scan.check_alltt_skip) ;
()
;;

register_init "verbatim" init_verbatim 
;;

(* The program package for JJL  que j'aime bien *)

let look_escape () =
  let lexbuf = MyLexing.from_string (Out.to_string line_buff) in
  do_escape lexbuf
;;

let init_program () =
  def_code "\\program"
    (fun lexbuf ->
      Scan.top_open_block "pre" "" ;
      let process =
         (fun () -> look_escape () ; Dest.put_char '\n')
      and finish = look_escape in
      noeof (scan_byline process finish) lexbuf) ;
  def_code "\\endprogram" close_verbenv
;;

register_init "program" init_program
;;
    

(* The moreverb package *)
let tab_val = ref 8

let put_verb_tabs () =
  let char = ref 0 in
  Out.iter_next
    (fun c next -> match c with
      | '\t' ->
          let limit = !tab_val - !char mod !tab_val in
          for _j = 1 to limit do
            Dest.put_char ' ' ; incr char
          done ;  
      | c -> Scan.translate_put_unicode c next ; incr char)
    line_buff ;
  Out.reset line_buff

let open_verbenv_tabs () =
  Scan.top_open_block "pre" "" ;
  let process = (fun () -> put_verb_tabs () ; Dest.put_char '\n')
  and finish = put_verb_tabs in
  process, finish

and close_verbenv_tabs lexbuf =
  Scan.top_close_block "pre" ;
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
  Scan.top_open_block "pre" "" ;
  line := start_arg ;
  let first_line = ref true in
  let process = 
    (fun () ->
      if !first_line then begin
        first_line := false ;
        if not (Out.is_empty line_buff) then
          output_line inter_arg star ;        
      end else
        output_line inter_arg star  ;
      Dest.put_char '\n')
  and finish =
     (fun () ->
       if not (Out.is_empty line_buff) then
         output_line inter_arg star) in
  process, finish

and close_listing lexbuf =
  Scan.top_close_block "pre" ;
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
      let p, f = open_verbenv_tabs () in
      Lexstate.save_lexstate () ;
      let first = get_line lexbuf in
      Lexstate.restore_lexstate () ;
      scan_this Scan.main first ;
      Dest.put_char '\n' ;
      noeof (scan_byline p f) lexbuf) ;
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
      let arg = Get.get_int (save_body lexbuf) in
      Dest.put (sprintf "%4d " arg)) ;

  def_code "\\listing"
    (fun lexbuf ->
      let inter = Get.get_int (save_opt "1" lexbuf) in
      let start = Get.get_int (save_body lexbuf) in
      interval := inter ;
      let p, f = open_listing start inter false in
      noeof (scan_byline p f) lexbuf) ;
  def_code "\\endlisting" close_listing ;

  def_code "\\listingcont"
    (fun lexbuf ->
      let p, f = open_listing !line !interval false in
      noeof (scan_byline p f) lexbuf) ;
  def_code "\\endlistingcont" close_listing ;

  def_code "\\listing*"
    (fun lexbuf ->
      let inter = Get.get_int (save_opt "1" lexbuf) in
      let start = Get.get_int (save_body lexbuf) in
      interval := inter ;
      let p, f = open_listing start inter true in
      noeof (scan_byline p f) lexbuf) ;
  def_code "\\endlisting*" close_listing ;

  def_code "\\listingcont*"
    (fun lexbuf ->
      Scan.check_alltt_skip lexbuf ;
      let p, f = open_listing !line !interval false in
      noeof (scan_byline p f) lexbuf) ;
  def_code "\\endlistingcont*" close_listing ;
  ())

(* The comment package *)

let init_comment () =
  def_code "\\@excludecomment" open_forget ;
  def_code "\\end@excludecomment"  Scan.check_alltt_skip ;
;;

register_init "comment" init_comment      
;;    

(* And preview, which is identical, up to interface *)

let init_preview () =
  def_code "\\@prw" open_forget ;
  def_code "\\end@prw"  Scan.check_alltt_skip ;
;;

register_init "preview" init_preview
;;    

(* The listings package *)


let default_linerange = [LineNumber 1, LineNumber 99999]

let parse_linerange s =
  let r =
    try
      lst_parse_linerange (MyLexing.from_string s)
    with
    | ParseError ->
        warning ("lst: invalid linerange '"^s^"'") ;
        default_linerange in
  match r with
  | [] -> default_linerange
  | _ -> r


(* 
  Caml code for
  \def\lst@spaces
    {\whiledo{\value{lst@spaces}>0}{~\addtocounter{lst@spaces}{-1}}}
*)

let code_spaces _lexbuf =
  let n = Counter.value_counter "lst@spaces" in
    if !lst_showspaces then
      for _i = 1 to n do
        Dest.put_char '_'
      done
    else
      Dest.put_hspace true (Length.Char n);
    Counter.set_counter "lst@spaces" 0
;;

let check_style sty =
  if String.length sty > 0 && sty.[0] == '\\' then
    sty
  else
    "\\csname lst@"^sty^"\\endcsname"

let check_visi = function
  | "visible" -> true
  | "invisible" -> false
  | _ -> assert false

let do_code_double_delim process_B process_E lexbuf =
  let sty = subst_arg lexbuf in
  let sty = check_style sty in
  let lxm_B = get_prim_arg lexbuf in
  let lxm_E = get_prim_arg lexbuf in
  if lxm_B <> "" && lxm_E <> "" then begin
    let head_B = lxm_B.[0]
    and rest_B = String.sub lxm_B 1 (String.length lxm_B-1)
    and head_E = lxm_E.[0]
    and rest_E = String.sub lxm_E 1 (String.length lxm_E-1) in
    lst_init_save_char head_B
      (process_B
         sty
         (fun c s -> c = head_E && s = rest_E)
         rest_B) ;
    lst_init_save_char head_E (process_E rest_E)
  end

let code_double_delim process_B process_E lexbuf =
  do_code_double_delim (process_B true) (process_E true) lexbuf
  
let code_single_delim  process_B process_E lexbuf =
  let visi =  check_visi (get_prim_arg lexbuf) in
  do_code_double_delim (process_B visi) (process_E visi) lexbuf

let do_code_line is_com lexbuf =
  let visi =  check_visi (get_prim_arg lexbuf) in
  let visi =
    if is_com && not visi then begin
      Misc.warning "invisible commments not available" ;
      true 
    end else visi in
  let sty = subst_arg lexbuf in
  let sty = check_style sty in
  let lxm_LC = get_prim_arg lexbuf in
  if lxm_LC <> "" then begin
    let head = lxm_LC.[0]
    and rest = String.sub lxm_LC 1 (String.length lxm_LC-1) in
    lst_init_save_char head (lst_process_LC visi sty rest)
  end

let code_line_comment = do_code_line true
and code_line_delim = do_code_line false

let code_stringizer lexbuf =
  let mode = Scan.get_prim_arg lexbuf in
  let _sty = subst_arg lexbuf in
  let schars = Scan.get_prim_arg lexbuf in
  lst_init_save_chars schars (lst_process_stringizer mode)
;;

let lst_finalize inline =
  if inline then begin
    lst_output_token ()
  end else begin
    if is_outputing !lst_top_mode then begin
      scan_this main "\\lst@forget@lastline\\lsthk@LastLine" ;
    end
  end ;
  end_mode !lst_top_mode
;;


let open_lst_inline keys =
  scan_this Scan.main "\\lsthk@PreSet" ;
  scan_this_list Scan.main ("\\lstset{"::keys@["}"]) ;
(*  scan_this Scan.main  "\\lsthk@AfterSetLanguage" ; *)
(* For inline *)
  scan_this Scan.main "\\lsthk@InlineUnsave" ;
(* Ignoring output *)
  lst_gobble := Get.get_int_string (string_to_arg "\\lst@gobble") ;
  lst_first := Get.get_int_string (string_to_arg "\\lst@first") ;
  lst_last := Get.get_int_string (string_to_arg "\\lst@last") ;
  lst_nlines := 0 ;
  lst_init_char_table true ;
  scan_this Scan.main "\\lsthk@SelectCharTable" ;
  if !lst_extended then
    for i = 128 to 255 do
      lst_init_char (Char.chr i) lst_process_letter
    done ;
  scan_this Scan.main "\\lsthk@Init" ;
(* Change char categories *)
  let alsoletter = Scan.get_prim "\\lst@alsoletter" in
  if alsoletter <> "" then begin
    lst_init_chars alsoletter  lst_process_letter
  end ;
(* Directives *)
  if !lst_directives then begin
    lst_init_save_char '#' lst_process_start_directive
  end ;
(* Print key *)
  if not !lst_print then begin
    lst_last := -2 ; lst_first := -1
  end ;  
(* Strings *)
(* Escapes to TeX *)
  if !lst_mathescape then begin
    lst_init_save_char '$' (lst_process_escape true '$')
  end ;
  let begc = Scan.get_this_main "\\@getprintnostyle{\\lst@BET}"
  and endc = Scan.get_this_main "\\@getprintnostyle{\\lst@EET}" in
  if begc <> "" && endc <> "" then begin
    lst_init_save_char begc.[0] (lst_process_escape false endc.[0])
  end ;
  scan_this Scan.main "\\lsthk@InitVar" ;
  lst_scan_mode := Empty ;
  lst_top_mode := Normal

and close_lst_inline () =
  lst_finalize true ;
  while !Scan.cur_env = "command-group" do
    scan_this Scan.main "\\endgroup"
  done ;
  scan_this Scan.main "\\lsthk@DeInit"
;;

let lst_user_name name = name^"@lst@user"

let close_lst_env name =
   let com_name = (end_env (lst_user_name name)) in
   lst_finalize false ;
   scan_this Scan.main "\\lsthk@DeInit" ;
   scan_this Scan.main com_name

let open_lst_env name =
  let com_name = (start_env (lst_user_name name)) in
  (fun lexbuf ->
    Image.stop () ;
    scan_this Scan.main "\\lsthk@PreSet" ;
    expand_command_no_skip com_name lexbuf ;
(*    scan_this Scan.main "\\lsthk@AfterSetLanguage" ; *)
    lst_init_char_table false ;
    scan_this Scan.main "\\lsthk@SelectCharTable" ;
    if !lst_extended then
      for i = 128 to 255 do
        lst_init_char (Char.chr i) lst_process_letter
      done ;
(* Set and exploit command sequence *)
    scan_this Scan.main "\\lsthk@Init" ;
    lst_gobble := Get.get_int_string (string_to_arg "\\lst@gobble") ;
    let linerange = Scan.get_prim "\\lst@linerange" in
    lst_linerange := parse_linerange linerange ;
    lst_nlines := 0 ; lst_nblocks := 0 ;
    lst_tabsize := Get.get_int_string (string_to_arg "\\lst@tabsize") ;
(* Change char categories *)
  let alsoletter = Scan.get_prim "\\lst@alsoletter" in
  if alsoletter <> "" then begin
    lst_init_chars alsoletter  lst_process_letter
  end ;
(* Directives *)
    if !lst_directives then begin
      lst_init_save_char '#' lst_process_start_directive
    end ;
(* Print key *)
    if not !lst_print then begin
      lst_last := -2 ; lst_first := -1
    end ;  
(* Escapes to TeX *)
    if !lst_mathescape then begin
      lst_init_save_char '$' (lst_process_escape true '$')
    end ;
    let begc = Scan.get_this_main "\\@getprintnostyle{\\lst@BET}"
    and endc = Scan.get_this_main "\\@getprintnostyle{\\lst@EET}" in
    if begc <> "" && endc <> "" then begin
      lst_init_save_char begc.[0] (lst_process_escape false endc.[0])
    end ;
    scan_this Scan.main "\\lsthk@InitVar" ;
    lst_scan_mode := Empty ;
    set_next_linerange Normal ;
    scan_this Scan.main "\\lst@pre\\@lst@caption\\@open@lstbox" ;  
    scan_this Scan.main "\\lst@basicstyle" ;
(* Eat first line *)
    save_lexstate () ;
    noeof eat_line lexbuf ;
    restore_lexstate () ;
    if !Misc.verbose > 1 then eprintf "LISTINGS: first line is scanned\n" ;
(* For detecting endstring, must be done after eat_line *)
    lst_init_save_char '\\' (lst_process_end ("end{"^name^"}")) ;
    noeof listings lexbuf ;
    if !Misc.verbose > 1 then eprintf "LISTINGS: scanning over\n" ;
    close_lst_env name ;
    scan_this Scan.main "\\@close@lstbox\\lst@post" ;
    Scan.top_close_block "" ;
    Scan.close_env !Scan.cur_env ;    
    Image.restart () ;
    Scan.check_alltt_skip lexbuf)
;;   
    

let do_newenvironment lexbuf =
  let name = get_prim_arg lexbuf in
  let nargs,optdef = match save_opts ["0" ; ""] lexbuf with
  |  [x ; y ] -> x,y
  | _ -> assert false in
  let body1 = subst_body lexbuf in
  let body2 = subst_body lexbuf in
  if
    Latexmacros.exists (start_env name) ||
    Latexmacros.exists (end_env name)
  then
    warning
      ("Not (re)-defining environment ``"^name^"'' with \\lstnewenvironment")
  else begin
    Latexmacros.def
      (start_env (lst_user_name name))
      (latex_pat
         (match optdef with
         | {arg=No _}    -> []
         | {arg=Yes s ; subst=env} -> [do_subst_this_list (mkarg s env)])
         (match nargs with 
         | {arg=No _} -> 0
         | {arg=Yes s ; subst=env} -> Get.get_int (mkarg s env)))
      (Subst body1) ;
    Latexmacros.def (end_env (lst_user_name name))  zero_pat (Subst body2) ;
    def_code (start_env name) (open_lst_env name)
  end


let lst_boolean lexbuf =
  let b = get_prim_arg lexbuf in
  Dest.put
    (match b with
    | "" -> "false"
    | s  when s.[0] = 't' || s.[0] = 'T' -> "true"
    | _ -> "false")
;;

def_code "\\@callopt"
    (fun lexbuf ->
      let csname = Scan.get_csname lexbuf in
      let all_arg = get_prim_arg lexbuf in
      let lexarg = MyLexing.from_string all_arg in
      let opt = Subst.subst_opt "" lexarg in
      let arg = Save.rest lexarg in
      let exec = csname::"["::opt@["]{";arg;"}"] in
      scan_this_list  Scan.main exec)


type css_border = None | Solid | Double

(*
let echo name n lexbuf =
  Printf.eprintf "Command %s\n" name ;
  for i = 1 to n do
    let arg = subst_arg lexbuf in
    Printf.eprintf "  %i: <<%s>>\n" i arg
  done
*)

let init_listings () =
  Scan.newif_ref "lst@print" lst_print ;
  Scan.newif_ref "lst@includerangemarker" lst_includerangemarker ;
  Scan.newif_ref "lst@extendedchars" lst_extended ;
  Scan.newif_ref "lst@texcl" lst_texcl ;
  Scan.newif_ref "lst@sensitive" lst_sensitive ;
  Scan.newif_ref "lst@mathescape" lst_mathescape ;
  Scan.newif_ref "lst@directives" lst_directives ;
  Scan.newif_ref "lst@stringspaces" lst_string_spaces ;
  Scan.newif_ref "lst@showlines" lst_showlines ;
  Scan.newif_ref "lst@showspaces" lst_showspaces ;
  Scan.newif_ref "lst@showtabs" lst_showtabs ;
  def_code "\\lst@spaces" code_spaces ;
  def_code "\\lst@boolean" lst_boolean ;
  def_code "\\lst@def@stringizer" code_stringizer ;
  def_code "\\lst@AddTo"
    (fun lexbuf ->
      let sep = Scan.get_prim_arg lexbuf in
      let name = Scan.get_csname lexbuf in
      let old =
        try match Latexmacros.find_fail name with
        | _, Subst s -> s
        | _,_        -> []
        with
        | Latexmacros.Failed -> [] in
      let toadd = get_prim_arg lexbuf in
      Latexmacros.def name zero_pat
        (Subst (if is_empty_list old then [toadd] else old@[sep;toadd]))) ;      
  def_code "\\lst@lExtend"
    (fun lexbuf ->
      let name = Scan.get_csname lexbuf in
      try
        match Latexmacros.find_fail name with
        | _, Subst body ->
            let toadd = Subst.subst_arg lexbuf in
            Latexmacros.def name zero_pat
                (Subst (body @ ["%\n"^toadd]))
        | _, _ ->
            warning ("Cannot \\lst@lExtend ``"^name^"''")
      with
      | Latexmacros.Failed ->
            warning ("Cannot \\lst@lExtend ``"^name^"''")) ;

  def_code "\\lstnewenvironment" do_newenvironment ;
(* Special arg parsing for lstlisting *)
  def_code (start_env (lst_user_name "lstlisting"))
    (fun lexbuf ->
      let keys = Subst.subst_opt "" lexbuf in
      let lab =
        if Save.if_next_char '\n' lexbuf then
          ""
        else
          Scan.get_prim_arg lexbuf in
      let lab = if lab = " " then "" else lab in
      if lab <> "" then
        def "\\lst@intname" zero_pat (CamlCode (fun _ -> Dest.put lab)) ;
      scan_this_list Scan.main ("\\lstset{"::keys@["}"])) ;
  def_code (end_env (lst_user_name "lstlisting")) (fun _ -> ()) ;
  def_code (start_env "lstlisting") (open_lst_env "lstlisting") ;
(* Init comments from .hva *)
  def_code "\\lst@balanced@comment"
    (fun lexbuf ->
      code_double_delim lst_process_BBC lst_process_EBC lexbuf) ;
  def_code "\\lst@nested@comment"
    (fun lexbuf ->
      code_double_delim lst_process_BNC lst_process_ENC lexbuf) ;
  def_code "\\lst@line@comment" code_line_comment;
(* Idem for delimters *)
  def_code "\\lst@line@delim"  code_line_delim ;
  def_code "\\lst@single@delim"
    (fun lexbuf -> code_single_delim lst_process_BBC lst_process_EBC lexbuf) ;
  def_code "\\lstinline"
    (fun lexbuf ->
      Image.stop () ;
      let keys = Subst.subst_opt "" lexbuf in
      let {arg=arg} = save_verbatim lexbuf in
      Scan.new_env "*lstinline*" ;
      scan_this main "\\mbox{" ;
      open_lst_inline keys  ;
      Dest.open_group "code" ;
      begin try
        scan_this listings arg
      with
      | Eof _ -> ()
      end ;
      close_lst_inline () ;
      Dest.close_group () ;
      scan_this main "}" ;
      Scan.close_env "*lstinline*" ;
      Image.restart ()) ;

  def_code "\\lst@definelanguage"
    (fun lexbuf ->
      let dialect = get_prim_opt "" lexbuf in
      let language = get_prim_arg lexbuf in
      let base_dialect = get_prim_opt "!*!" lexbuf in

      match base_dialect with
      | "!*!" ->
          let keys = subst_arg_list lexbuf in
          let _ = save_opt "" lexbuf in
          let xs =
            "\\lst@definelanguage@{"::language::"}{"::
            dialect::"}{"::keys@["}"] in
          scan_this_list main xs
      | _  ->
          let base_language = get_prim_arg lexbuf in
          let keys = subst_arg_list lexbuf in
          let _ = save_opt "" lexbuf in
          let xs =
            "\\lst@derivelanguage@{"::
             language::"}{"::dialect::"}{"::
             base_language::"}{"::base_dialect::"}{"::
             keys@["}"] in
          scan_this_list main xs) ;

(* Interpret 'trblTRBL' subset to yield border-style specifications in CSS2 *)
    let string_of_border = function
      | None -> "none"
      | Solid -> "solid"
      | Double -> "double" in

    def_code "\\lst@see@frame"
     (fun lexbuf ->
       let arg = get_prim_arg lexbuf in
       let bs = Array.make 4 None in
       for i = 0 to String.length arg-1 do
         match arg.[i] with
         | 't' -> bs.(0) <- Solid
         | 'T' -> bs.(0) <- Double
         | 'r' -> bs.(1) <- Solid
         | 'R' -> bs.(1) <- Double
         | 'b' -> bs.(2) <- Solid
         | 'B' -> bs.(2) <- Double
         | 'l' -> bs.(3) <- Solid
         | 'L' -> bs.(3) <- Double
         | _ -> ()
       done ;
       let specif = match bs with
       |  [| None ; None ; None ; None |] -> ""
       |  [| Solid ; Solid ; Solid ; Solid |] -> "border-style:solid;"
       |  [| Double ; Double ; Double ; Double |] -> "border-style:double;"
       |  [| bt ; br ; bb ; bl |]
           when bt=bb && br=bl ->
             Printf.sprintf "border-style:%s %s;"
               (string_of_border bt)
               (string_of_border br)
       | [| bt ; br ; bb ; bl |] ->
           Printf.sprintf "border-style:%s %s %s %s;"
             (string_of_border bt)
             (string_of_border br)
             (string_of_border bb)
             (string_of_border bl)
       | _ -> assert false in
       Dest.put specif)
;;

register_init "listings" init_listings
;;


register_init "fancyvrb"
    (fun () ->
      def_code "\\@Verbatim"
        (fun lexbuf ->
          let p, f = open_verbenv false in
          noeof (scan_byline p f) lexbuf) ;
      def_code "\\@endVerbatim" close_verbenv)
;;

let init_longtable () =
  let is_cmd cmd = Latexmacros.exists (cmd^"@lt@exists") in
  def_code "\\@longtable"
    (fun lexbuf ->
      let out = Out.create_buff () in
      let again = scan_bycommand out is_cmd lexbuf in
      scan_this Scan.main again) ;
  def_code "\\lt@exists"
    (fun lexbuf ->
      let cmd = get_csname lexbuf in
      Latexmacros.def (cmd^"@lt@exists") zero_pat (Subst [])) ;
  ()
;;

register_init "longtable" init_longtable
;;

(* mathjax *)

let init_mathjax auto =
  (fun () ->
    Lexstate.jaxauto := auto ;
    def_code "\\@textjax"
      (fun lexbuf ->
        let arg = subst_arg lexbuf in
        Dest.open_group "*mathjax*" ;
        Dest.clearstyle () ;
        Dest.put "\\(" ;
        Dest.put arg ;
        Dest.put "\\)" ;
        Dest.close_group ()) ;
    def_code "\\mathjax"
      (fun lexbuf ->
        Dest.open_group "*mathjax*" ;
        Dest.clearstyle () ;
        open_subst lexbuf) ;
      def_code "\\endmathjax"
      (fun _lexbuf ->
        Dest.close_group ()) ;
    ())
;;

register_init "mathjax@auto" (init_mathjax true);
register_init "mathjax@std" (init_mathjax false)
;;

def_code "\\@scaninput"
  (fun lexbuf ->
    let pre = save_arg lexbuf in
    let file = get_prim_arg lexbuf in
    let {arg=post ; subst=post_subst} = save_arg lexbuf in
    try
      let true_name,chan = Myfiles.open_tex file in
      if !verbose > 0 then
        message ("Scan input file: "^true_name) ;
      let filebuff = Lexing.from_channel chan in
      start_lexstate () ;
      let old_input = !input_verb in
      if old_input then warning "Nested \\@scaninput" ;
      input_verb := true ;
      Location.set true_name filebuff ;
      begin try
        record_lexbuf (MyLexing.from_string post) post_subst ;
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
      close_in chan ;
      input_verb := old_input
    with
    | Myfiles.Except ->
        warning ("Not opening file: "^file)
    | Myfiles.Error s ->
        warning s)
let rien = ()
end
} 

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
module type S =
  sig

    val no_prelude : unit -> unit

    val print_env_pos : unit -> unit
    val main : Lexing.lexbuf -> unit

    (* additional resources needed for extension modules. *)
    val subst : Lexing.lexbuf -> unit
    val subst_arg : (Lexing.lexbuf -> unit) -> Lexing.lexbuf -> string
    val subst_this : (Lexing.lexbuf -> unit) -> string -> string
    val cur_env : string ref
    val new_env : string -> unit
    val close_env : string -> unit
    val env_level : int ref
    val macro_register : string -> unit
    val top_open_block : string -> string -> unit
    val top_close_block : string -> unit
    val get_this : (Lexing.lexbuf -> unit) -> string -> string
  end

module Make (Dest : OutManager.S) =
struct
open Misc
open Parse_opts
open Lexing
open Myfiles
open Latexmacros
open Save
open Tabular
open Lexstate


let header = "$Id: latexscan.mll,v 1.107 1999-06-02 15:42:31 maranget Exp $" 


let sbool = function
  | false -> "false"
  | true  -> "true"

module Index = Index.Make (Dest)
module Foot = Foot.MakeFoot (Dest)


let my_int_of_string s =
  try int_of_string s with
  Failure m -> raise (Misc.ScanError (m^": ``"^s^"''"))
;;

let last_letter name =
  let c = String.get name (String.length name-1) in
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
;;

let top_par n =
  if not (!display || !in_math) then Dest.par n
;;




let if_level = ref 0
;;

let cur_env = ref ""
and macros = ref []
and stack_env = Lexstate.create ()
and env_level = ref 0
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
  let n = Dest.get_fontsize () in
  if n >= 3 then n-1 else n
;;

let open_script_font () =
  Dest.open_mod (Font (get_script_font ()))
;;

let inc_size i =
  let n = Dest.get_fontsize () in
  let new_size =
    if n+i <= 1 then 1
    else if n+i >= 7 then 7
    else n+i in
  Dest.open_mod (Font new_size)
;;

let big_size () =  Dest.open_mod (Font 7)
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
    Dest.open_display (display_arg !verbose)
  end

and top_item_display () =
  if !display then begin
    Dest.item_display ()
  end
;;

let top_close_display () =
  if !display then begin
    Dest.close_display ()
  end

(* vertical display *)
let open_vdisplay () =  
  if !verbose > 1 then
    prerr_endline "open_vdisplay";
  if not !display then
    raise (Misc.Fatal ("VDISPLAY in non-display mode"));
  Dest.open_block "TABLE" (display_arg !verbose)

and close_vdisplay () =
  if !verbose > 1 then
    prerr_endline "close_vdisplay";
  Dest.close_block "TABLE"

and open_vdisplay_row s =
  if !verbose > 1 then
    prerr_endline "open_vdisplay_row";
  Dest.open_block "TR" "" ;
  Dest.open_block "TD" s ;
  Dest.open_display (display_arg !verbose)

and close_vdisplay_row () =
  if !verbose > 1 then
    prerr_endline "close_vdisplay_row";
  Dest.close_display () ;
  Dest.force_block "TD" "&nbsp;" ;
  Dest.close_block "TR"
;;


let open_center () =  Dest.open_block "DIV" "ALIGN=center"
and close_center () = Dest.close_block "DIV"
;;

(* Latex environment stuff *)
let new_env env =
  push stack_env (!cur_env,!macros) ;
  Location.push_pos () ;
  cur_env := env ;
  macros := [] ;
  if env <> "document" then incr env_level ;
  if !verbose > 1 then begin
    Location.print_pos () ;
    Printf.fprintf stderr "Begin : %s <%d>" env !env_level ;
    prerr_endline ""
  end ;
;;

let print_env_pos () =
  Location.print_top_pos () ;
  prerr_endline ("Latex environment ``"^ !cur_env^"'' is pending")
;;

let error_env close_e open_e =
  raise
    (Dest.Close
       ("Latex env error: ``"^close_e^"'' closes ``"^open_e^"''"))
;;

let close_env env  =
  if !verbose > 1 then begin
    Printf.fprintf stderr "End: %s <%d>" env !env_level ;
    prerr_endline  ""
  end ;
  if env = !cur_env then begin  
    if env <> "document" then decr env_level ;
    let e,m = pop stack_env in    
    cur_env := e ;
    macros_unregister () ;
    macros := m ;
    Location.pop_pos ()
  end else
    error_env env !cur_env
;;

      
(* Top functions for blocks *)
let no_display = function
  "TABLE" | "TR" | "TD" | "DISPLAY" | "VDISPLAY" -> true
|  _ -> false
;;
type array_type = {math : bool ; border : bool}
type in_table = Table of array_type | NoTable | Tabbing
;;

let cur_format = ref [||]
and stack_format = Lexstate.create ()
and cur_col = ref 0
and stack_col = Lexstate.create ()
and in_table = ref NoTable
and stack_table = Lexstate.create ()
and first_col = ref false
and first_border = ref false
and stack_first = Lexstate.create ()
and stack_first_b = Lexstate.create ()
and in_multi = ref false
and stack_multi_flag = Lexstate.create ()
and stack_multi = Lexstate.create ()
;;

let top_open_block block args =
  if !verbose > 2 then prerr_endline ("Top open: "^block);
  push stack_table !in_table ;
  in_table := NoTable ;
  begin match block with
    "PRE" ->
      push stack_display !display ;
      if !display then begin
        Dest.item_display () ;
        display := false
      end ;
      Dest.open_block "PRE" args
  | _ ->
      if !display then begin
        Dest.item_display () ; Dest.open_block block args ;
        Dest.open_display (display_arg !verbose)
      end else
        Dest.open_block block args
  end

and top_close_block_aux close_fun block =
  if !verbose > 2 then prerr_endline ("Top close: "^block) ;
  in_table := pop stack_table ;
  begin match block with
    "PRE" ->
      display := pop stack_display ;
      close_fun block ;
      if !display then Dest.item_display ()
  | _ ->
      if !display then begin
        Dest.close_display () ; close_fun block ; Dest.item_display ()
      end else
        close_fun block
  end
;;

let top_close_block block = top_close_block_aux Dest.close_block block
and top_erase_block block = top_close_block_aux Dest.erase_block block

let top_open_group () =
  top_open_block "" "" ; new_env ""

and top_close_group () =
  if !cur_env = "*mbox" then begin
    top_close_block "" ;
    in_math := pop stack_in_math ; display := pop stack_display ;
    if !display then Dest.item_display () ;
    close_env "*mbox"
  end else begin
    top_close_block "" ;
    close_env ""
  end
;;


let do_get_this make_style  lexfun s =
  let par_val = Dest.forget_par () in
  start_normal display in_math ;

  if !verbose > 1 then
    prerr_endline ("get_this : ``"^s^"''") ;  
  let lexer = Lexing.from_string s in
  let r = Dest.to_string (fun () ->
    top_open_group () ;
    make_style () ;
    lexfun lexer ;
    top_close_group ()) in

  if !verbose > 1 then begin
    prerr_endline ("get_this ``"^s^"'' -> ``"^r^"''")
  end ;
  end_normal display in_math ;
  Dest.par par_val ;
  r


let get_this lexfun s = do_get_this (fun () -> ()) lexfun s
and get_this_nostyle lexfun s = do_get_this Dest.nostyle lexfun s
and get_this_clearstyle lexfun s = do_get_this Dest.clearstyle lexfun s

let subst_buff = Out.create_buff ()
;;
let more_buff = Out.create_buff ()
;;

let subst_this subst arg =
try
  let _ = String.index arg '#' in
  if !verbose > 1 then begin
    Printf.fprintf stderr "subst_this : [%s]\n" arg ;
    prerr_args ()
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

  
let put_delim delim i =
  if !verbose > 1 then
    prerr_endline
     ("put_delim: ``"^delim^"'' ("^string_of_int i^")") ;
  if delim <> "." then begin
    Dest.begin_item_display (fun () -> ()) false ;
    Symb.put_delim Dest.skip_line Dest.put delim i ;
    let _ = Dest.end_item_display () in ()
  end
;;

let default_format =
  Tabular.Align
    {hor="left" ; vert = "" ; wrap = false ;
      pre = "" ; post = "" ; width = None}

and center_format =
  Tabular.Align
    {hor="center" ; vert = "top" ; wrap = false ;
      pre = "" ; post = "" ; width = None} 
;;


let pretty_array_type = function
  | Table {math = m ; border = b} ->
      "Table math="^(if m then "+" else "-")^
      " border="^(if b then "+" else "-")
  | NoTable -> "NoTable"
  | Tabbing -> "Tabbing"

let is_table = function
  | Table _ -> true
  | _       -> false

and is_noborder_table = function
  | Table {border = b} -> not b
  | _                  -> false

and is_tabbing = function
  | Tabbing -> true
  | _ -> false

and math_table = function
  | Table {math = m} -> m
  | _ -> raise (Misc.Fatal "math_table")
;;

let par_val t =
  if is_table t then Some 0 else Some 1

let prerr_array_state () =
  prerr_endline (pretty_array_type !in_table) ;
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
  push stack_first !first_col;
  push stack_first_b !first_border;
  push stack_multi_flag !in_multi ;
  in_multi := false ;
  if !verbose > 1 then begin
    prerr_endline "Save array state:" ;
    prerr_array_state ()
  end    

and restore_array_state () =
  in_table := pop stack_table ;
  cur_col := pop stack_col ;
  cur_format := pop stack_format ;
  first_col := pop stack_first ;
  first_border := pop stack_first_b;
  in_multi := pop stack_multi_flag ;
  if !verbose > 1 then begin
    prerr_endline "Restore array state:" ;
    prerr_array_state ()
  end  
;;

exception EndInside
;;
exception NoMulti
;;

let attribut name = function
  | "" -> ""
  | s  -> " "^name^"="^s

and as_colspan = function
  |  1  -> ""
  |  n -> " COLSPAN="^string_of_int n

let is_inside = function
    Tabular.Inside _ -> true
  | _ -> false

let is_border = function
  | Tabular.Border _ -> true
  | _ -> false

and as_inside = function
  Tabular.Inside s -> s
| _        -> ""
(*
and as_align f span = match f with
  Tabular.Align {vert=v ; hor=h ; wrap=w ; width=size} ->
(*    (match size with
    | Some (Length.Percent n) ->
        attribut "WIDTH" (string_of_int n^"%")
    | Some (Length.Absolute n) ->
        attribut "WIDTH" (string_of_int (n * Length.font))
    | _ -> "")^ *)
    attribut "VALIGN" v^
    attribut "ALIGN" h^
    (if w then "" else " NOWRAP")^
    as_colspan span
| _       ->  raise (Misc.Fatal ("as_align"))
*)
and as_wrap = function
  | Tabular.Align {wrap = w} -> w
  | _ -> false

and as_pre = function
  | Tabular.Align {pre=s} -> s
  | _ -> raise (Misc.Fatal "as_pre")

and as_post = function
  | Tabular.Align {post=s} -> s
  | f -> raise (Misc.Fatal ("as_post "^pretty_format f))
;;

let get_col format i =
  let r = 
    if i >= Array.length format+1 then
      raise (Misc.ScanError ("This array/tabular column has no specification"))
    else if i = Array.length format then default_format
    else format.(i) in
  if !verbose > 2 then begin
   Printf.fprintf stderr "get_col : %d: " i ;
   prerr_endline (pretty_format r) ;
   prerr_string " <- " ;
   pretty_formats format ;
   prerr_newline ()
  end ;
  r
;;

let show_inside main format i closing =
(*
  if !verbose > -1 then begin
    prerr_string ("show_inside: "^string_of_int i)
  end ;
*)
  let t = ref i in
  begin try while true do
    begin match get_col format !t with
      Tabular.Inside s ->
        let s = get_this main s in
	Dest.make_inside s !in_multi;
(*
        Dest.open_cell center_format 1;
        Dest.put s ;
        Dest.close_cell "";
*)
    | Tabular.Border s -> 
	Dest.make_border s;
	if !first_border then first_border := false;
    | _ -> raise EndInside
    end ;
    t := !t+1
  done with EndInside ->
    if (!t = i) && (closing || !first_border)  then
      Dest.make_border " ";
  end ;
(*
  if !verbose > -1 then
    prerr_endline (" -> "^string_of_int !t) ;
*)
  !t
;;

let rec eat_inside format i b insides =
  if i >= Array.length format then (i , b , insides)
  else begin
    let f = get_col format i in
    if is_inside f then
      eat_inside format (i+1) b (insides+1)
    else if is_border f then
      eat_inside format (i+1) (b+1) insides
    else i, b, insides
  end
;;

let rec find_end n format i b insides = match n with
  0 -> eat_inside format i b insides
| _ ->
   let f = get_col format i in
   if is_inside f then
     find_end n format (i+1) b (insides +1)
   else if is_border f then
     find_end n format (i+1) (b+1) insides
   else
     find_end (n-1) format (i+1) b insides
;;


let find_start i = if !first_border then 0 else i

let find_align format =
  let t = ref 0 in
  while (is_inside (get_col format !t)) || (is_border (get_col format !t)) do
    t := !t+1
  done ;
  !t
;;

let next_no_border format n =
  let t = ref n in
  while is_border (get_col format !t) do
    t:= !t+1
  done;
  !t
;;

let show_inside_multi main format i j =
  let rec show_rec i =
    if i >= j then ()
    else begin
      let s = get_this main (as_inside (get_col format i)) in
      Dest.put s ;
      show_rec (i+1)
    end in
  show_rec i
;;

let do_open_col main format span insides =
  let save_table = !in_table in
  Dest.open_cell format span insides;
  if not (as_wrap format) && math_table !in_table then begin
    display  := true ;
    Dest.open_display (display_arg !verbose)
  end ;
  if math_table !in_table && not (as_wrap format) then begin
    scan_this main "$"
  end ;
  scan_this main (as_pre format) ;
  in_table := save_table 

let open_col main  =
  let _ = Dest.forget_par () in
  Dest.open_cell_group () ;
  cur_col :=  show_inside main !cur_format !cur_col false;
  let format = (get_col !cur_format !cur_col) in
  do_open_col main format 1 0
;;

let open_first_col main =
  first_col := true ;
  first_border := true;
  open_col main  
;;

let erase_col main =
  let old_format = get_col !cur_format !cur_col in
  scan_this main (as_post old_format) ;
  if math_table !in_table  && not (as_wrap old_format) then
    scan_this main "$" ;
  if !display then begin
    Dest.close_display () ;
    display := false
  end ;
  Dest.erase_cell () ;
  Dest.erase_cell_group ()
;;


let open_row () =
  cur_col := 0 ;
  Dest.new_row ()

and close_row () = Dest.close_row ()
;;


let do_hline main =
  if !verbose > 2 then begin
    Printf.fprintf stderr "hline: %d %d" !cur_col (Array.length !cur_format) ;
    prerr_newline ()
  end ;
  erase_col main ;
  Dest.erase_row () ;
  
  Dest.make_hline (Array.length !cur_format) (is_noborder_table !in_table);
(*
  Dest.new_row () ;
  Dest.open_cell center_format (Array.length !cur_format) ;
  Dest.close_mods () ;
  Dest.horizontal_line "NOSHADE" "2" "100" ;
  Dest.close_cell "" ;
  Dest.close_row () ;
*)  
  open_row () ;
  open_first_col main
;;

let do_multi n format main =
  if !verbose > 2 then begin
    prerr_string
      ("multicolumn: n="^string_of_int n^" format:") ;
    pretty_formats format ;
    prerr_endline ""
  end ;

  erase_col main ;

  let start_span = find_start !cur_col
  and k,b,insides = find_end n !cur_format !cur_col 0 0 in
  let end_span = k - b in

  in_multi := true;

  let i = show_inside main format 0 true in
(* let i = find_align format in*)

  Dest.open_cell_group () ;
  do_open_col main (get_col format i) (end_span - start_span) insides;
  push stack_multi (!cur_format,k) ;
  cur_format := format ;
  cur_col := i ;
;;


let close_col_aux main content is_last =
  let old_format = get_col !cur_format !cur_col in
  scan_this main (as_post old_format) ;
  if math_table !in_table && not (as_wrap old_format) then
    scan_this main "$" ;
  if !display then begin
    Dest.close_display () ;
    display := false
  end ;
  if is_last && Dest.is_empty () then Dest.erase_cell ()
  else begin
    if !in_multi then begin
      let _ = show_inside main !cur_format (!cur_col+1) true in
      in_multi := false ;
      let f,n = pop stack_multi in
      cur_format := f ;
      cur_col := next_no_border f n;
      cur_col := show_inside main !cur_format !cur_col false;
    end else begin
      cur_col := !cur_col + 1;
      cur_col := show_inside main !cur_format !cur_col true;
    end;
    Dest.close_cell content;
    if !first_col then begin
      first_col := false;
      first_border := false;
    end
  end ;
  Dest.close_cell_group ()
;;

let close_col main content = close_col_aux main content false
and close_last_col main content = close_col_aux main content true

and close_last_row () =
  if !first_col then
    Dest.erase_row ()
  else
    Dest.close_row ()
;;

(*
let rec open_ngroups = function
  | 0 -> ()
  | n -> Dest.open_group "" ; open_ngroups (n-1)

let rec close_ngroups = function
  | 0 -> ()
  | n -> Dest.force_block "" "" ; close_ngroups (n-1)
*)

(* Compute functions *)

let get_style lexfun s =
  start_normal display in_math ;
  let lexer = Lexing.from_string s in
  let r = Dest.to_style (fun () -> lexfun lexer) in
  end_normal display in_math ;
  r

let check_this lexfun s =
  if !verbose > 1 then
    prerr_endline ("check_this: ``"^s^"''");
  start_normal display in_math ;
  let save_par = Dest.forget_par () in
  Dest.open_block "TEMP" "";
  let r =
    try
      scan_this lexfun s ;
      true
    with
    |  x -> false in
  Dest.erase_block "TEMP" ;
  Dest.par save_par ;
  end_normal display in_math ;
  if !verbose > 1 then
    prerr_endline ("check_this: ``"^s^"'' = "^sbool r);
  r
  

(* Image stuff *)

let iput_newpage arg =
  let n = Image.page () in
  Image.put ("% page: "^n^"\n") ;
  Image.put "\\clearpage\n\n" ;
  Dest.image arg n

(*  Dest.put "<IMG " ;
  if arg <> "" then begin
    Dest.put arg;
    Dest.put_char ' '
  end ;
  Dest.put "SRC=\"" ;
  Dest.put n ;
  Dest.put "\">"*)
;;


let stack_entry = Lexstate.create ()
and stack_out = Lexstate.create ()
;;

let start_other_scan env lexfun lexbuf =
  if !verbose > 1 then begin
    prerr_stack_string ("Start other scan ("^env^"), env stack is")
      (fun (x,_) -> x) stack_env ;
    prerr_endline ("Current env is: ``"^ !cur_env^"''") ;
    prerr_stack_string "Entry stack is" (fun x -> x) stack_entry
  end;
  save_lexstate () ;
  push stack_entry env ;
  Lexstate.rev stack_entry ;
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
      (fun (x,_) -> x) stack_env ;
    prerr_endline ("Current env is: ``"^ !cur_env^"''")
  end
;;


let stop_other_scan comment main lexbuf =
  if !verbose > 1 then begin
    prerr_stack_string "Stop image: env stack is"
      (fun (x,_) -> x) stack_env ;
    prerr_endline ("Current env is: ``"^ !cur_env^"''")
  end;
  let _ = pop stack_entry in
  if not comment then close_env !cur_env ;
  if not (Lexstate.empty stack_out) then begin
    complete_scan main lexbuf ;
    while not (Lexstate.empty stack_out) do
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
    -1 -> raise (Misc.Fatal "Empty string in check_rec")
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
| s  ->
    Dest.open_group tag ;
    open_script_font () ;
    scan_this main s;
    Dest.close_group ()
;;



let standard_sup_sub main what sup sub =
  if !display && (complex sup || complex sub) then begin
    Dest.force_item_display () ;
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
      Dest.force_item_display ()
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
    Dest.force_item_display () ;
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
    Dest.force_item_display ()
  end
;;

let int_sup_sub something vsize main what sup sub =
  if something then begin
    Dest.force_item_display () ;
    what () ;
    Dest.force_item_display ()
  end ;
  if sup <> "" || sub <> "" then begin
    open_vdisplay () ;
    open_vdisplay_row "ALIGN=left NOWRAP" ;
    open_script_font () ;
    scan_this main sup ;
    close_vdisplay_row () ;
    open_vdisplay_row "ALIGN=left" ;
    for i = 2 to vsize do
      Dest.skip_line ()
    done ;
    close_vdisplay_row () ;
    open_vdisplay_row "ALIGN=left NOWRAP" ;
    open_script_font () ;
    scan_this main sub ;
    close_vdisplay_row () ;
    close_vdisplay () ;
    Dest.force_item_display ()
  end
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


let no_prelude () =
  if !verbose > 1 then prerr_endline "Filter mode" ;
  flushing := true ;
  prelude := false ;
  let _ = Dest.forget_par () in () ;
  Dest.set_out out_file
;;

let macro_depth = ref 0
;;

let expand_command main skip_blanks name lexbuf =
  let exec = function
    | Subst body ->
        if !verbose > 2 then
          prerr_endline ("user macro: "^body) ;            
        scan_this_may_cont main lexbuf body
    | CamlCode f -> f lexbuf in

  let pat,body = find_macro name in
  let args = make_stack name pat lexbuf in
  let saw_par = !Save.seen_par in
  let is_limit = checklimits lexbuf ||  Latexmacros.limit name in
  if not !alltt && (is_limit || Latexmacros.big name) then begin
    let sup,sub = Save.get_sup_sub lexbuf in
    let do_what =
      (fun () -> scan_body exec body args) in
    if !display && is_limit then
      limit_sup_sub main do_what sup sub
    else if !display &&  Latexmacros.int name then
      int_sup_sub true 3 main do_what sup sub
    else
      standard_sup_sub main do_what sup sub
  end else begin
    if (!verbose > 1) then begin
      prerr_endline
        ("Expanding macro "^name^" {"^(string_of_int !macro_depth)^"}") ;
      macro_depth := !macro_depth + 1
    end ;
    push stack_alltt !alltt ;
    if !alltt then begin
      alltt := false ;
      scan_body exec body args ;
      alltt := not !alltt
    end else
      scan_body exec body args ;
    let _ = pop stack_alltt in
    if (!verbose > 1) then begin
      prerr_endline ("Cont after macro "^name^": ") ;
      macro_depth := !macro_depth - 1
    end ;
    if saw_par then top_par (par_val !in_table)
    else if
      (!in_math && Latexmacros.invisible name) ||
      (not !in_math && not !alltt &&
       is_subst_noarg body pat && last_letter name)
    then begin
      if !verbose > 2 then
        prerr_endline "skipping blanks";
      skip_blanks lexbuf
    end else begin
      if !verbose > 2 then begin
        prerr_endline "not skipping blanks"
      end
    end ;
  end
;;

} 

let command_name = '\\' (('@' ? ['A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])

rule  main = parse
(* comments *)
   '%'+
   {if !alltt then begin
     let lxm = lexeme lexbuf in
     Dest.put lxm ;
     main lexbuf
   end else
     comment lexbuf}

(* Paragraphs *)
  | '\n' +
      {let nlnum = String.length (lexeme lexbuf) in
       if !withinLispComment
       then begin
         afterLispCommentNewlines := nlnum;
         if !verbose > 2 then prerr_endline "NL caught after LispComment"
       end else begin
         if !alltt then
           Dest.put (lexeme lexbuf)
         else if nlnum >= 2 then
           top_par (par_val !in_table)
         else
           Dest.put_separator () ;
         main lexbuf
       end}
(* subscripts and superscripts *)
  | ('_' | '^')
     {let lxm = lexeme lexbuf in
     if !alltt then Dest.put lxm
     else if not !in_math then begin
       warning ("``"^lxm^"''occuring outside math mode") ;
       Dest.put lxm
     end else begin
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
| "$" | "$$"
     {let lxm = lexeme lexbuf in
     if !alltt then begin
       Dest.put lxm ; main lexbuf
     end else begin
       let dodo = lxm <> "$" in
       let math_env = if dodo then "*display" else "*math" in
       if !in_math then begin
         in_math := pop stack_in_math ;
         if dodo then begin
           Dest.close_display () ;
           close_center ()
         end else begin
           top_close_display () ;
           Dest.close_group ()
         end ;
         display := pop stack_display ;
         if !display then begin
           Dest.item_display ()
         end ;
         close_env math_env ;
         main lexbuf
     end else begin
       push stack_in_math !in_math ;
       in_math := true ;
       let lexfun lb =
         if !display then  Dest.item_display () ;
         push stack_display !display ;
         if dodo then begin
           display  := true ;
           open_center() ;
           Dest.open_display (display_arg !verbose)
         end else begin
           Dest.open_group "" ;
           top_open_display () ;
         end;
         skip_blanks lb ; main lb in
       new_env math_env ;
       lexfun lexbuf
     end end}

(* Definitions of  simple macros *)
(* inside tables and array *)
  | [' ''\n']* "&"
    {if !alltt then begin
      let lxm = lexeme lexbuf in
      for i = 0 to String.length lxm -1 do
        Dest.put (Dest.iso lxm.[i])
      done
    end else if is_table !in_table  then begin
      close_col main "&nbsp;"; 
      open_col main
    end ;
    skip_blanks_pop lexbuf ;
    main lexbuf}
(* Substitution  *)
  | '#' ['1'-'9']
      {let lxm = lexeme lexbuf in
      begin if !alltt then
        Dest.put lxm
      else
        let i = Char.code lxm.[1] - Char.code '1' in
        scan_arg (scan_this main) i
      end ;
      main lexbuf}
(* Commands *)
  | command_name
      {let name = lexeme lexbuf in
      expand_command main skip_blanks name lexbuf ;
      main lexbuf}
(* Groups *)
| '{' 
    {if !activebrace then
      top_open_group ()
    else begin
      Dest.put_char '{'
    end ;
    main lexbuf}
| '}' 
    {if !activebrace then begin
      let cenv = !cur_env in
      top_close_group ()
    end else begin
      Dest.put_char '}'
    end ;
    main lexbuf}
| eof {()}
| ' '+
   {if !alltt then
     let lxm = lexeme lexbuf in Dest.put lxm
   else
     Dest.put_char ' ';
   main lexbuf}
(* Alphabetic characters *)
| ['a'-'z' 'A'-'Z']+
   {let lxm =  lexeme lexbuf in
   if !in_math then begin
      Dest.put_in_math lxm;
    end else
      Dest.put lxm ;
    main lexbuf}
(* Html specials *)
| '<'
    {Dest.put (Dest.iso '<') (*"&lt;"*) ; main lexbuf}
| '>'
    {Dest.put (Dest.iso '>') (*"&gt;"*) ; main lexbuf}
| '~'         { Dest.put_nbsp ()(*"&nbsp;"*); main lexbuf }
(* Spanish stuff *)
| "?`"
    {Dest.put (Dest.iso '¿') ;
    main lexbuf}
| "!`"
  {Dest.put (Dest.iso '¡') ;
  main lexbuf}
(* One character *)
| _ 
   {let lxm = lexeme_char lexbuf 0 in
   Dest.put (Dest.iso lxm) ;
   main lexbuf}

and latex2html_latexonly = parse
| '%' + [ ' ' '\t' ] * "end{latexonly}" [ ^ '\n' ] * '\n'
    { () }
| _ 
    {latex2html_latexonly lexbuf}
| eof
    {failwith "EOF in latexonly"}

and latexonly = parse
   '%'+ ' '* ("END"|"end") ' '+ ("LATEX"|"latex")  [^'\n']* '\n'
     {stop_other_scan true main lexbuf}
|  '%'+ ' '* ("HEVEA"|"hevea") ' '*
     {latexonly lexbuf}
|  '%'
     {latex_comment lexbuf ; latexonly lexbuf}
|  "\\end"
     {let arg = save_arg lexbuf in
     if arg = "latexonly" then begin
       stop_other_scan false main lexbuf
     end else if arg = top stack_entry then begin
       let _ = pop stack_entry in
       push stack_out arg ;
       begin match find_macro (end_env arg) with
         _,(Subst body) ->
           scan_this_may_cont latexonly lexbuf body
       |  _,_ ->
           raise (Misc.ScanError ("Bad closing macro in latexonly: ``"^arg^"''"))
       end
     end else
       latexonly lexbuf}
| command_name  | _ {latexonly lexbuf}
| eof
    {if Lexstate.empty stack_lexbuf then ()
    else begin
      let lexbuf = previous_lexbuf () in
      latexonly lexbuf
    end}


and latex_comment = parse
  '\n' | eof  {()}
| [^'\n']+    {latex_comment lexbuf}



and image = parse
   '%'+ ' '* ("END"|"end") ' '+ ("IMAGE"|"image")  [^'\n']* '\n'
     {Image.put_char '\n' ; stop_other_scan true main lexbuf}
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
    scan_arg (scan_this image) i ;
    image lexbuf}
|  "\\end"
     {let lxm = lexeme lexbuf in
     Save.start_echo () ;
     let arg = save_arg lexbuf in
     let true_arg = Save.get_echo () in
     if arg = "toimage" then begin
       Image.put_char '\n' ;
       stop_other_scan false main lexbuf
     end else if arg = top stack_entry then begin
       let _ = pop stack_entry in
       push stack_out arg ;
       begin match find_macro (end_env arg) with
         _,(Subst body) ->
           scan_this_may_cont  image lexbuf body
       |  _,_ -> raise (Misc.ScanError ("Bad closing macro in image: ``"^arg^"''"))
       end
     end else begin
       Image.put lxm ; Image.put true_arg ;
       image lexbuf
     end}
|  command_name
    {let lxm = lexeme lexbuf in
    begin match lxm with
(* Definitions of  simple macros, bodies are not substituted *)
    | "\\def" | "\\gdef" ->
        Save.start_echo () ;
        let _ = Save.csname lexbuf in
        let _ = Save.defargs lexbuf in
        let _ = save_arg lexbuf in
        Image.put lxm ;
        Image.put (Save.get_echo ())
    | "\\renewcommand" | "\\newcommand" | "\\providecommand" ->
        Save.start_echo () ;
        let _ = Save.csname lexbuf in
        let _ = parse_args_opt ["0" ; ""] lexbuf in
        let _ = save_arg lexbuf in
        Image.put lxm ;
        Image.put (Save.get_echo ())
    | "\\newenvironment" | "\\renewenvironment" ->
        Save.start_echo () ;
        let _ = save_arg lexbuf in
        let _ = parse_quote_arg_opt "0" lexbuf in
        let _ = parse_quote_arg_opt "" lexbuf in
        let _ = save_arg lexbuf in
        let _ = save_arg lexbuf in
        Image.put lxm ;
        Image.put (Save.get_echo ())
    | _ -> Image.put lxm end ;
    image lexbuf}
| _
     {let s = lexeme lexbuf in
     Image.put s ;
     image lexbuf}
| eof
    {if Lexstate.empty stack_lexbuf then ()
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
| ' '+ | '\n' {mbox_arg lexbuf}
| eof
     {if not (Lexstate.empty stack_lexbuf) then begin
     let lexbuf = previous_lexbuf () in
     if !verbose > 2 then begin
       prerr_endline "Poping lexbuf in mbox_arg" ;
       pretty_lexbuf lexbuf
     end ;
     mbox_arg lexbuf
   end else raise (Misc.ScanError "End of file in \\mbox argument")}
| '{' | ("\\bgroup" ' '* '\n'? ' '*)
    {push stack_table !in_table ; in_table := NoTable ;
    push stack_in_math !in_math ; in_math := false ;
    if !display then Dest.item_display () ;
    push stack_display !display ; display := false ;
    Dest.open_block "" "" ;
    new_env "*mbox"}

and skip_blanks_pop = parse
  ' '+ {skip_blanks_pop lexbuf}
| '\n' {more_skip_pop lexbuf}
| ""   {()}
| eof
   {if not (Lexstate.empty stack_lexbuf) then begin
     let lexbuf = previous_lexbuf () in
     if !verbose > 2 then begin
       prerr_endline "Poping lexbuf in skip_blanks" ;
       pretty_lexbuf lexbuf
     end ;
     skip_blanks_pop lexbuf
   end else ()}

and more_skip_pop = parse
  '\n'+ {top_par (par_val !in_table)}
| ""    {skip_blanks_pop lexbuf}
| eof
   {if not (Lexstate.empty stack_lexbuf) then begin
     let lexbuf = previous_lexbuf () in
     if !verbose > 2 then begin
       prerr_endline "Poping lexbuf in skip_blanks" ;
       pretty_lexbuf lexbuf
     end ;
     more_skip_pop lexbuf
   end else ()}

and to_newline = parse
|  '\n' {()}
| _     {Out.put_char more_buff (Lexing.lexeme_char lexbuf 0) ;
        to_newline lexbuf}
| eof
   {if not (Lexstate.empty stack_lexbuf) then
     let lexbuf = previous_lexbuf () in
     to_newline lexbuf}

and skip_blanks = parse
  ' '+ {skip_blanks lexbuf}
| '\n' {more_skip lexbuf}
| ""   {()}

and more_skip = parse
  '\n'+ {top_par (par_val !in_table)}
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
     {if !if_level = 0 then skip_blanks lexbuf
     else skip_false lexbuf}
| "\\fi" ['a'-'z' 'A'-'Z']+
     {skip_false lexbuf}
| "\\fi"
     {if !if_level = 0 then begin
        skip_blanks lexbuf
     end else begin
       if_level := !if_level -1 ;
       skip_false lexbuf
     end}
| _ {skip_false lexbuf}


and comment = parse
  ' '* ("BEGIN"|"begin") ' '+ ("IMAGE"|"image")
    {skip_comment lexbuf ; start_image_scan "" image lexbuf ; main lexbuf}
(* Backward compatibility with latex2html *)
| [ ' ' '\t' ] * "begin{latexonly}"
    {latex2html_latexonly lexbuf;
     main lexbuf}
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
   if !flushing then Dest.flush_out () }
| "" {raise (Misc.ScanError "Latex comment is not terminated")}

and subst = parse
'#' ['1'-'9']
    {let lxm = lexeme lexbuf in
    let i = Char.code (lxm.[1]) - Char.code '1' in
    scan_arg (fun arg -> subst (Lexing.from_string arg)) i ;
    subst lexbuf}
| '#' '#'
    {Out.put_char subst_buff '#' ; subst lexbuf}
|  "\\#" | '\\' | [^'\\' '#']+
    {Out.put subst_buff (lexeme lexbuf) ; subst lexbuf}
|  eof {()}

{
let check_alltt_skip lexbuf =
  if not (Lexstate.empty stack_alltt) && pop stack_alltt then begin
    push stack_alltt true
  end else begin
    push stack_alltt false ;
    skip_blanks lexbuf
  end
;;

let def_fun name f =
  def_code name
    (fun lexbuf ->
      let arg = subst_arg subst lexbuf in
      scan_this main (f arg))
and def_name_code name f = def_code name (f name)
;;

(* Styles and packages *)
let do_documentclass command lexbuf =
  Save.start_echo () ;
  let opt = parse_quote_arg_opt "" lexbuf in
  let arg =  save_arg lexbuf in
  let echo_args = Save.get_echo () in
  begin try if not !styleloaded then
    input_file 0 main (arg^".hva")
  with
    Myfiles.Except | Myfiles.Error _ ->
      raise (Misc.ScanError ("No base style"))
  end ;
  Image.start () ;
  Image.put command ;
  Image.put echo_args ;
  Image.put "\n"
;;

def_name_code  "\\documentstyle" do_documentclass ;
def_code  "\\documentclass" (do_documentclass "\\documentclass")
;;


let do_input lxm lexbuf =
  Save.start_echo () ;
  let arg = Save.input_arg lexbuf in
  let echo_arg = Save.get_echo () in
  let arg = subst_this subst arg in
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
  end
;;

def_code "\\input" (do_input "\\input") ;
def_code "\\include" (do_input "\\include") ;
def_code "\\bibliography" (do_input "\\bibliography")
;;

(* Command definitions *)
let subst_body s = if top_level () then s else subst_this subst s
;;

let do_newcommand lxm lexbuf =
  Save.start_echo () ;
  let name = subst_this subst (Save.csname lexbuf) in
  let nargs = parse_args_opt ["0" ; ""] lexbuf in
  let body =
    if top_level () then save_arg lexbuf
    else subst_arg subst lexbuf in    
  if (!env_level = 0) && lxm <> "\\@forcecommand"  then
    Image.put
      (lxm^Save.get_echo ()^"\n") ;
  let nargs,(def,defval) = match nargs with
    [a1 ; a2] ->
      Get.get_int (from_ok a1),
      (match a2 with
      | No s -> false,s
      | Yes s -> true,s)
  | _ -> assert false in
  begin try
    (match lxm with
      "\\newcommand"   -> def_macro_pat
    | "\\renewcommand" -> redef_macro_pat
    | "\\@forcecommand" -> silent_def_pat        
    | _                -> provide_macro_pat) name
      (Latexmacros.make_pat
         (if def then
           [subst_body  defval]
         else []) nargs)
      (Subst body) ;
    macro_register name
  with Latexmacros.Failed -> () end
;;

def_name_code "\\renewcommand" do_newcommand  ;
def_name_code "\\newcommand" do_newcommand ;
def_name_code "\\providecommand" do_newcommand ;
def_name_code "\\@forcecommand" do_newcommand ;
;;

def_name_code "\\newcolumntype"
  (fun lxm lexbuf ->
    Save.start_echo () ;
    let name = subst_this subst (Save.csname lexbuf) in
    let nargs = save_opt "0" lexbuf in
    let body = save_arg lexbuf in
    let rest = Save.get_echo () in
    if !env_level = 0 then
      Image.put (lxm^rest^"\n") ;
    def_coltype
      name
      (Latexmacros.make_pat [] (Get.get_int nargs))
      (Subst body) ;
    macro_register (Misc.column_to_command name))
;;

let do_newenvironment lxm lexbuf =
  Save.start_echo () ;
  let name = subst_arg subst lexbuf in
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
         (match optdef with
         | No _ -> []
         | Yes s -> [subst_body s])
         (match nargs with No _ -> 0 | Yes s -> Get.get_int s))
      (Subst (subst_body body1)) (Subst (subst_body body2));
    macro_register (start_env name) ; 
    macro_register (end_env name)
  with Latexmacros.Failed -> () end
;;

def_name_code "\\newenvironment" do_newenvironment ;
def_name_code  "\\renewenvironment" do_newenvironment
;;

let do_newtheorem lxm lexbuf =
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
        def_macro ("\\the"^name) 0 (Subst ("\\arabic{"^name^"}")) ;
        name
    | _,Yes within ->
        Counter.def_counter name within ;
        def_macro ("\\the"^name) 0
          (Subst ("\\the"^within^".\\arabic{"^name^"}")) ;
        name
    | Yes numbered_like,_ -> numbered_like in
    
    def_env_pat name (Latexmacros.make_pat [""] 0)
      (Subst ("\\cr{\\bf\\stepcounter{"^cname^"}"^caption^"~"^
              "\\the"^cname^"}\\quad\\ifoptarg{\\purple[#1]\\quad}\\fi\\it"))
      (Subst "\\cr")
  with Latexmacros.Failed -> () end
;;

def_name_code "\\newtheorem" do_newtheorem ;
def_name_code "\\renewtheorem" do_newtheorem
;;

(* Command definitions, TeX style *)

let do_def global lxm lexbuf =
  let name = Save.csname lexbuf in
  let name = subst_this subst name in
  let args_pat = Save.defargs lexbuf in
  let body = save_arg lexbuf in
  if !env_level = 0 || global then
    Image.put
      (lxm^name^
       (List.fold_right (fun s r -> s^r) args_pat ("{"^body^"}\n"))) ;
  begin try
    def_macro_pat name ([],args_pat) (Subst body) ;
    if not global then macro_register name
  with Latexmacros.Failed -> () end
;;

def_name_code "\\def" (do_def false) ;
def_name_code "\\gdef" (do_def true)
;;

let do_let global lxm lexbuf =
  let name = subst_arg subst lexbuf in
  Save.skip_equal lexbuf ;
  let alt = subst_arg subst lexbuf in
  begin try
    let nargs,body = find_macro alt in
    begin try
      def_macro_pat name nargs body ;
      if not global then macro_register name
    with Latexmacros.Failed -> () end
  with Not_found -> () end ;
  if !env_level = 0 || global then begin
    Image.put lxm ;
    Image.put name ;
    Image.put "=" ;
    Image.put alt ;
    Image.put "\n"
  end ;
  if not global then macro_register name
;;

def_name_code "\\let" (do_let false)
;;

let do_global lxm lexbuf =
  let next = save_arg lexbuf in
  begin match next with
  | "\\def" -> do_def true (lxm^next) lexbuf
  | "\\let" -> do_let true (lxm^next) lexbuf
  | _       -> warning "Ignored \\global"
  end
;;



def_name_code "\\global" do_global
;;

(* TeXisms *)
def_code "\\noexpand"
  (fun lexbuf ->
     let arg = save_arg lexbuf in
     Dest.put arg)
;;

let do_unskip () =
 let _ = Dest.forget_par () in
 Dest.unskip ()
;;

def_code "\\unskip"
    (fun lexbuf ->
      do_unskip () ;
      check_alltt_skip lexbuf)
;;

def_code "\\csname"
  (fun lexbuf ->
    skip_blanks lexbuf ;
    let name = "\\"^subst_this subst (Save.incsname lexbuf) in
    check_alltt_skip lexbuf ;
    expand_command main skip_blanks name lexbuf)
;;

(* Complicated use of output blocks *)
def_code "\\left"
  (fun lexbuf ->
    if !display then begin
      let _,f,is_freeze = Dest.end_item_display () in
      let delim = save_arg lexbuf in
      Dest.delay (fun vsize -> put_delim delim vsize) ;
      Dest.begin_item_display f is_freeze
    end)
;;

def_code "\\right"
  (fun lexbuf ->
    if !display then begin
      let delim = save_arg lexbuf in
      let vsize,f,is_freeze = Dest.end_item_display () in
      put_delim delim vsize;
      Dest.flush vsize ;
      Dest.begin_item_display f is_freeze ;
      let sup,sub = Save.get_sup_sub lexbuf in
      let do_what = (fun () -> ()) in
      int_sup_sub false vsize main do_what sup sub
    end ;
    check_alltt_skip lexbuf)
;;

def_code "\\over"
  (fun lexbuf ->
    if !display then begin
      let mods = Dest.insert_vdisplay
          (fun () ->
            open_vdisplay () ;
            open_vdisplay_row "NOWRAP ALIGN=center") in
      close_vdisplay_row () ;
      open_vdisplay_row "" ;
      Dest.close_mods () ;
      Dest.horizontal_line  "NOSHADE" "2" "100";
      close_vdisplay_row () ;
      open_vdisplay_row "NOWRAP ALIGN=center" ;
      Dest.close_mods () ;
      Dest.open_mods mods ;
      Dest.freeze
        (fun () ->
          close_vdisplay_row () ;
          close_vdisplay ())
    end else begin
      Dest.put "/"
    end)
;;

let check_not = function
  | "\\in" -> "\\notin"
  | "="    -> "\\neq"
  | "\\subset" -> "\\notsubset"
  | s -> "\\neg\\:"^s
;;

def_fun "\\not" check_not
;;

def_fun "\\upercase" String.uppercase
;;

(* list items *)
def_code "\\@li" (fun _ -> Dest.item ()) ;
def_code "\\@linum" (fun _ -> Dest.nitem ()) ;
def_code "\\@dt"
  (fun lexbuf ->
    let arg = save_arg lexbuf in
    Dest.ditem (scan_this main) arg)
;;

    
(* Html primitives *)
def_code "\\@open"
  (fun lexbuf ->
    let tag = save_arg lexbuf in
    let arg = save_arg lexbuf in
    if no_display tag then begin
      if tag="DISPLAY" then begin
        push stack_display !display;
        display := true ;
        top_open_display ()
      end else if tag="VDISPLAY" then begin
        top_item_display () ;
        open_vdisplay () ;
        open_vdisplay_row "NOWRAP ALIGN=center"
      end else begin
        warning ("direct opening of "^tag);
        top_open_block tag arg
      end
    end else
      top_open_block tag arg)
;;

def_code "\\@insert"
  (fun lexbuf ->
          let tag = save_arg lexbuf in
          let arg = save_arg lexbuf in
          Dest.insert_block tag arg )
;;

def_code "\\@close"
  (fun lexbuf ->
    let tag = save_arg lexbuf in
    if no_display tag then begin
      if tag="DISPLAY" then begin
        top_close_display ();
        display := pop stack_display
      end else if tag = "VDISPLAY" then begin
        close_vdisplay_row () ;
        close_vdisplay () ;
        top_item_display ()
      end else begin
        warning ("direct closing of "^tag);
        top_close_block tag
      end
    end else
      top_close_block tag)
;;

def_code "\\@print"
  (fun lexbuf ->
          let arg = save_arg lexbuf in
          Dest.put arg) ;
def_code "\\@subst"
  (fun lexbuf ->
    let arg = subst_arg subst lexbuf in
    Dest.put arg)
;;

def_code "\\@notags"
  (fun lexbuf ->
          let arg = save_arg lexbuf in
          let arg = get_this main arg in
          let buff = Lexing.from_string arg in
          Dest.put (Save.tagout buff)  )
;;
def_code "\\@anti"
  (fun lexbuf ->
          let arg = save_arg lexbuf in
          let envs = get_style main arg in
          Dest.erase_mods envs)
;;
def_code "\\@style"  
  (fun lexbuf ->
          let arg = save_arg lexbuf in
          Dest.open_mod (Style arg) )
;;
def_code "\\@fontcolor"  
  (fun lexbuf ->
          let arg = save_arg lexbuf in
          Dest.open_mod (Color arg) )
;;
def_code "\\@fontsize"  
  (fun lexbuf ->
          let arg = save_arg lexbuf in
          Dest.open_mod (Font (Get.get_int arg)) )
;;
def_code "\\@nostyle" (fun lexbuf -> Dest.nostyle () )
;;
def_code "\\@clearstyle" (fun lexbuf -> Dest.clearstyle () )
;;
def_code "\\@incsize"
  (fun lexbuf ->
          let arg = save_arg lexbuf in
          inc_size (Get.get_int arg) )
;;
def_code "\\htmlcolor"
  (fun lexbuf ->
          let arg = save_arg lexbuf in
          let arg = get_this_nostyle main arg in
          Dest.open_mod (Color ("\"#"^arg^"\"")) )
;;

def_code "\\usecounter"
  (fun lexbuf ->
          let arg = subst_arg subst lexbuf in
          Counter.set_counter arg 0 ;
          Dest.set_dcount arg )
;;
def_code "\\@fromlib"
  (fun lexbuf ->
          let arg = save_arg lexbuf in
          start_lexstate ();
          Mylib.put_from_lib arg Dest.put;
          restore_lexstate ())
;;
def_code "\\imageflush"
  (fun lexbuf ->
          let arg = save_opt "" lexbuf in
          iput_newpage arg )
;;
def_code "\\textalltt"
  (fun lexbuf ->
          let arg = save_arg lexbuf in
          let old = !alltt in
          scan_this main "\\mbox{" ;
          alltt := true ;
          Dest.open_group "CODE" ;
          scan_this main arg ;
          Dest.close_group () ;
          scan_this main "}" ;
          alltt := old )
;;
def_code "\\@itemdisplay"
  (fun lexbuf -> Dest.force_item_display ())
;;
def_code "\\@br"
  (fun lexbuf -> Dest.skip_line ())
;;


(* TeX conditionals *)
let testif cell lexbuf =
  if !cell then check_alltt_skip lexbuf
  else skip_false lexbuf

let setif cell b lexbuf =
  cell := b ;
  check_alltt_skip lexbuf 
;;

let extract_if name =
  let l = String.length name in
  if l <= 3 || String.sub name 0 3 <> "\\if" then
    raise (Error ("Bad newif: "^name)) ;
  String.sub name 3 (l-3)
;;

let def_and_register name f =
  def_code name f ; macro_register name
;;

let newif lexbuf =
  let arg = subst_arg subst lexbuf in
  try
    let name = extract_if arg in
    let cell = ref false in
    def_and_register ("\\if"^name) (testif cell) ;
    def_and_register ("\\"^name^"true") (setif cell true) ;
    def_and_register ("\\"^name^"false") (setif cell false)
  with Latexmacros.Failed -> ()
;;

def_code "\\newif" newif 
;;

def_code "\\else" (fun lexbuf -> skip_false lexbuf)
;;

def_code "\\fi" (fun lexbuf -> check_alltt_skip lexbuf)
;;


let newif_ref name cell =
  def_code ("\\if"^name) (testif cell) ;
  def_code ("\\"^name^"true") (setif cell true) ;
  def_code ("\\"^name^"false") (setif cell false)
;;

newif_ref "symb" symbols ;
newif_ref "alltt" alltt ;
newif_ref "silent" silent;
newif_ref "math" in_math ;
newif_ref "mmode" in_math ;
newif_ref "display" display ;
newif_ref "french" french ;
newif_ref "html" html;
newif_ref "text" text;
newif_ref "info" text;
newif_ref "optarg" optarg;
newif_ref "styleloaded" styleloaded;
newif_ref "activebrace" activebrace;
def_code ("\\iftrue") (testif (ref true)) ;
def_code ("\\iffalse") (testif (ref false))
;;

(* ifthen package *)
def_code "\\ifthenelse"
    (fun lexbuf ->
      let cond = save_arg lexbuf in
      let arg_true = save_arg lexbuf in
      let arg_false = save_arg lexbuf in
      scan_this main
        (if Get.get_bool cond then arg_true else arg_false))
;;

def_code "\\whiledo"
    (fun lexbuf ->
      let test = save_arg lexbuf in
      let body = save_arg lexbuf in
      let btest = ref (Get.get_bool test) in
      while !btest do
        scan_this main body ;
        btest := Get.get_bool test
      done)
;;

def_fun "\\newboolean" (fun s -> "\\newif\\if"^s)
;;

def_code "\\setboolean"
    (fun lexbuf ->
      let name = subst_arg subst lexbuf in
      let arg = save_arg lexbuf in
      let b = Get.get_bool arg in
      scan_this main ("\\"^name^(if b then "true" else "false")))
;;

(* Color package *)
def_code "\\definecolor"
  (fun lexbuf ->
    let clr = subst_arg subst lexbuf in
    let mdl = subst_arg subst lexbuf in
    let value = subst_arg subst lexbuf in
    Color.define clr mdl value )
;;

def_code "\\color"
  (fun lexbuf ->
    let clr = subst_arg subst lexbuf in
    let htmlval = Color.retrieve clr in
    Dest.open_mod (Color ("\""^htmlval^"\"")) ;
    skip_blanks lexbuf)
;;

(* Bibliographies *)
def_code "\\cite"
  (fun lexbuf ->
    let opt = subst_opt "" subst lexbuf in
    let args = List.map (subst_this subst) (Save.cite_arg lexbuf) in
    Dest.put_char '[' ;
    Dest.open_group "CITE" ;
    let rec do_rec = function
        [] -> ()
      | [x] -> Dest.loc_ref (get_this main (Auxx.bget x)) x
      | x::rest ->
          Dest.loc_ref (get_this main (Auxx.bget x)) x ;
          Dest.put ", " ;
          do_rec rest in
    do_rec args ;
    if opt <> "" then begin
      Dest.put ", " ;
      scan_this main opt ;
    end ;
    Dest.close_group () ;
    Dest.put_char ']' )
;;

def_fun "\\@bibref" Auxx.bget
;;

(* Includes *)
def_code "\\includeonly"
  (fun lexbuf ->
    let arg = Save.cite_arg lexbuf in
    add_includes arg )
;;

(* Foot notes *)
def_code "\\@stepanchor"
  (fun lexbuf ->
    let mark = Get.get_int (save_arg lexbuf) in
    Foot.step_anchor mark) ;
def_code "\\@anchorval"
  (fun lexbuf ->
    let mark = Get.get_int (save_arg lexbuf) in
    Dest.put (string_of_int (Foot.get_anchor mark)))
;;

def_code "\\@footnotetext"
  (fun lexbuf ->
    start_lexstate () ; 
    let mark = Get.get_int (save_arg lexbuf) in
    let text = save_arg lexbuf in
    let text = get_this main ("\\@clearstyle "^text) in
    Foot.register
      mark
      (get_this main ("\\@fnmarknote{"^string_of_int mark^"}"))
      text ;
    restore_lexstate ())
;;

def_code "\\@footnoteflush"
  (fun lexbuf ->
    let sec_here = subst_arg subst lexbuf
    and sec_notes = get_this_nostyle main "\\@footnotelevel" in
    start_lexstate () ;
    Foot.flush (scan_this main) sec_notes sec_here ;
    restore_lexstate ())
;;

(* Opening and closing environments *)

def_code "\\begin"
  (fun lexbuf ->
    let env = subst_arg subst lexbuf in
    if env = "document" && !prelude then begin
      Image.put "\\pagestyle{empty}\n\\begin{document}\n";
      prelude := false ;
      let _ = Dest.forget_par () in () ;
      Dest.set_out out_file
    end ;
    new_env env ;
    let macro = "\\csname "^env^"\\endcsname"  in
      if env <> "document" then
        top_open_block "" "" ;
      let old_envi = save_stack stack_entry in
      push stack_entry env ;
      scan_this_may_cont main lexbuf macro ;
      restore_stack stack_entry old_envi)
;;

def_code "\\end"
  (fun lexbuf ->
    let env = subst_arg subst lexbuf in
    scan_this main ("\\csname end"^env^"\\endcsname") ;
    if env <> "document" then top_close_block "" ;
    close_env env ;
    if env = "document" then raise Misc.EndInput)
;;

let little_more lexbuf =
  to_newline lexbuf ;
  Out.to_string more_buff
;;

def_code "\\endinput" (fun lexbuf ->
  let reste = little_more lexbuf in
  scan_this main reste ;
  raise Misc.EndInput)    
;;

(* Boxes *)

def_code "\\mbox" (fun lexbuf -> mbox_arg lexbuf)
;;

let def_print name s = def_code name (fun _ -> Dest.put s)
and redef_print name s = redef_code name (fun _ -> Dest.put s)
;;

def_code "\\newsavebox"
  (fun lexbuf ->
    let name = save_arg lexbuf in
    begin try def_print name ""
    with Latexmacros.Failed -> () end )
;;

def_code "\\savebox" 
  (fun lexbuf ->
    let name = subst_this subst (save_arg lexbuf) in
    warning "savebox";
    skip_opt lexbuf ;
    skip_opt lexbuf ;
    let body = save_arg lexbuf in
    redef_print name (get_this main body) ;
    macro_register name)
;;

def_code "\\sbox"
  (fun lexbuf ->
    let name = subst_this subst (save_arg lexbuf) in
    let body = save_arg lexbuf in
    redef_print name (get_this main body) ;
    macro_register name)
;;

def_code "\\usebox"
  (fun lexbuf ->
    let arg = save_arg lexbuf in
    scan_this main arg )
;;

def_code "\\lrbox"
  (fun _ ->
    top_close_block "" ;
    let lexbuf = previous_lexbuf () in
    let name = subst_arg subst lexbuf in
    Dest.open_aftergroup
      (fun s ->
        redef_print name s ;
        macro_register name ;
        "") ;
    scan_this main ("\\mbox{"))
;;

def_code "\\endlrbox"
  (fun _ ->
    scan_this main "}" ; Dest.close_group () ;
    top_open_block "" "")
;;


(* chars *)
def_code "\\char"
  (fun lexbuf ->
    let arg = Save.num_arg lexbuf in
    if not !silent && (arg < 32 || (arg > 127 && arg < 161)) then begin
      Location.print_pos () ;
      prerr_endline ("Warning: \\char, check output");
    end ;
    Dest.put (Dest.iso (Char.chr arg)) ;
    if not !alltt then check_alltt_skip lexbuf)
;;

def_code "\\symbol"
  (fun lexbuf ->
    let arg = save_arg lexbuf in
    scan_this main ("\\char"^arg) )
;;

(* labels *)
def_code "\\label"
  (fun lexbuf ->
    let save_last_closed = Dest.get_last_closed () in
    let lab = subst_arg subst lexbuf in
    Dest.loc_name lab "" ;
    Dest.set_last_closed save_last_closed)
;;

def_code "\\ref"
  (fun lexbuf ->
    let lab = subst_arg subst lexbuf in 
    Dest.loc_ref (get_this main (Auxx.rget lab)) lab)
;;

def_code "\\pageref"
 (fun lexbuf ->
    let lab = subst_arg subst lexbuf in
    Dest.loc_ref "X" lab )
;;

(* index *)
def_code "\\@index"
  (fun lexbuf ->
    let save_last_closed = Dest.get_last_closed () in
    let tag = get_this_nostyle main (save_opt "default" lexbuf) in
    let arg = subst_arg subst lexbuf in
    begin try
      Index.treat (check_this main)
        tag arg (get_this_nostyle main "\\@currentlabel")
    with Index.Error s -> raise (Misc.ScanError s)
    end ;
    Dest.set_last_closed save_last_closed)
;;

def_code "\\@printindex"
  (fun lexbuf ->
    start_lexstate () ;
    let tag =  get_this_nostyle main (save_opt "default" lexbuf) in
    new_env "*index*" ;
    scan_this main "\\@forcecommand{\\makelabel}[1]{#1}" ;
    begin try
      Index.print (scan_this main) tag
    with Index.Error s -> raise (Misc.ScanError s)
    end ;
    close_env "*index*" ;
    restore_lexstate ())
;;

def_code "\\@indexname"
  (fun lexbuf ->
    let tag = get_this_nostyle main (save_opt "default" lexbuf) in
    let name = subst_arg subst lexbuf in
    Index.changename tag name)
;;

let new_index lexbuf =
  let tag = get_this_nostyle main (save_arg lexbuf) in
  let suf = subst_arg subst lexbuf in
  let _   = save_arg lexbuf in
  let name = subst_arg subst lexbuf in
  Index.newindex tag suf name
;;

def_code "\\newindex" new_index ;
def_code "\\renewindex" new_index
;;

(* Counters *)
let alpha_of_int i = String.make 1 (Char.chr (i-1+Char.code 'a'))
and upalpha_of_int i = String.make 1 (Char.chr (i-1+Char.code 'A'))
;;

let rec roman_of_int = function
  0 -> ""
| 1 -> "i"
| 2 -> "ii"
| 3 -> "iii"
| 4 -> "iv"
| 9 -> "ix"
| i ->
   if i < 9 then "v"^roman_of_int (i-5)
   else
     let d = i / 10 and u = i mod 10 in
     String.make d 'x'^roman_of_int u
;;

let uproman_of_int i = String.uppercase (roman_of_int i)
;;

let fnsymbol_of_int = function
  0 -> " "
| 1 -> "*"
| 2 -> "#"
| 3 -> "%"
| 4 -> "\167"
| 5 -> "\182"
| 6 -> "||"
| 7 -> "**"
| 8 -> "##"
| 9 -> "%%"
| i -> alpha_of_int (i-9)
;;

let def_printcount name f =
  def_code name
    (fun lexbuf ->
      let cname = get_this_nostyle main (save_arg lexbuf) in
      let cval = Counter.value_counter cname in
      Dest.put (f cval))
;;

def_printcount "\\arabic" string_of_int ;
def_printcount "\\alph"  alpha_of_int ;
def_printcount "\\Alph"  upalpha_of_int ;
def_printcount "\\roman" roman_of_int;
def_printcount "\\Roman" uproman_of_int;
def_printcount "\\fnsymbol" fnsymbol_of_int
;;


def_code "\\newcounter"
  (fun lexbuf ->
          let name = get_this_nostyle main (save_arg lexbuf) in
          let within = save_opt "" lexbuf in
          let within = get_this_nostyle main within in
          Counter.def_counter name within ;
          scan_this main ("\\def\\the"^name^"{\\arabic{"^name^"}}") )
;;

def_code "\\addtocounter"
  (fun lexbuf ->
    let name = get_this_nostyle main (save_arg lexbuf) in
    let arg = save_arg lexbuf in
    Counter.add_counter name (Get.get_int arg))
;;

def_code "\\setcounter"
  (fun lexbuf ->
          let name = get_this_nostyle main (save_arg lexbuf) in
          let arg = save_arg lexbuf in
          Counter.set_counter name (Get.get_int arg) )
;;

def_code "\\stepcounter"
  (fun lexbuf ->
          let name = get_this_nostyle main (save_arg lexbuf) in
          Counter.step_counter name )
;;

def_print "\\@currentlabel" "" ;
def_code "\\refstepcounter"
  (fun lexbuf ->
          let name = get_this_nostyle main (save_arg lexbuf) in
          Counter.step_counter name ;
          redef_print "\\@currentlabel"
            (get_this_clearstyle main ("\\the"^name)) ;
          macro_register "\\@currentlabel")
;;

def_code "\\numberwithin"
  (fun lexbuf ->
          let name = get_this main (save_arg lexbuf) in
          let within = get_this main (save_arg lexbuf) in
          Counter.number_within name within )
;;

(* terminal output *)
def_code "\\typeout"
  (fun lexbuf ->
    let what = subst_arg subst lexbuf in
    prerr_endline what )
;;

def_code "\\warning"
  (fun lexbuf ->
    let what = subst_arg subst lexbuf in
    warning what )
;;

(* spacing *)

let do_space vert lexbuf  = 
  let arg = subst_arg subst lexbuf in
  begin try
    let n = match Length.main (Lexing.from_string arg) with
    | Length.Absolute n -> n
    | _                 -> raise Length.No in
    if vert then
      for i=1 to n do
        Dest.skip_line ()
      done
    else
      for i=1 to n do
        Dest.put_nbsp (); (* "&nbsp;"*)
      done
  with Length.No ->
    warning ((if vert then "\\vspace" else "\\hspace")^
             " with arg ``"^arg^"''")
  end
;;

def_code "\\hspace"  (fun lexbuf -> do_space false lexbuf) ;
def_code "\\vspace"  (fun lexbuf -> do_space true lexbuf)
;;

(* Explicit groups *)
def_code "\\begingroup" (fun _ -> new_env "command-group")
;;

def_code "\\endgroup" (fun _ -> close_env !cur_env)
;;

(* alltt *)

def_code "\\alltt"
  (fun _ ->
    alltt := true ; Dest.close_block "" ; Dest.open_block "PRE" "") ;
def_code "\\endalltt"
  (fun _ ->
    alltt := true ; Dest.close_block "PRE" ; Dest.open_block "" "")
;;

(* Multicolumn *)

def_code "\\multicolumn"
    (fun lexbuf ->
      let n = Get.get_int (save_arg lexbuf) in      
      let format =  Tabular.main (save_arg lexbuf) in
      do_multi n  format main)
;;

def_code "\\hline"
  (fun lexbuf ->
    (* if is_noborder_table !in_table then*)
    do_hline main ;
    skip_blanks_pop lexbuf ;
    let _ = Dest.forget_par () in
    ())
;;

(* inside tabbing *)
let do_tabul lexbuf =
  if is_tabbing !in_table then begin
    do_unskip () ;
    Dest.close_cell ""; Dest.open_cell default_format 1 0
  end ;
  skip_blanks_pop lexbuf
;;

def_code "\\>" do_tabul ;
def_code "\\=" do_tabul
;;

def_code "\\kill"
  (fun lexbuf ->
    if is_tabbing !in_table then begin
      do_unskip () ;
      Dest.close_cell "";
      Dest.erase_row () ;
      Dest.new_row () ;
      Dest.open_cell default_format 1 0
    end ;
    skip_blanks_pop lexbuf)
;;


(* Tabular and arrays *)


let open_tabbing lexbuf =
  top_close_block "" ;
  let lexbuf = Lexstate.previous_lexbuf in
  let lexfun lb =
    Dest.open_table false "CELLSPACING=0 CELLPADDING=0" ;
    Dest.new_row ();
    Dest.open_cell default_format 1 0 in
  push stack_table !in_table ;
  in_table := Tabbing ;
  new_env "tabbing" ;
  lexfun lexbuf
;;

def_code "\\tabbing" open_tabbing
;;

let check_width = function
  | None -> ""
  | Some (Length.Absolute x) ->
      " WIDTH="^string_of_int (x * Length.font)
  | Some (Length.Percent x) ->
      " WIDTH=\""^string_of_int x^"%\""
;;

let open_array env lexbuf =
  top_close_block "" ;

  save_array_state ();
  Tabular.border := false ;
  let len =
    match env with
    | "tabular*" ->
        let arg = save_arg lexbuf in
        begin try  Some (Get.get_length arg)
        with Length.No -> begin
          warning ("``tabular*'' with length argument: "^
                   subst_this subst arg) ;
          None end
        end
    | _ -> None in
      
  skip_opt lexbuf ;
  let format = save_arg lexbuf in
  let format = Tabular.main format in
  cur_format := format ;
  push stack_in_math !in_math ;
  in_table := Table
       {math = (env = "array")  ;
         border = !Tabular.border} ;
  in_math := false ;
  new_env env ;
  if !display then Dest.item_display () ;
  push stack_display !display ;
  display := false ;
  if !Tabular.border then
    Dest.open_table true ("CELLSPACING=0 CELLPADDING=1"^check_width len)
  else
    Dest.open_table false ("CELLSPACING=2 CELLPADDING=0"^check_width len);
  open_row() ;
  open_first_col main ;
  skip_blanks_pop lexbuf
;;

def_code "\\array" (open_array "array") ;
def_code "\\tabular" (open_array "tabular") ;
def_code "\\tabular*" (open_array "tabular*")
;;


let close_tabbing _ =
  Dest.do_close_cell ();
  Dest.close_row ();
  Dest.close_table ();
  in_table := pop stack_table ;
  close_env "tabbing" ;
  top_open_block "" ""
;;

def_code "\\endtabbing" close_tabbing
;;

let close_array env _ =
  do_unskip () ;
  close_last_col main "" ;
  close_last_row () ;
  if env = !cur_env then begin
    Dest.close_table () ;
    restore_array_state () ;
    in_math := pop stack_in_math ;
    display := pop stack_display;
    if !display then Dest.item_display () ;
    close_env env
  end else begin
    error_env env !cur_env ;
  end ;
  top_open_block "" ""
;;

def_code "\\endarray" (close_array "array") ;
def_code "\\endtabular" (close_array "tabular") ;
def_code "\\endtabular*" (close_array "tabular*")
;;


def_code "\\\\"
 (fun lexbuf -> 
   do_unskip () ;
   let _ = parse_args_opt [""] lexbuf in
   if is_table !in_table  then begin
     close_col main "&nbsp;" ; close_row () ;
     open_row () ; open_first_col main
   end else if is_tabbing !in_table then begin
     Dest.close_cell "";
     Dest.close_row () ;
     Dest.new_row () ;
     Dest.open_cell default_format 1 0
   end else begin
     if !display then
       warning "\\\\ in display mode, ignored"
     else
       Dest.skip_line ()
   end ;
   skip_blanks_pop lexbuf ;
   let _ = Dest.forget_par () in ())
;;


(* Other scanners *)

def_code "\\latexonly"
  (fun lexbuf ->
    top_close_block "" ;
    let lexbuf = previous_lexbuf () in
    start_other_scan "latexonly" latexonly lexbuf)
;;

def_code "\\toimage"
  (fun lexbuf ->
    top_close_block "" ;
    let lexbuf = previous_lexbuf () in
    start_image_scan "" image lexbuf)
;;

(* Info  format specific *)

def_code "\\@infomenu"
  (fun lexbuf ->
    let arg = get_this main (save_arg lexbuf) in
    Dest.infomenu arg)
;;

def_code  "\\@infonode"
  (fun lexbuf ->
    let opt = save_opt "" lexbuf in
    let num = get_this main (save_arg lexbuf) in
    let nom = get_this main (save_arg lexbuf) in
    Dest.infonode opt num nom)
;;

def_code "\\@infoNoteFlush"
    (fun lexbuf ->
      let sec_here = subst_arg subst lexbuf
      and theflush = subst_arg subst lexbuf
      and sec_notes = get_this_nostyle main "\\@footnotelevel" in
      if !Foot.some && Section.value sec_here <= Section.value sec_notes then begin
	scan_this main ("\stepcounter{footnotesflush}%\n\@infonode{}{"^theflush^"}{Notes}");
      end)
;;
	

def_code "\\@printHR"
    (fun lexbuf ->
      let arg = save_arg lexbuf in
      let taille = save_arg lexbuf in
      Dest.horizontal_line arg "2" taille)
;;

(* Accents *)
let aigu = function
  "a" -> "á" | "e" -> "é" | "i" | "\\i" | "\\i " -> "í"
| "o" -> "ó" | "u" -> "ú"
| "A" -> "Á" | "E" -> "É" | "I" | "\\I" | "\\I " -> "Í"
| "O" -> "Ó" | "U" -> "Ú"
| "y" -> "ý" | "Y" -> "Ý"
| "" | " " -> "'"
| s   -> s

and grave = function
  "a" -> "à" | "e" -> "è"  | "i" -> "ì"
| "o" -> "ò" | "u" -> "ù"  | "\\i" | "\\i " -> "í"
| "A" -> "À" | "E" -> "È"  | "I" -> "Ì"
| "O" -> "Ò" | "U" -> "Ù"  | "\\I" | "\\I " -> "Ì"
| "" | " " -> "`"
| s -> s
and circonflexe = function
  "a" -> "â" | "e" -> "ê"  | "i" -> "î"
| "o" -> "ô" | "u" -> "û"  | "\\i" | "\\i " -> "î"
| "A" -> "Â" | "E" -> "Ê"  | "I" -> "Î"
| "O" -> "Ô" | "U" -> "Û"  | "\\I" | "\\I " -> "Î"
| "" | " " -> "\\@print{^}"
| s -> s

and trema = function
  "a" -> "ä" | "e" -> "ë"  | "i" -> "ï"
| "o" -> "ö" | "u" -> "ü"  | "\\i" | "\\i " -> "ï"
| "A" -> "Ä" | "E" -> "Ë"  | "I" -> "Ï"
| "O" -> "Ö" | "U" -> "Ü"  | "\\I" | "\\I " -> "Ï"
| "" | " " -> "¨"
| s -> s

and cedille = function
  "c" -> "ç"
| "C" -> "Ç"
| s   -> s

and tilde = function
  "a" -> "ã" | "A" -> "Ã"
| "o" -> "õ" | "O" -> "Õ"
| "n" -> "ñ" | "N" -> "Ñ"
| "" | " " -> "\\@print{~}"
| s   -> s
;;


      
def_fun "\\'"  aigu ;
def_fun "\\`"  grave ;
def_fun "\\^"  circonflexe ;
def_fun "\\\"" trema ;
def_fun "\\c"  cedille ;
def_fun "\\~"  tilde
;;

Get.init
      (fun nostyle s ->
	do_get_this
	  (if nostyle then Dest.nostyle else (fun () -> ()))
	  main s)
      macro_register new_env close_env
;;
      
Tabular.init (subst_this subst);;


end}

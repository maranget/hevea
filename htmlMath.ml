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

let header = "$$" 


open Misc
open Parse_opts
open HtmlCommon
open Latexmacros

(*
A faire en appel a Html :

-*freeze
-*pop_freeze
-*flush_freeze
-open_block
-close_block
-*close_flow_loc
-*close_flow
-pretty_stack
-pretty_cur
-prerr_flags
-do_close_mods
-free
-pop_out
-push_out
-try_close_block
-*get_block
-erase_block
-get_fontsize
-open_mod
-open_group
-close_group
-do_put_char
-new_status
-do_put
-pretty_mods

??
-empty
-blank
-out_stack
-cur_out

A mettre en local :
ncols
ncols_stack
*)





let freeze f =
  push out_stack (Freeze f) ;
  if !verbose > 2 then begin
    prerr_string "freeze: stack=" ;
    pretty_stack !out_stack
  end
;;

let flush_freeze () = match !out_stack with
  Freeze f::rest ->
    let _ = pop "out" out_stack in
      if !verbose > 2 then begin
      prerr_string "flush_freeze" ;
      pretty_stack !out_stack
    end ;
    f () ; true
| _ -> false
;;

let pop_freeze () = match !out_stack with
  Freeze f::rest ->
    let _ = pop "out" out_stack in
    f,true
| _ -> (fun () -> ()),false
;;


let inside_stack = ref []
;;
let saved_inside = ref []
;;
let ncols_stack = ref []
;;
let in_math_stack = ref []
;;




let try_open_display () =
  push ncols_stack flags.ncols ;
  push inside_stack flags.table_inside ;
  push saved_inside false ;
  flags.table_inside <- false ;
  flags.ncols <- 0

and try_close_display () =
  flags.ncols <- pop "ncols, close" ncols_stack ;
  flags.table_inside <- pop "saved_inside, close" saved_inside || flags.table_inside ;
  flags.table_inside <- pop "inside" inside_stack || flags.table_inside
;;


let close_flow_loc s =
  if !verbose > 2 then
    prerr_endline ("close_flow_loc: "^s) ;

  let active  = !cur_out.active
  and pending = !cur_out.pending in
  if close_block_loc check_empty s then begin
    !cur_out.pending <- active @ pending ;
    true
  end else begin
    !cur_out.pending <- active @ pending ;
    false
  end
;;
let close_flow s =
  if !verbose > 2 then
    prerr_flags ("=> close_flow ``"^s^"''");
  let _ = close_flow_loc s in
  if !verbose > 2 then
    prerr_flags ("<= close_flow ``"^s^"''")
;;

let get_block s args =
  if !verbose > 2 then begin
    prerr_flags "=> get_block";
  end ;
  do_close_mods () ;
  let pempty = see_top "empty" empty_stack
  and pblank = see_top "blank" blank_stack
  and pinsert = see_top "insert" insert_stack in
  try_close_block (pblock ()) ;
  flags.empty <- pempty ; flags.blank <- pblank ; flags.insert <- pinsert;
  do_close_block None s ;
  let _,_,pout = pop_out out_stack in  
  let old_out = !cur_out in  
  cur_out := new_status pout.nostyle pout.pending pout.active;
  let mods = !cur_out.active @ !cur_out.pending in
  do_close_mods () ;
  do_open_block None s args ;
  Out.copy old_out.out !cur_out.out ;
  free old_out ;    
  !cur_out.pending <- mods ;
  let r = !cur_out in
  cur_out := pout ;
  if !verbose > 2 then begin
    Out.debug stderr r.out ;
    prerr_endline "";
    prerr_flags "<= get_block"
  end ;
  r

(* put functions *)

let is_digit = function
    '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'0'|'.'|',' -> true
  | _ -> false
;;

let is_number s =
  let r = ref true in
  for i = 0 to String.length s -1 do
    r := !r && is_digit s.[i]
  done;
  !r
;;


let is_op = function
  "+" | "-"|"/"|"*"|"%"|"<"|">"|"="|"("|")"|"{"|"}"|"["|"]"|","|";"|":"|"|"|"&"|"#"|"!"|"~"|"$" -> true
| _ -> false
;;

let put s =
  let s_blank =
    let r = ref true in
    for i = 0 to String.length s - 1 do
      r := !r && is_blank (String.get s i)
    done ;
    !r in
  let save_last_closed = flags.last_closed in
  do_pending () ;
  flags.empty <- false;
  flags.blank <- s_blank && flags.blank ;
  if !Parse_opts.mathml then begin
    if is_number s then do_put ("<mn> "^s^" </mn>\n")
    else if is_op s then do_put ("<mo> "^s^" </mo>\n")
    else do_put s;
    flags.ncols <- flags.ncols +1;
  end else
    do_put s ;
  if s_blank then flags.last_closed <- save_last_closed
;;

let put_char c =
  let save_last_closed = flags.last_closed in
  let c_blank = is_blank c in
  do_pending () ;
  flags.empty <- false;
  flags.blank <- c_blank && flags.blank ;
  if !Parse_opts.mathml then begin
    if is_digit c then do_put ("<mn> "^String.make 1 c^" </mn>\n")
    else if is_op (String.make 1 c) then do_put ("<mo> "^String.make 1 c^" </mo>\n")
    else do_put_char c;
    flags.ncols <- flags.ncols +1;
  end else do_put_char c ;
  if c_blank then flags.last_closed <- save_last_closed
;;

let put_in_math s =
  if flags.in_pre && !pedantic then
    put s
  else begin
    do_pending () ;
    if !Parse_opts.mathml then begin
      do_put "<mi>";
      do_put s;
      do_put "</mi>\n";
      flags.ncols <- flags.ncols +1;
    end else begin
      do_put "<I>";
      do_put s;
      do_put "</I>"
    end;
    flags.empty <- false; flags.blank <- false;
  end
;;

(*----------*)
(* DISPLAYS *)
(*----------*)


let open_maths () =
  push in_math_stack flags.in_math;
  flags.in_math <- true;
  if !Parse_opts.mathml then open_block "math" ""
;;

let close_maths () =
  flags.in_math <- pop "in_math" in_math_stack;
  if !Parse_opts.mathml then close_block "math"
;;


let begin_item_display f is_freeze =
  if !verbose > 2 then begin
    Printf.fprintf stderr "begin_item_display: ncols=%d empty=%s" flags.ncols (sbool flags.empty) ;
    prerr_newline ()
  end ;
  if !Parse_opts.mathml then 
    open_block "mrow" ""
  else
    open_block "TD" "nowrap";
  open_block "" "" ;
  if is_freeze then(* push out_stack (Freeze f) ;*)freeze f;


and end_item_display () =
  let f,is_freeze = pop_freeze () in
  let _ = close_flow_loc "" in
  if !Parse_opts.mathml then begin
    if close_flow_loc "mrow" then
      flags.ncols <- flags.ncols + 1;
  end else begin
    if close_flow_loc "TD" then
      flags.ncols <- flags.ncols + 1;
  end;
  if !verbose > 2 then begin
    Printf.fprintf stderr "end_item_display: ncols=%d stck: " flags.ncols;
    pretty_stack !out_stack
  end;
  flags.vsize,f,is_freeze
;;

let open_display args =
  if !verbose > 2 then begin
    Printf.fprintf stderr "open_display: %s -> " args
  end ;
  try_open_display () ;
  if !Parse_opts.mathml then begin
    open_block "mrow" "";
  end else begin
    open_block "DISPLAY" args ;
    open_block "TD" "NOWRAP" ;
  end;
  open_block "" "" ;
  if !verbose > 2 then begin
    pretty_cur !cur_out ;
    prerr_endline ""
  end     
;;

let close_display () =
  if !verbose > 2 then begin
    prerr_flags "=> close_display"
  end ;
  if not (flush_freeze ()) then begin
    close_flow "" ;
    let n = flags.ncols in
    if (n = 0 && not flags.blank) then begin
      if !verbose > 2 then begin
        prerr_string "No Display n=0" ;
        (Out.debug stderr !cur_out.out);
        prerr_endline "" 
      end;
      let active = !cur_out.active and pending = !cur_out.pending in
      do_close_mods () ;
      if !Parse_opts.mathml then begin
	let ps,_,ppout = pop_out out_stack in
	if ps <> "mrow" then
	  failclose ("close_display: "^ps^" closes mrow");
	try_close_block "mrow";
	let old_out = !cur_out in
	cur_out := ppout ;
	do_close_mods () ;
	Out.copy old_out.out !cur_out.out ;
	flags.empty <- false ; flags.blank <- false ;
	free old_out ;
      end else begin
	let ps,_,pout = pop_out out_stack in
	if ps <> "TD" then
          failclose ("close_display: "^ps^" closes TD") ;
	do_close_mods () ;
	try_close_block "TD" ;
	let ps,_,ppout = pop_out out_stack in
	if ps <> "DISPLAY" then
          failclose ("close_display: "^ps^" closes DISPLAY") ;
	try_close_block "DISPLAY" ;
	let old_out = !cur_out in
	cur_out := ppout ;
	do_close_mods () ;
	Out.copy old_out.out !cur_out.out ;
	flags.empty <- false ; flags.blank <- false ;
	free old_out ; free pout ;     
      end;
      !cur_out.pending <- active @ pending
    end else if (n=1 && flags.blank) then begin
      if !verbose > 2 then begin
        prerr_string "No display n=1";
        (Out.debug stderr !cur_out.out);
        prerr_endline "" ;
      end;
      if not !Parse_opts.mathml then close_flow "FORGET" ;
      let active = !cur_out.active and pending = !cur_out.pending in
      let ps,_,pout = pop_out out_stack in
      if !Parse_opts.mathml then begin
	if ps<> "mrow" then
	  failclose ("close_display: "^ps^" closes mrow");
	try_close_block "mrow";
      end else begin
	if ps <> "DISPLAY" then
          failclose ("close_display: "^ps^" closes DISPLAY") ;
	try_close_block "DISPLAY" ;
      end;
      let old_out = !cur_out in
      cur_out := pout ;
      do_close_mods () ;
      Out.copy_no_tag old_out.out !cur_out.out ;
      flags.empty <- false ; flags.blank <- false ;
      free old_out ;
      !cur_out.pending <- active @ pending
    end else begin
      if !verbose > 2 then begin
        prerr_string ("One Display n="^string_of_int n) ;
        (Out.debug stderr !cur_out.out);
        prerr_endline ""
      end;
      flags.empty <- flags.blank ;
      if !Parse_opts.mathml then
	close_flow "mrow"
      else begin
	close_flow "TD" ;
	close_flow "DISPLAY"
      end;
    end ;
    try_close_display ()
  end ;
  if !verbose > 2 then
    prerr_flags ("<= close_display")
;;

  
let do_item_display force =
  if !verbose > 2 then begin
    prerr_endline ("Item Display ncols="^string_of_int flags.ncols^" table_inside="^sbool flags.table_inside)
  end ;
  if not !Parse_opts.mathml then begin
    let f,is_freeze = pop_freeze () in
    if (force && not flags.empty) || flags.table_inside then begin
      push saved_inside (pop "saved_inside, item" saved_inside || flags.table_inside) ;
      flags.table_inside <- false ;
      let active  = !cur_out.active
      and pending = !cur_out.pending in
      flags.ncols <- flags.ncols + 1 ;
      let save = get_block "TD" "NOWRAP" in
      if !verbose > 2 then begin
	Out.debug stderr !cur_out.out ;
	prerr_endline "To be copied"
      end;
      if close_flow_loc "TD" then flags.ncols <- flags.ncols + 1; 
      if !verbose > 2 then begin
	Out.debug stderr !cur_out.out ;
	prerr_endline "Was copied"
      end;
      Out.copy save.out !cur_out.out ;
      flags.empty <- false ; flags.blank <- false ;
      free save ;
      !cur_out.pending <- active @ pending ;
      !cur_out.active <- [] ;
      if !verbose > 2 then begin
	Out.debug stderr !cur_out.out ;
	prerr_endline ("Some Item")
      end;
    open_block "TD" "nowrap" ;
      open_block "" ""
    end else begin
      if !verbose > 2 then begin
	Out.debug stderr !cur_out.out ;
	prerr_endline "No Item" ;
	prerr_endline ("flags: empty="^sbool flags.empty^" blank="^sbool flags.blank)
      end;
      close_flow "" ;
      if !verbose > 2 then begin
	Out.debug stderr !cur_out.out ;
	prerr_endline "No Item" ;
	prerr_endline ("flags: empty="^sbool flags.empty^" blank="^sbool flags.blank)
      end;
      open_block "" ""
    end ;
    if is_freeze then push out_stack (Freeze f) ;
  end else begin
     if (force && not flags.empty) || flags.table_inside then
       flags.ncols <- flags.ncols + 1 ;
  end;
  if !verbose > 2 then begin
    prerr_string ("out item_display -> ncols="^string_of_int flags.ncols) ;
    pretty_stack !out_stack
  end ;
;;

let item_display () = do_item_display false
and force_item_display () = do_item_display true
;;



let erase_display () =
  erase_block "" ;
  if !Parse_opts.mathml then
    erase_block "mrow"
  else begin
    erase_block "TD" ;
    erase_block "DISPLAY" ;
  end;
  try_close_display ()
;;



(* vertical display *)

let display_arg  verbose =
  if verbose > 0 then
    "BORDER=1 CELLSPACING=0 CELLPADDING=0"
  else
    "CELLSPACING=0 CELLPADDING=0"
;;

let open_vdisplay display =  
  if !verbose > 1 then
    prerr_endline "open_vdisplay";
  if not display then
    raise (Misc.Fatal ("VDISPLAY in non-display mode"));
  open_block "TABLE" (display_arg !verbose)

and close_vdisplay () =
  if !verbose > 1 then
    prerr_endline "close_vdisplay";
  close_block "TABLE"

and open_vdisplay_row s =
  if !verbose > 1 then
    prerr_endline "open_vdisplay_row";
  open_block "TR" "" ;
  open_block "TD" s ;
  open_display (display_arg !verbose)

and close_vdisplay_row () =
  if !verbose > 1 then
    prerr_endline "close_vdisplay_row";
  close_display () ;
  force_block "TD" "&nbsp;" ;
  close_block "TR"
;;



(* Sup/Sub stuff *)

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



let get_script_font () =
  let n = get_fontsize () in
  if n >= 3 then n-1 else n
;;

let open_script_font () =
  open_mod (Font (get_script_font ()))
;;


let complex s = match check_ital s with
  Complex -> true
| _       -> false
;;

let put_sup_sub tag scanner = function
  "" -> ()
| s  ->
    open_group tag ;
    open_script_font () ;
    scanner s;
    close_group ()
;;



let standard_sup_sub scanner what sup sub display =
  if display && (complex sup || complex sub) then begin
    force_item_display () ;
    open_vdisplay display ;
    if sup <> "" then begin
      open_vdisplay_row "NOWRAP" ;
      open_script_font () ;
      scanner sup ;
      close_vdisplay_row ()
    end ;           
    open_vdisplay_row "" ;
    what ();
    close_vdisplay_row () ;
    if sub <> "" then begin
      open_vdisplay_row "NOWRAP" ;
      open_script_font () ;
      scanner sub ;
      close_vdisplay_row ()
    end ;
      close_vdisplay () ;
      force_item_display ()
  end else begin
     what ();
     put_sup_sub "SUB" scanner sub ;
     put_sup_sub "SUP" scanner sup
  end
;;


let limit_sup_sub scanner what sup sub display =
  if !Parse_opts.mathml then begin
    match sub,sup with
    | "","" -> what ()
    | a,b ->
	open_block "munderover" "";
	do_put_char '\n';
	open_display "";
	what ();
	close_display ();
	open_display "";
	scanner a;
	close_display ();
	open_display "";
	scanner b;
	close_display ();
	close_block "munderover";
  end else begin
    if sup = "" && sub = "" then
      what ()
    else begin
      force_item_display () ;
      open_vdisplay display ;
      open_vdisplay_row "ALIGN=center" ;
      open_script_font () ;
      scanner sup ;
      close_vdisplay_row () ;
      open_vdisplay_row "ALIGN=center" ;
      what () ;
      close_vdisplay_row () ;
      open_vdisplay_row "ALIGN=center" ;
      open_script_font () ;
      scanner sub ;
      close_vdisplay_row () ;
      close_vdisplay () ;
      force_item_display ()
    end
  end
;;

let int_sup_sub something vsize scanner what sup sub display =
  if !Parse_opts.mathml then begin
    match sub,sup,something with
    | "","",true -> what ()
    | "","",false -> ()
    | a,b,true -> 
	open_block "msubsup" "";
	what ();
	scanner a;
	scanner b;
	close_block "msubsup";
    | a,b,false ->
	open_block "msubsup" "";
	put "&nbsp";
	scanner a;
	scanner b;
	close_block "msubsup";
  end else begin
    if something then begin
      force_item_display () ;
      what () ;
      force_item_display ()
    end ;
    if sup <> "" || sub <> "" then begin
      open_vdisplay display ;
      open_vdisplay_row "ALIGN=left NOWRAP" ;
      open_script_font () ;
      scanner sup ;
      close_vdisplay_row () ;
      open_vdisplay_row "ALIGN=left" ;
      for i = 2 to vsize do
	skip_line ()
      done ;
      close_vdisplay_row () ;
      open_vdisplay_row "ALIGN=left NOWRAP" ;
      open_script_font () ;
      scanner sub ;
      close_vdisplay_row () ;
      close_vdisplay () ;
      force_item_display ()
    end
  end
;;


let insert_vdisplay open_fun =
  if !verbose > 2 then begin
    prerr_flags "=> insert_vdisplay" ;
  end ;
  try
    let mods = !cur_out.pending @ !cur_out.active in
    let bs,bargs,bout = pop_out out_stack in
    if bs <> "" then
      failclose ("insert_vdisplay: "^bs^" closes ``''");
    let ps,pargs,pout = pop_out out_stack in
    if ps <> "TD" then
      failclose ("insert_vdisplay: "^ps^" close TD");
    let pps,ppargs,ppout = pop_out out_stack  in
    if pps <> "DISPLAY" then
      failclose ("insert_vdisplay: "^pps^" close DISPLAY");
    let new_out = new_status false [] [] in
    push_out out_stack (pps,ppargs,new_out) ;
    push_out out_stack (ps,pargs,pout) ;
    push_out out_stack (bs,bargs,bout) ;    
    close_display () ;
    cur_out := ppout ;
    open_fun () ;
    do_put (Out.to_string new_out.out) ;
    flags.empty <- false ; flags.blank <- false ;
    free new_out ;    
    if !verbose > 2 then begin
      prerr_string "insert_vdisplay -> " ;
      pretty_mods mods ;
      prerr_newline ()
    end ;
    if !verbose > 2 then
      prerr_flags "<= insert_vdisplay" ;
    mods
  with PopFreeze ->
    raise (Error "\\over should be properly parenthesized")
;;

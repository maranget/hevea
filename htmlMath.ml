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

let header = "$Id: htmlMath.ml,v 1.7 1999-09-01 13:53:49 maranget Exp $" 


open Misc
open Parse_opts
open HtmlCommon
open Latexmacros
open Stack



let delay_stack = Stack.create "delay_stack"
;;
(* delaying output .... *)

let delay f =
  if !verbose > 2 then
    prerr_flags "=> delay" ;
  push vsize_stack flags.vsize ;
  flags.vsize <- 0;
  push delay_stack f ;
  open_block "DELAY" "" ;
  if !verbose > 2 then
    prerr_flags "<= delay"
;;

let flush x =
  if !verbose > 2 then
    prerr_flags ("=> flush arg is ``"^string_of_int x^"''");
  try_close_block "DELAY" ;
  let ps,_,pout = pop_out out_stack in
  if ps <> "DELAY" then
    raise (Misc.Fatal ("html: Flush attempt on: "^ps)) ;
  let mods = !cur_out.active @ !cur_out.pending in
  do_close_mods () ;
  let old_out = !cur_out in
  cur_out := pout ;
  let f = pop delay_stack in
  f x ;
  Out.copy old_out.out !cur_out.out ;
  flags.empty <- false ; flags.blank <- false ;
  free old_out ;
  !cur_out.pending <- mods ;
  flags.vsize <- max (pop vsize_stack) flags.vsize ;
  if !verbose > 2 then
    prerr_flags "<= flush"
;;

(* put functions *)

let put  = HtmlCommon.put
and put_char = HtmlCommon.put_char
;;

let put_in_math s =
  if flags.in_pre && !pedantic then
    put s
  else begin
    put "<I>";
    put s;
    put "</I>";
    flags.empty <- false; flags.blank <- false;
  end
;;

(*----------*)
(* DISPLAYS *)
(*----------*)

let open_center () =  open_block "DIV" "ALIGN=center"
and close_center () = close_block "DIV"
;;

let display_arg  verbose =
  if verbose > 0 then
    "BORDER=1 CELLSPACING=0 CELLPADDING=0"
  else
    "CELLSPACING=0 CELLPADDING=0"
;;




let begin_item_display f is_freeze =
  if !verbose > 2 then begin
    Printf.fprintf stderr "begin_item_display: ncols=%d empty=%s" flags.ncols (sbool flags.empty) ;
    prerr_newline ()
  end ;
  open_block "TD" "NOWRAP";
  open_block "" "" ;
  if is_freeze then(* push out_stack (Freeze f) ;*)freeze f;


and end_item_display () =
  let f,is_freeze = pop_freeze () in
  let _ = close_flow_loc "" in
  if close_flow_loc "TD" then
    flags.ncols <- flags.ncols + 1;
  if !verbose > 2 then begin
    Printf.fprintf stderr "end_item_display: ncols=%d stck: " flags.ncols;
    pretty_stack out_stack
  end;
  flags.vsize,f,is_freeze
;;

let open_display args =
  if !verbose > 2 then begin
    Printf.fprintf stderr "open_display: %s -> " args
  end ;
  try_open_display () ;
  open_block "DISPLAY" args ;
  open_block "TD" "NOWRAP" ;
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
      !cur_out.pending <- active @ pending
    end else if (n=1 && flags.blank) then begin
      if !verbose > 2 then begin
        prerr_string "No display n=1";
        (Out.debug stderr !cur_out.out);
        prerr_endline "" ;
      end;
      close_flow "FORGET" ;
      let active = !cur_out.active and pending = !cur_out.pending in
      let ps,_,pout = pop_out out_stack in
      if ps <> "DISPLAY" then
        failclose ("close_display: "^ps^" closes DISPLAY") ;
      try_close_block "DISPLAY" ;
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
      close_flow "TD" ;
      close_flow "DISPLAY"
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
  let f,is_freeze = pop_freeze () in
  if (force && not flags.empty) || flags.table_inside then begin
    push saved_inside (pop saved_inside || flags.table_inside) ;
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
    open_block "TD" "NOWRAP" ;
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
  if !verbose > 2 then begin
    prerr_string ("out item_display -> ncols="^string_of_int flags.ncols) ;
    pretty_stack out_stack
  end ;
;;

let item_display () = do_item_display false
and force_item_display () = do_item_display true
;;



let erase_display () =
  erase_block "" ;
  erase_block "TD" ;
  erase_block "DISPLAY" ;
  try_close_display ()
;;


let open_maths display =
  if display then open_center ();
  push in_math_stack flags.in_math;
  flags.in_math <- true;
  if display then open_display (display_arg !verbose)
  else open_group "";
;;

let close_maths display =
  if display then close_display ()
  else close_group ();
  flags.in_math <- pop in_math_stack;
  if display then close_center ()
;;



(* vertical display *)

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
    if not !pedantic then open_script_font () ;
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
;;

let int_sup_sub something vsize scanner what sup sub display =
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
      failclose ("insert_vdisplay: "^ps^" closes TD");
    let pps,ppargs,ppout = pop_out out_stack  in
    if pps <> "DISPLAY" then
      failclose ("insert_vdisplay: "^pps^" closes DISPLAY");
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



let over display lexbuf =
  if display then begin
    let mods = insert_vdisplay
        (fun () ->
          open_vdisplay display ;
          open_vdisplay_row "NOWRAP ALIGN=center") in
    close_vdisplay_row () ;
    open_vdisplay_row "" ;
    close_mods () ;
    horizontal_line  "NOSHADE" "2" "100";
    close_vdisplay_row () ;
    open_vdisplay_row "NOWRAP ALIGN=center" ;
    close_mods () ;
    open_mods mods ;
    freeze
      (fun () ->
        close_vdisplay_row () ;
        close_vdisplay ();)
  end else begin
    put "/"
  end
;;


(* Gestion of left and right delimiters *)

let put_delim delim i =
  if !verbose > 1 then
    prerr_endline
     ("put_delim: ``"^delim^"'' ("^string_of_int i^")") ;
  if delim <> "." then begin
    begin_item_display (fun () -> ()) false ;
    Symb.put_delim skip_line put delim i ;
    let _ = end_item_display () in ()
  end
;;

let left delim =
  let _,f,is_freeze = end_item_display () in
  delay (fun vsize -> put_delim delim vsize) ;
  begin_item_display f is_freeze
;;

let right delim =
  let vsize,f,is_freeze = end_item_display () in
  put_delim delim vsize;
  flush vsize ;
  begin_item_display f is_freeze ;
  vsize
;;



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

let header = "$Id: mathML.ml,v 1.5 1999-06-22 14:51:48 tessaud Exp $" 


open Misc
open Parse_opts
open HtmlCommon
open Latexmacros


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


(*----------*)
(* DISPLAYS *)
(*----------*)

and begin_item_display f is_freeze =
  if !verbose > 2 then begin
    Printf.fprintf stderr "begin_item_display: ncols=%d empty=%s" flags.ncols (sbool flags.empty) ;
    prerr_newline ()
  end ;
  open_block "mrow" "";
  open_block "" "" ;
  if is_freeze then(* push out_stack (Freeze f) ;*)freeze f;


and end_item_display () =
  let f,is_freeze = pop_freeze () in
  let _ = close_flow_loc "" in
  if close_flow_loc "mrow" then
    flags.ncols <- flags.ncols + 1;
  if !verbose > 2 then begin
    Printf.fprintf stderr "end_item_display: ncols=%d stck: " flags.ncols;
    pretty_stack !out_stack
  end;
  flags.vsize,f,is_freeze


and open_display args =
  if !verbose > 2 then begin
    Printf.fprintf stderr "open_display: %s -> " args
  end ;
  try_open_display () ;
  open_block "mrow" "";
  do_put_char '\n';
  open_block "" "" ;
  if !verbose > 2 then begin
    pretty_cur !cur_out ;
    prerr_endline ""
  end     


and close_display () =
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
      !cur_out.pending <- active @ pending
    end else if (n=1 (*&& flags.blank*)) then begin
      if !verbose > 2 then begin
        prerr_string "No display n=1";
        (Out.debug stderr !cur_out.out);
        prerr_endline "" ;
      end;
      let active = !cur_out.active and pending = !cur_out.pending in
      let ps,_,pout = pop_out out_stack in
      if ps<> "mrow" then
	failclose ("close_display: "^ps^" closes mrow");
      try_close_block "mrow";
      let old_out = !cur_out in
      cur_out := pout ;
      do_close_mods () ;
      if flags.blank then Out.copy_no_tag old_out.out !cur_out.out
      else Out.copy old_out.out !cur_out.out;
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
      close_flow "mrow";
      do_put_char '\n';
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
  if ((*force && *)not flags.empty) || flags.table_inside then
    flags.ncols <- flags.ncols + 1 ;
  let active  = !cur_out.active
  and pending = !cur_out.pending in
  if flags.empty then erase_mods active;
  close_flow "";
  open_block "" "";
  !cur_out.pending <- active @ pending ;
  !cur_out.active <- [] ;
  if is_freeze then freeze f;
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
  erase_block "mrow";
  try_close_display ()
;;

let open_maths display =
  if !verbose > 1 then prerr_endline "=> open_maths";
  push in_math_stack flags.in_math;
  if display then do_put "<BR>\n";
  if not flags.in_math then open_block "math" "align=\"center\""
  else erase_mods [Style "mtext"];
(*  else if pblock()="mtext" then close_block "mtext";*)
  do_put_char '\n';
  flags.in_math <- true;
  open_display "";
  open_display "";
;;

let close_maths display =
  if !verbose >1 then prerr_endline "=> close_maths";
  close_display ();
  close_display ();
  flags.in_math <- pop "in_math" in_math_stack;
  do_put_char '\n';
  if not flags.in_math then begin
(*    if pblock ()="mtext" then close_block "mtext";*)
    close_block "math" end
  else open_mod (Style "mtext");
(*  end else open_block "mtext" "";*)
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
    if ps <> "mrow" then
      failclose ("insert_vdisplay: "^ps^" closes mrow");
    let new_out = new_status false [] [] in
    push_out out_stack (ps,pargs,new_out) ;
    push_out out_stack (bs,bargs,bout) ;    
    close_display () ;
    cur_out := pout ;
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
    raise (Error "wrong parenthesization");
;;


(* delaying output .... *)
(*
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
  let f = pop "delay" delay_stack in
  f x ;
  Out.copy old_out.out !cur_out.out ;
  flags.empty <- false ; flags.blank <- false ;
  free old_out ;
  !cur_out.pending <- mods ;
  flags.vsize <- max (pop "vsive" vsize_stack) flags.vsize ;
  if !verbose > 2 then
    prerr_flags "<= flush"
;;
*)

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

let is_open_delim = function
  | "(" | "[" | "{" | "<" -> true
  | _ -> false
and is_close_delim = function
  | ")" | "]" | "}" | ">" -> true
  | _ -> false
;;

let open_delim () =
  open_display "";
  freeze
    ( fun () ->
      close_display ();
      close_display (););
and is_close () =
  let f, is_freeze = pop_freeze () in
  if is_freeze then begin
    freeze f;
    false
  end else
    true;
and close_delim () =
  let f, is_freeze = pop_freeze () in
  if is_freeze then begin
    close_display ();
  end else begin
    close_display ();
    open_display "";
    warning "Math expression improperly parenthesized";
  end
;;



let put s =
  let s_blank =
    let r = ref true in
    for i = 0 to String.length s - 1 do
      r := !r && is_blank (String.get s i)
    done ;
    !r in
  let s_blanc =
    let r = ref true in
    for i = 0 to String.length s - 1 do
      r := !r &&  ((String.get s i)=' ')
    done ;
    !r in
  if not s_blanc then begin
    let s_op = is_op s
    and s_number = is_number s in
    let save_last_closed = flags.last_closed in
    if is_open_delim s then open_delim ();
    let s_text = if is_close_delim s then is_close () else false in
    if s_op || s_number then force_item_display ();
    do_pending () ;
    flags.empty <- false;
    flags.blank <- s_blank && flags.blank ;
    if s_number then do_put ("<mn> "^s^" </mn>\n")
    else if s_text then do_put ("<mtext>"^s^"</mtext>")
    else if s_op then begin
      do_put ("<mo> "^s^" </mo>\n");
    end else begin
      do_put s
    end;
    if s_blank then flags.last_closed <- save_last_closed;
    if is_close_delim s then close_delim ();
  end
;;

let put_char c =
  let save_last_closed = flags.last_closed in
  let c_blank = is_blank c in
  if c <> ' ' then begin
    let s = String.make 1 c in
    let c_op = is_op s in
    let c_digit = is_digit c in
    if is_open_delim s then open_delim ();
    let c_text = if is_close_delim s then is_close () else false in
    if c_op || c_digit then force_item_display ();
    do_pending () ;
    flags.empty <- false;
    flags.blank <- c_blank && flags.blank ;
    if c_digit then do_put ("<mn> "^s^" </mn>\n")
    else if c_text then do_put ("<mtext>"^s^"</mtext>")
    else if c_op then begin
      do_put ("<mo> "^s^" </mo>\n");
    end else begin
      do_put_char c;
    end;
    if c_blank then flags.last_closed <- save_last_closed;
    if is_close_delim s then close_delim ();
  end
;;

let put_in_math s =
  if flags.in_pre && !pedantic then
    put s
  else begin
    force_item_display ();
    do_pending () ;
    do_put "<mi> ";
    do_put s;
    do_put " </mi>\n";
    flags.empty <- false; flags.blank <- false;
  end
;;



(* Sup/Sub stuff *)


let put_sub_sup  scanner s = 
  open_display "";
  scanner s;
  close_display ();
;;

let insert_sub_sup tag scanner s t =
  let f, is_freeze = pop_freeze () in
  let ps,pargs,pout = pop_out out_stack in
  if ps <> "" then failclose ("sup_sub: "^ps^" closes ``''");
  let new_out = new_status false [] [] in
  push_out out_stack (ps,pargs,new_out);
  close_block "";
  cur_out := pout;
  open_block tag "";
  open_display "";
  let texte = Out.to_string new_out.out in
  do_put (if texte = "" then "<mo> &InvisibleTimes; </mo>" else texte);
  flags.empty <- false; flags.blank <- false;
  free new_out;
  close_display ();
  put_sub_sup scanner s;
  if t<>"" then put_sub_sup scanner t;
  close_block tag;
  open_block "" "";
  if is_freeze then freeze f
;;

let standard_sup_sub scanner what sup sub display =
  match sub,sup with
  | "","" -> what ()
  | a,"" -> 
      open_block "msub" "";
      open_display "";
      what ();
      if flags.empty then begin
	erase_display ();
	erase_block "msub";
	insert_sub_sup "msub" scanner a "";
      end else begin
	close_display ();
	put_sub_sup scanner a;
	close_block "msub";
      end;
  | "",b ->
      open_block "msup" "";
      open_display "";
      what ();
      if flags.empty then begin
	erase_display ();
	erase_block "msup";
	insert_sub_sup "msup" scanner b "";
      end else begin
	close_display ();
	put_sub_sup scanner b;
	close_block "msup";
      end;
  | a,b ->
      open_block "msubsup" "";
      open_display "";
      what ();
      if flags.empty then begin
	erase_display ();
	erase_block "msubsup";
	insert_sub_sup "msubsup" scanner a b;
      end else begin
	close_display ();
	put_sub_sup scanner a;
	put_sub_sup scanner b;
	close_block "msubsup";
      end;
;;


let limit_sup_sub scanner what sup sub display =
  match sub,sup with
  | "","" -> what ()
  | a,"" -> 
      open_block "munder" "";
      open_display "";
      what ();
      if flags.empty then begin
	erase_display ();
	erase_block "munder";
	insert_sub_sup "munder" scanner a "";
      end else begin
	close_display ();
	put_sub_sup scanner a;
	close_block "munder";
      end;
  | "",b ->
      open_block "mover" "";
      open_display "";
      what ();
      if flags.empty then begin
	erase_display ();
	erase_block "mover";
	insert_sub_sup "mover" scanner b "";
      end else begin
	close_display ();
	put_sub_sup scanner b;
	close_block "mover";
      end;
  | a,b ->
      open_block "munderover" "";
      open_display "";
      what ();
      if flags.empty then begin
	erase_display ();
	erase_block "munderover";
	insert_sub_sup "munderover" scanner a b;
      end else begin
	close_display ();
	put_sub_sup scanner a;
	put_sub_sup scanner b;
	close_block "munderover";
      end;
;;

let int_sup_sub something vsize scanner what sup sub display =
  standard_sup_sub scanner what sup sub display
;;


let over display lexbuf =
 if display then begin
    force_item_display ();
    let mods = insert_vdisplay
        (fun () ->
          open_block "mfrac" "";
	  open_display "") in
    close_display () ;
    open_display "" ;
    freeze
      (fun () ->
        close_display () ;
        close_block "mfrac")
  end else begin
    put "/"
  end
;;


let tr = function
  "<" -> "<"
| ">" -> ">"
| "\\{" -> "{"
| "\\}" -> "}"
| s   -> s
;;

let left delim = 
  force_item_display ();
  open_display "";
  if delim <>"." then put ("<mo> "^ tr delim^" </mo>");
  force_item_display ();
  freeze
    ( fun () ->
      force_item_display ();
      close_display ();
      warning "Left delimitor not matched with a right one.";
      force_item_display ();
      close_display ();)
;;

let right delim =
  force_item_display ();
  if delim <> "." then put ("<mo> "^tr delim^" </mo>");
  force_item_display ();
  let f,is_freeze = pop_freeze () in
  if not is_freeze then begin
    warning "Right delimitor alone";
    close_display ();
    open_display "";
  end else begin
    try
      let ps,parg,pout = pop_out out_stack in
      let pps,pparg,ppout = pop_out out_stack in
      if pblock() = "mfrac" then begin
	warning "Right delimitor not matched with a left one.";
	push_out out_stack (pps,pparg,ppout);
	push_out out_stack (ps,parg,pout);
	freeze f;
	close_display ();
	open_display "";
      end else begin
	push_out out_stack (pps,pparg,ppout);
	push_out out_stack (ps,parg,pout);
	close_display ();
      end;
    with PopFreeze -> raise (Error ("Bad placement of right delimitor"));
  end;
  3
;;

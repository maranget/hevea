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

let header = "$Id: mathML.ml,v 1.12 2000-06-02 15:23:37 maranget Exp $" 


open Misc
open Parse_opts
open Element
open HtmlCommon
open Latexmacros
open Stack

(*----------*)
(* DISPLAYS *)
(*----------*)

let begin_item_display f is_freeze =
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
    pretty_stack out_stack
  end;
  flags.vsize,f,is_freeze


and open_display () =
  if !verbose > 2 then begin
    Printf.fprintf stderr "open_display: "
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
      !cur_out.pending <- to_pending pending active
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
      !cur_out.pending <- to_pending pending active
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
  close_flow "";
  open_block "" "";
  !cur_out.pending <- to_pending pending active;
  !cur_out.active <- [] ;
  if is_freeze then freeze f;
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
  erase_block "mrow";
  try_close_display ()
;;

let open_maths display =
  if !verbose > 1 then prerr_endline "=> open_maths";
  push stacks.s_in_math flags.in_math;
  if display then do_put "<BR>\n";
  if not flags.in_math then open_block "math" "align=\"center\""
  else erase_mods [Style "mtext"];
  do_put_char '\n';
  flags.in_math <- true;
  open_display ();
  open_display ();
;;

let close_maths display =
  if !verbose >1 then prerr_endline "=> close_maths";
  close_display ();
  close_display ();
  flags.in_math <- pop stacks.s_in_math ;
  do_put_char '\n';
  if not flags.in_math then begin
    close_block "math" end
  else open_mod (Style "mtext");
;;




let insert_vdisplay open_fun =
  if !verbose > 2 then begin
    prerr_flags "=> insert_vdisplay" ;
  end ;
  try
    let mods = to_pending !cur_out.pending !cur_out.active in
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
    raise (UserError "wrong parenthesization");
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
  open_display ();
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
    open_display ();
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
    if s_number then begin
      do_put ("<mn> "^s^" </mn>\n")
    end else if s_text then begin
      do_put ("<mtext>"^s^"</mtext>")
    end else if s_op then begin
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
    if c_digit then begin
      do_put ("<mn> "^s^" </mn>\n")
    end else if c_text then begin
      do_put ("<mtext>"^s^"</mtext>")
    end else if c_op then begin
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


let put_sub_sup  s = 
  open_display ();
  put s;
  item_display ();  
  close_display ();
;;

let insert_sub_sup tag s t =
  let f, is_freeze = pop_freeze () in
  let ps,pargs,pout = pop_out out_stack in
  if ps <> "" then failclose ("sup_sub: "^ps^" closes ``''");
  let new_out = new_status false [] [] in
  push_out out_stack (ps,pargs,new_out);
  close_block "";
  cur_out := pout;
  open_block tag "";
  open_display ();
  let texte = Out.to_string new_out.out in
  do_put (if texte = "" then "<mo> &InvisibleTimes; </mo>" else texte);
  flags.empty <- false; flags.blank <- false;
  free new_out;
  close_display ();
  put_sub_sup s;
  if t<>"" then put_sub_sup t;
  close_block tag;
  open_block "" "";
  if is_freeze then freeze f
;;

let get_sup_sub
    (scanner : Lexstate.arg -> unit)
    (s : Lexstate.arg) =
  to_string (fun () -> scanner s)

let standard_sup_sub scanner what sup sub display =
  let sup = get_sup_sub scanner sup
  and sub = get_sup_sub  scanner sub in
  match sub,sup with
  | "","" -> what ()
  | a,"" -> 
      open_block "msub" "";
      open_display ();
      what ();
      if flags.empty then begin
	erase_display ();
	erase_block "msub";
	insert_sub_sup "msub" a "";
      end else begin
	close_display ();
	put_sub_sup a;
	close_block "msub";
      end;
  | "",b ->
      open_block "msup" "";
      open_display ();
      what ();
      if flags.empty then begin
	erase_display ();
	erase_block "msup";
	insert_sub_sup "msup" b "";
      end else begin
	close_display ();
	put_sub_sup b;
	close_block "msup";
      end;
  | a,b ->
      open_block "msubsup" "";
      open_display ();
      what ();
      if flags.empty then begin
	erase_display ();
	erase_block "msubsup";
	insert_sub_sup "msubsup" a b;
      end else begin
	close_display ();
	put_sub_sup a;
	put_sub_sup b;
	close_block "msubsup";
      end;
;;



let limit_sup_sub scanner what sup sub display =
  let sup = get_sup_sub scanner sup
  and sub = get_sup_sub  scanner sub in
  match sub,sup with
  | "","" -> what ()
  | a,"" -> 
      open_block "munder" "";
      open_display ();
      what ();
      if flags.empty then begin
	erase_display ();
	erase_block "munder";
	insert_sub_sup "munder" a "";
      end else begin
	close_display ();
	put_sub_sup a;
	close_block "munder";
      end;
  | "",b ->
      open_block "mover" "";
      open_display ();
      what ();
      if flags.empty then begin
	erase_display ();
	erase_block "mover";
	insert_sub_sup "mover" b "";
      end else begin
	close_display ();
	put_sub_sup b;
	close_block "mover";
      end;
  | a,b ->
      open_block "munderover" "";
      open_display ();
      what ();
      if flags.empty then begin
	erase_display ();
	erase_block "munderover";
	insert_sub_sup "munderover" a b;
      end else begin
	close_display ();
	put_sub_sup a;
	put_sub_sup b;
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
	  open_display ()) in
    force_item_display ();
    flags.ncols <- flags.ncols +1;
    close_display () ;
    open_display () ;
    freeze
      (fun () ->
	force_item_display ();
	flags.ncols <- flags.ncols +1;
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

let left delim k = 
  force_item_display ();
  open_display ();
  if delim <>"." then put ("<mo> "^ tr delim^" </mo>");
  k 0 ;
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
    open_display ();
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
	open_display ();
      end else begin
	push_out out_stack (pps,pparg,ppout);
	push_out out_stack (ps,parg,pout);
	close_display ();
      end;
    with PopFreeze -> raise (UserError ("Bad placement of right delimitor"));
  end;
  3
;;

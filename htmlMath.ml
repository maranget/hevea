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

let header = "$Id: htmlMath.ml,v 1.49 2012-06-05 14:55:39 maranget Exp $"


open Misc
open Parse_opts
open HtmlCommon
open MyStack



let delay_stack = MyStack.create "delay_stack"
(* delaying output .... *)

let delay f =
  if !verbose > 2 then prerr_flags "=> delay" ;
  push stacks.s_vsize flags.vsize ;
  flags.vsize <- 0;
  push delay_stack f ;
  open_block DELAY "" ;
  if !verbose > 2 then prerr_flags "<= delay"

let flush x =
  if !verbose > 2 then
    prerr_flags ("=> flush arg is ``"^string_of_int x^"''");
  try_close_block DELAY ;
  let ps,_,pout = pop_out out_stack in
  if ps <> DELAY then
    raise (Misc.Fatal ("html: Flush attempt on: "^string_of_block ps)) ;
  let mods = as_envs !cur_out.active !cur_out.pending in
  do_close_mods () ;
  let old_out = !cur_out in
  cur_out := pout ;
  let f = pop delay_stack in
  f x ;
  Out.copy old_out.out !cur_out.out ;
  flags.empty <- false ; flags.blank <- false ;
  !cur_out.pending <- mods ;
  flags.vsize <- max (pop stacks.s_vsize) flags.vsize ;
  if !verbose > 2 then
    prerr_flags "<= flush"

(* put functions *)

let put  = HtmlCommon.put
and put_char = HtmlCommon.put_char

let put_in_math s =
  if flags.in_pre && !pedantic then
    put s
  else begin
    put "<i>";
    put s;
    put "</i>";
    flags.empty <- false; flags.blank <- false;
  end

(*----------*)
(* DISPLAYS *)
(*----------*)

let display_cell_arg tdarg =
  let arg =
    if !displayverb then
      "class=\"vdcell\""
    else
      "class=\"dcell\"" in
  match tdarg with
    | "" -> arg
    | _  -> arg ^ " " ^ tdarg

let open_display_cell tdarg = open_block TD (display_cell_arg tdarg)

let begin_item_display f is_freeze =
  if !verbose > 2 then begin
    Printf.fprintf stderr "begin_item_display: ncols=%d empty=%s" flags.ncols (sbool flags.empty) ;
    prerr_newline ()
  end ;
  open_display_cell "" ;
  open_block DFLOW "" ;
  if is_freeze then freeze f


and end_item_display () =
  let f,is_freeze = pop_freeze () in
  let _ = close_flow_loc check_empty DFLOW in
  if close_flow_loc check_empty TD then
    flags.ncols <- flags.ncols + 1;
  if !verbose > 2 then begin
    Printf.fprintf stderr "end_item_display: ncols=%d stck: " flags.ncols;
    pretty_stack out_stack
  end;
  flags.vsize,f,is_freeze

(********************************************************
                                                         *                                                      *
                                                         *    To open display with vertical alignment arguments  *
                                                         *                                                       *
*********************************************************)

let open_display_varg centering varg =
  if !verbose > 2 then begin
    Printf.fprintf stderr "open_display: "
  end ;
  try_open_display () ;
  open_block (DISPLAY centering) varg ;
  open_display_cell "" ;
  open_block DFLOW "" ;
  if !verbose > 2 then begin
    pretty_cur !cur_out ;
    prerr_endline ""
  end


(*
  let open_display_varg_harg centering varg harg =
  if !verbose > 2 then begin
  Printf.fprintf stderr "open_display: "
  end ;
  try_open_display () ;
  open_block (DISPLAY centering) (varg^harg);
  open_display_cell "" ;
  open_block DFLOW "" ;
  if !verbose > 2 then begin
  pretty_cur !cur_out ;
  prerr_endline ""
  end
*)

let open_display centering = open_display_varg centering "style=\"vertical-align:middle\""

(* argument force forces the display structure,
   when false, the TABLE/TR/TD may be spared in two situation
   1. No display cell at all (n=0)
   2. One display cell, one empty cell *)
let close_display force =
  if !verbose > 2 then begin
    prerr_flags "=> close_display " ; pretty_stack out_stack ;
    Out.debug stderr !cur_out.out
  end ;
  if not (flush_freeze ()) then begin
    close_flow DFLOW ;
    if !verbose > 3 then begin
      Printf.eprintf "Just closed DFLOW " ;  pretty_stack out_stack ;
      Out.debug stderr !cur_out.out
    end ;
    let n = flags.ncols in
    if !verbose > 2 then
      Printf.fprintf stderr "=> close_display, ncols=%d\n" n ;
    if (n = 0 && not flags.blank && not force) then begin
      if !verbose > 2 then begin
        prerr_string "No Display n=0" ;
        (Out.debug stderr !cur_out.out);
        prerr_endline ""
      end;
      let active = !cur_out.active and pending = !cur_out.pending in
      do_close_mods () ;
      let ps,_,_pout = pop_out out_stack in
      if ps <> TD then
        failclose "close_display" ps TD ;
      do_close_mods () ;
      try_close_block TD ;
      let ps,_,ppout = pop_out out_stack in
      begin match ps with
        | DISPLAY _ -> ()
        | _ ->
            failclose "close_display" ps (DISPLAY false)
      end;
      try_close_block ps ;
      let old_out = !cur_out in
      cur_out := ppout ;
      do_close_mods () ;
      Out.copy old_out.out !cur_out.out ;
      flags.empty <- false ; flags.blank <- false ;
      !cur_out.pending <- as_envs active pending
    end else if (n=1 && flags.blank && not force) then begin
      if !verbose > 2 then begin
        prerr_string "No display n=1";
        (Out.debug stderr !cur_out.out);
        prerr_endline "" ;
      end;
      close_flow FORGET ;
      let active = !cur_out.active and pending = !cur_out.pending in
      let ps,_,pout = pop_out out_stack in
      begin match ps with
        | DISPLAY _ -> ()
        | _ ->
            failclose "close_display" ps (DISPLAY false)
      end ;
      try_close_block ps ;
      let old_out = !cur_out in
      cur_out := pout ;
      do_close_mods () ;
      Out.copy_no_tag old_out.out !cur_out.out ;
      flags.empty <- false ; flags.blank <- false ;
      !cur_out.pending <- as_envs active pending
    end else begin
      if !verbose > 2 then begin
        prerr_string ("One Display n="^string_of_int n) ;
        (Out.debug stderr !cur_out.out);
        prerr_endline ""
      end;
      flags.empty <- flags.blank ;
      close_flow TD ;
      close_flow (DISPLAY false)
    end ;
    try_close_display ()
  end ;
  if !verbose > 2 then
    prerr_flags ("<= close_display")


let do_item_display force =
  if !verbose > 2 then begin
    prerr_endline ("Item Display ncols="^string_of_int flags.ncols^" table_inside="^sbool flags.table_inside^", force="^sbool force) ;
    pretty_stack out_stack
  end ;
  if (force && not flags.empty) || flags.table_inside then begin
    let f,is_freeze = pop_freeze () in
    push stacks.s_saved_inside
      (pop stacks.s_saved_inside || flags.table_inside) ;
    flags.table_inside <- false ;
    let active  = !cur_out.active
    and pending = !cur_out.pending in
    flags.ncols <- flags.ncols + 1 ;
    close_flow DFLOW ;
    close_flow TD ;
    if !verbose > 2 then begin
      prerr_endline "Added Item to Display" ;
      Out.debug stderr !cur_out.out ;
    end;
    open_display_cell "" ;
    open_block DFLOW "" ;
    !cur_out.pending <- as_envs active pending ;
    !cur_out.active <- [] ;
    if is_freeze then push out_stack (Freeze f)
  end else begin
    if !verbose > 2 then begin
      Out.debug stderr !cur_out.out ;
      prerr_endline "No Item" ;
      prerr_endline ("flags: empty="^sbool flags.empty^" blank="^sbool flags.blank)
    end
  end

let item_display () = do_item_display false
and force_item_display () = do_item_display true


let erase_display () =
  erase_block DFLOW ;
  erase_block TD ;
  erase_block (DISPLAY false);
  try_close_display ()


let open_maths display =
  push stacks.s_in_math flags.in_math;
  flags.in_math <- true;
  open_group "";
  if display then open_display true

let close_maths display =
  (* force a table in that case, because we want to apply style class *)
  if display then close_display true ;
  close_group () ;
  flags.in_math <- pop stacks.s_in_math

(* vertical display *)

let open_vdisplay center display =
  if !verbose > 1 then
    prerr_endline "open_vdisplay";
  if not display then  raise (Misc.Fatal ("VDISPLAY in non-display mode"));
  open_block TABLE (display_arg center !verbose)

and close_vdisplay () =
  if !verbose > 1 then
    prerr_endline "close_vdisplay";
  close_block TABLE

and open_vdisplay_row trarg tdarg  =
  if !verbose > 1 then
    prerr_endline "open_vdisplay_row";
  open_block TR trarg ;
  open_display_cell tdarg ;
  open_display false

and close_vdisplay_row () =
  if !verbose > 1 then
    prerr_endline "close_vdisplay_row";
  close_display false ;
  force_block TD "&nbsp;"  ;
  close_block TR



(* Sup/Sub stuff *)

let put_sup_sub display scanner (arg : string Lexstate.arg) =
  if display then open_display false else open_block INTERN "" ;
  scanner arg ;
  if display then close_display false else close_block INTERN

let reput_sup_sub tag = function
  | "" -> ()
  | s  ->
      open_block INTERN "" ;
      clearstyle () ;
      if not  (flags.in_pre && !pedantic) then begin
        put_char '<' ;
        put tag ;
        put_char '>'
      end ;
      put s ;
      if not  (flags.in_pre && !pedantic) then begin
        put "</" ;
        put tag ;
        put_char '>'
      end ;
      close_block INTERN


let standard_sup_sub scanner what sup sub display =
  let sup,fsup =
    hidden_to_string (fun () -> put_sup_sub display scanner sup)
  in
  let sub,fsub =
    hidden_to_string (fun () -> put_sup_sub display scanner sub) in

  if display && (fsub.table_inside || fsup.table_inside) then begin
    force_item_display () ;
    open_vdisplay false display ;
    if sup <> "" then begin
      open_vdisplay_row "" "" ;
      clearstyle () ;
      put sup ;
      close_vdisplay_row ()
    end ;
    open_vdisplay_row "" "" ;
    what ();
    close_vdisplay_row () ;
    if sub <> "" then begin
      open_vdisplay_row "" "" ;
      clearstyle () ;
      put sub ;
      close_vdisplay_row ()
    end ;
    close_vdisplay () ;
    force_item_display ()
  end else begin
    what ();
    reput_sup_sub "sub" sub ;
    reput_sup_sub "sup" sup
  end


let limit_sup_sub scanner what sup sub display =
  let sup = to_string (fun () -> put_sup_sub display scanner sup)
  and sub = to_string (fun () -> put_sup_sub display scanner sub) in
  if sup = "" && sub = "" then
    what ()
  else begin
    force_item_display () ;
    open_vdisplay false display ;
    open_vdisplay_row "" "style=\"text-align:center\"" ;
    put sup ;
    close_vdisplay_row () ;
    open_vdisplay_row "" "style=\"text-align:center\"" ;
    what () ;
    close_vdisplay_row () ;
    open_vdisplay_row "" "style=\"text-align:center\"" ;
    put sub ;
    close_vdisplay_row () ;
    close_vdisplay () ;
    force_item_display ()
  end

let int_sup_sub something vsize scanner what sup sub display =
  let sup = to_string (fun () -> put_sup_sub display scanner sup)
  and sub = to_string (fun () -> put_sup_sub display scanner sub) in
  if something then begin
    force_item_display () ;
    what () ;
    force_item_display ()
  end ;
  if sup <> "" || sub <> "" then begin
    open_vdisplay false display ;
    open_vdisplay_row "" "style=\"text-align:left\"" ;
    put sup ;
    close_vdisplay_row () ;
    open_vdisplay_row "" "style=\"text-align:left\"" ;
    for _i = 2 to vsize do
      skip_line ()
    done ;
    close_vdisplay_row () ;
    open_vdisplay_row "" "style=\"text-align:left\"" ;
    put sub ;
    close_vdisplay_row () ;
    close_vdisplay () ;
    force_item_display ()
  end


let insert_vdisplay open_fun =
  if !verbose > 2 then begin
    prerr_flags "=> insert_vdisplay" ;
  end ;
  try
    let mods = to_pending !cur_out.pending !cur_out.active in
    let bs,bargs,bout = pop_out out_stack in
    if bs <> DFLOW then
      failclose "insert_vdisplay" bs DFLOW ;
    let ps,pargs,pout = pop_out out_stack in
    if ps <> TD then
      failclose "insert_vdisplay" ps TD ;
    let pps,ppargs,ppout = pop_out out_stack  in
    let center =
      match pps with
        | DISPLAY b -> b
        | _ -> failclose "insert_vdisplay" pps (DISPLAY false) in
    let new_out = create_status_from_scratch false [] in
    push_out out_stack (DISPLAY false,ppargs,new_out) ;
    push_out out_stack (ps,pargs,pout) ;
    push_out out_stack (bs,bargs,bout) ;
    close_display false ;
    cur_out := ppout ;
    let () = open_fun center in (* force bool -> unit' type  *)
    do_put (Out.to_string new_out.out) ;
    flags.empty <- false ; flags.blank <- false ;
    if !verbose > 2 then begin
      prerr_string "insert_vdisplay -> " ;
      pretty_mods stderr mods ;
      prerr_newline ()
    end ;
    if !verbose > 2 then
      prerr_flags "<= insert_vdisplay" ;
    mods
  with PopFreeze ->
    raise (UserError "\\over should be properly parenthesized")


let line_in_vdisplay_row () =
  open_block TR "" ;
  open_block TD "class=\"hrule\"" ;
  (*
    close_mods () ;
    line_in_table () ;
  *)
  force_block TD "" ;
  force_block TR ""

let over _lexbuf =
  let mods = insert_vdisplay
    (fun center ->
      open_vdisplay center true ;
      open_vdisplay_row "" "style=\"text-align:center\"") in
  close_vdisplay_row () ;
  line_in_vdisplay_row () ;
  open_vdisplay_row "" "style=\"text-align:center\"" ;
  close_mods () ;
  open_mods mods ;
  freeze
    (fun () ->
      close_vdisplay_row () ;
      close_vdisplay ())

(* Gestion of left and right delimiters *)

let left _ k_delim k =
  let _,f,is_freeze = end_item_display () in
  delay
    (fun vsize ->
      begin_item_display (fun () -> ()) false ;
      k_delim vsize ;
      ignore (end_item_display ()) ;
      begin_item_display (fun () -> ()) false ;
      k vsize ;
      let _ = end_item_display () in
      ()) ;
  begin_item_display f is_freeze

let right _ k_delim =
  let vsize,f,is_freeze = end_item_display () in
  begin_item_display (fun () -> ()) false ;
  k_delim vsize;
  ignore (end_item_display ()) ;
  flush vsize ;
  begin_item_display f is_freeze ;
  vsize

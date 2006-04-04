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

let header = "$Id: html.ml,v 1.98 2006-04-04 08:45:11 maranget Exp $" 

(* Output function for a strange html model :
     - Text elements can occur anywhere and are given as in latex
     - A new grouping construct is given (open_group () ; close_group ())
*)

open Misc
open Parse_opts
open Latexmacros
open HtmlCommon

exception Error of string

type block = HtmlCommon.block

let addvsize x = flags.vsize <- flags.vsize + x 

(* Calls to other modules that are in the interface *)

let 
  over,
  erase_display,
  begin_item_display,
  end_item_display,
  force_item_display,
  item_display,
  close_display,
  open_display_varg,
  open_display,
  close_maths,
  open_maths, 
  put_in_math,
  math_put,
  math_put_char,
  left,
  right
    =
  if !Parse_opts.mathml then begin
    MathML.over,
    MathML.erase_display,
    MathML.begin_item_display,
    MathML.end_item_display,
    MathML.force_item_display,
    MathML.item_display,
    MathML.close_display,
    MathML.open_display_varg,
    MathML.open_display,
    MathML.close_maths,
    MathML.open_maths,
    MathML.put_in_math,
    MathML.put,
    MathML.put_char,
    MathML.left,
    MathML.right
  end else begin
    HtmlMath.over,
    HtmlMath.erase_display,
    HtmlMath.begin_item_display,
    HtmlMath.end_item_display,
    HtmlMath.force_item_display,
    HtmlMath.item_display,
    HtmlMath.close_display,
    HtmlMath.open_display_varg,
    HtmlMath.open_display,
    HtmlMath.close_maths,
    HtmlMath.open_maths,
    HtmlMath.put_in_math,
    HtmlMath.put,
    HtmlMath.put_char,
    HtmlMath.left,
    HtmlMath.right
  end
;;

let
  int_sup_sub,
  limit_sup_sub,
  standard_sup_sub
    =
  if !Parse_opts.mathml then
    MathML.int_sup_sub,
    MathML.limit_sup_sub,
    MathML.standard_sup_sub
  else
    HtmlMath.int_sup_sub,
    HtmlMath.limit_sup_sub,
    HtmlMath.standard_sup_sub
;;



let set_out out =  !cur_out.out <- out
and stop () =
  Stack.push stacks.s_active !cur_out.out ;
  Stack.push stacks.s_pending_par flags.pending_par ;
  !cur_out.out <- Out.create_null () ;
  flags.pending_par <- None

and restart () =
  !cur_out.out <- Stack.pop stacks.s_active ;
  flags.pending_par <- Stack.pop stacks.s_pending_par
;;


(* acces to flags *)
let is_empty () = flags.empty
and get_last_closed () = flags.last_closed
and set_last_closed s = flags.last_closed <- s
;;

    

let debug m =
  Printf.fprintf stderr
    "%s : table_vsize=%d vsize=%d" m flags.table_vsize flags.vsize ;
  prerr_newline ()
;;

let debug_empty f =
  prerr_string (if f.empty then "empty=true" else "empty=false")
;;

let put s = 
  if flags.in_math then math_put s
  else HtmlCommon.put s
;;

let put_char c =
  if flags.in_math then math_put_char c
  else HtmlCommon.put_char c
;;

let put_unicode i = match i with
| 0x3C -> put "&lt;"
| 0x3E -> put "&gt;"
| 0x26 -> put "&amp;"
| _ ->
  try put_char (OutUnicode.translate_out i)
  with OutUnicode.CannotTranslate ->
    put (Printf.sprintf "&#X%X;" i)
;;

let set_dt s = flags.dt <- s
and set_dcount s = flags.dcount <- s
;;

(*********************************************
*  Allows things like <LI CLASS=li-itemize>  *
*********************************************)

let item s =
  if !verbose > 2 then begin
    prerr_string "item: stack=" ;
    pretty_stack out_stack
  end ;
  let mods = all_to_pending !cur_out in
  clearstyle () ;
  !cur_out.pending <- mods ;
  let saved =
    if flags.nitems = 0 then begin
      let _ = forget_par () in () ;
      Out.to_string !cur_out.out
    end else  "" in
  flags.nitems <- flags.nitems+1;
  try_flush_par Now ;
  do_put ("<LI "^s^">") ;
  do_put saved

let nitem = item

let ditem scan arg s1 s2 =
  if !verbose > 2 then begin
    Printf.eprintf "DITEM: «%s» «%s» «%s»\n" arg s1 s2 ;
    prerr_string "ditem: stack=" ;
    pretty_stack out_stack
  end ;
  let mods = all_to_pending !cur_out in
  clearstyle () ;
  !cur_out.pending <- mods ;
  let true_scan =
    if flags.nitems = 0 then begin
      let _ = forget_par () in () ;
      let saved = Out.to_string !cur_out.out in
      (fun arg -> do_put saved ; scan arg)
    end else scan in
  try_flush_par Now ;
  do_put ("<DT "^s1^">") ;
  !cur_out.pending <- mods ;
  flags.nitems <- flags.nitems+1;
  open_block INTERN "" ;
  if flags.dcount <> "" then scan ("\\refstepcounter{"^ flags.dcount^"}") ;
  true_scan ("\\makelabel{"^arg^"}") ;
  close_block INTERN ;
  do_put ("<DD "^s2^">")
;;

let loc_name _ = ()

  
(* freeze everyting and change output file *)

let open_chan chan =
  open_group "" ;
  free !cur_out ;
  !cur_out.out <- Out.create_chan chan ;
;;

let close_chan () =
  Out.close !cur_out.out ;
  !cur_out.out <- Out.create_buff () ;
  close_group ()
;;


let to_style f =
  let old_flags = copy_flags flags in
  let _ = forget_par () in
  open_block INTERN "" ;
  clearstyle () ;
  f () ;
  let r = to_pending !cur_out.pending !cur_out.active in
  erase_block INTERN ;
  set_flags flags old_flags ;
  r
;;

let get_current_output () = Out.to_string !cur_out.out


let finalize check =
  if check then begin
    check_stacks ()
  end else begin
    (* Flush output in case of fatal error *)
    let rec close_rec () =
      if not (Stack.empty out_stack) then begin
        match Stack.pop out_stack with
        | Freeze _ -> close_rec ()
        | Normal (_,_,pout) ->
            Out.copy !cur_out.out pout.out ;
            cur_out := pout ;
            close_rec ()
      end in
    close_rec ()
  end ;
  Out.close !cur_out.out ;
  !cur_out.out <- Out.create_null ()
;;


let put_separator () = put "\n"
;;

let unskip () = 
  Out.unskip !cur_out.out;
  if flags.blank then
    flags.empty <- true;
;;
  
let put_tag tag = put tag
;;

let put_nbsp () =
  if !Lexstate.whitepre || (flags.in_math && !Parse_opts.mathml) then
    put_char ' '
  else
    put_unicode 0xA0
;;

let put_open_group () =
  put_char '{'
;;

let put_close_group () =
  put_char '}'
;;



let open_table border htmlargs =
  let _,arg_b, arg =
    if flags.in_math && !Parse_opts.mathml then
      "mtable","frame = \"solid\"",""
    else "TABLE","BORDER=1",htmlargs
  in
  if border then open_block TABLE (arg_b^" "^arg)
  else open_block TABLE arg
;;

let new_row () =
  if flags.in_math && !Parse_opts.mathml then
    open_block (OTHER "mtr") ""
  else open_block TR ""
;;


let attribut name = function
  | "" -> ""
  | s  -> " "^name^"="^s
and as_colspan = function
  |  1  -> ""
  |  n -> " COLSPAN="^string_of_int n
and as_colspan_mathml = function
  |  1  -> ""
  |  n -> " columnspan= \""^string_of_int n^"\""

let as_align f span = match f with
  Tabular.Align
    {Tabular.vert=v ; Tabular.hor=h ;
     Tabular.wrap=w ; Tabular.width=_} ->
    attribut "VALIGN" v^
    attribut "ALIGN" h^
    (if w then "" else " NOWRAP")^
    as_colspan span
| _       ->  raise (Misc.Fatal ("as_align"))
;;

let as_align_mathml f span = match f with
  Tabular.Align
    {Tabular.vert=v ; Tabular.hor=h } ->
    attribut "rowalign" ("\""^v^"\"")^
    attribut "columnalign" ("\""^h^"\"")^
    as_colspan_mathml span
| _       ->  raise (Misc.Fatal ("as_align_mathml"))
;;

let open_direct_cell attrs span =
  if flags.in_math && !Parse_opts.mathml then begin
    open_block (OTHER "mtd") (attrs^as_colspan_mathml span);
    open_display ()
  end else open_block TD (attrs^as_colspan span)

let open_cell format span _= 
  if flags.in_math && !Parse_opts.mathml then begin
    open_block (OTHER "mtd") (as_align_mathml format span);
    open_display ()
  end else open_block TD (as_align format span)
;;

let erase_cell () =  
  if flags.in_math && !Parse_opts.mathml then begin
    erase_display ();
    erase_block (OTHER "mtd")
  end else erase_block TD
and close_cell content = 
  if flags.in_math && !Parse_opts.mathml then begin
    close_display ();
    force_block (OTHER "mtd") ""
  end else force_block TD content
and do_close_cell () = 
    if flags.in_math && !Parse_opts.mathml then begin
      close_display ();
      close_block (OTHER "mtd")
    end else close_block TD
and open_cell_group () = open_group ""
and close_cell_group () = close_group ()
and erase_cell_group () = erase_block GROUP
;;


let erase_row () = 
  if flags.in_math && !Parse_opts.mathml then
    erase_block (OTHER "mtr")
  else erase_block TR
and close_row () = 
  if flags.in_math && !Parse_opts.mathml then
    close_block (OTHER "mtr")
  else close_block TR
;;

let close_table () = 
  if flags.in_math && !Parse_opts.mathml then
    close_block (OTHER "mtable")
  else close_block TABLE
;;

let make_border _ = ()
;;


let center_format =
  Tabular.Align  {Tabular.hor="center" ; Tabular.vert = "top" ;
		   Tabular.wrap = false ; Tabular.pre = "" ; 
		   Tabular.post = "" ; Tabular.width = Length.Default} 
;;

let make_inside s multi =
  if not (multi) then begin
    if pblock ()=TD || pblock() = (OTHER "mtd") then begin
      close_cell "&nbsp;";
      open_cell center_format 1 0;
      put s;
    end else begin
      open_cell center_format 1 0;
      put s;
      close_cell "&nbsp;"
    end;
  end
;;


let make_hline w noborder =
  if noborder then begin
    new_row ();
    if not (flags.in_math && !Parse_opts.mathml) then begin
      open_direct_cell "" w ;
      close_mods () ;
      line_in_table () ;
    end else begin
      open_cell center_format w 0;
      close_mods () ;
      put "<mo stretchy=\"true\" > &horbar; </mo>";
      force_item_display ();
    end;
    close_cell "" ;
    close_row ();
  end
;;

let infomenu _ = ()
and infonode _opt _num _arg = ()
and infoextranode _num _arg _text = ()
;;


let image arg n = 
  if flags.in_pre && !Parse_opts.pedantic then begin
    warning "Image tag inside preformatted block, ignored"
  end else begin
    put "<IMG " ;
    if arg <> "" then begin
      put arg;
      put_char ' '
    end ;
    put "SRC=\"" ;
    put n ;
    if !Parse_opts.pedantic then begin
      put "\" ALT=\"" ;
      put n
    end ;
    put "\">"
  end
;;

type saved = HtmlCommon.saved

let check = HtmlCommon.check
and hot = HtmlCommon.hot

let skip_line = skip_line
and flush_out = flush_out
and close_group = close_group
and open_aftergroup = open_aftergroup
and open_group = open_group
and erase_block s = erase_block (find_block s)
and insert_block s = insert_block (find_block s)
and insert_attr s = insert_attr (find_block s)
and force_block s = force_block (find_block s)
and close_block s = close_block (find_block s)
and open_block s = open_block (find_block s)
and forget_par = forget_par
and par = par
and erase_mods = erase_mods
and open_mod = open_mod
and clearstyle = clearstyle
and nostyle = nostyle
and get_fontsize = get_fontsize
and horizontal_line = horizontal_line
and to_string = to_string
;;

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


let header = "$Id: html.ml,v 1.57 1999-06-07 17:42:40 tessaud Exp $" 

(* Output function for a strange html model :
     - Text elements can occur anywhere and are given as in latex
     - A new grouping construct is given (open_group () ; close_group ())
*)

open Misc
open Parse_opts
open Latexmacros
open HtmlCommon

exception Error of string
;;


let r_quote = String.create 1
;;

let quote_char = function
  '<' -> "&lt;"
| '>' -> "&gt;"
| '&' -> "&amp;"
| c   -> (r_quote.[0] <- c ; r_quote)
;;

let r_translate = String.create 1
;;

let iso_translate = function
  '<' -> "&lt;"
| '>' -> "&gt;"
| '&' -> "&amp;"
| ' ' -> "&nbsp;"
| '¡' -> "&iexcl;"
| '¢' -> "&cent;"
| '£' -> "&pound;"
| '¤' -> "&curren;"
| '¥' -> "&yen;"
| '¦' -> "&brvbar;"
| '§' -> "&sect;"
| '¨' -> "&uml;"
| '©' -> "&copy;"
| 'ª' -> "&ordf;"
| '«' -> "&laquo;"
| '¬' -> "&not;"
| '­' -> "&shy;"
| '®' -> "&reg;"
| '¯' -> "&macr;"
| '°' -> "&deg;"
| '±' -> "&plusmn;"
| '²' -> "&sup2;"
| '³' -> "&sup3;"
| '´' -> "&acute;"
| 'µ' -> "&micro;"
| '¶' -> "&para;"
| '·' -> "&middot;"
| '¸' -> "&cedil;"
| '¹' -> "&sup1;"
| 'º' -> "&ordm;"
| '»' -> "&raquo;"
| '¼' -> "&frac14;"
| '½' -> "&frac12;"
| '¾' -> "&frac34;"
| '¿' -> "&iquest;"
| 'À' -> "&Agrave;"
| 'Á' -> "&Aacute;"
| 'Â' -> "&Acirc;"
| 'Ã' -> "&Atilde;"
| 'Ä' -> "&Auml;"
| 'Å' -> "&Aring;"
| 'Æ' -> "&AElig;"
| 'Ç' -> "&Ccedil;"
| 'È' -> "&Egrave;"
| 'É' -> "&Eacute;"
| 'Ê' -> "&Ecirc;"
| 'Ë' -> "&Euml;"
| 'Ì' -> "&Igrave;"
| 'Í' -> "&Iacute;"
| 'Î' -> "&Icirc;"
| 'Ï' -> "&Iuml;"
| 'Ð' -> "&ETH;"
| 'Ñ' -> "&Ntilde;"
| 'Ò' -> "&Ograve;"
| 'Ó' -> "&Oacute;"
| 'Ô' -> "&Ocirc;"
| 'Õ' -> "&Otilde;"
| 'Ö' -> "&Ouml;"
| '×' -> "&times;"
| 'Ø' -> "&Oslash;"
| 'Ù' -> "&Ugrave;"
| 'Ú' -> "&Uacute;"
| 'Û' -> "&Ucirc;"
| 'Ü' -> "&Uuml;"
| 'Ý' -> "&Yacute;"
| 'Þ' -> "&THORN;"
| 'ß' -> "&szlig;"
| 'à' -> "&agrave;"
| 'á' -> "&aacute;"
| 'â' -> "&acirc;"
| 'ã' -> "&atilde;"
| 'ä' -> "&auml;"
| 'å' -> "&aring;"
| 'æ' -> "&aelig;"
| 'ç' -> "&ccedil;"
| 'è' -> "&egrave;"
| 'é' -> "&eacute;"
| 'ê' -> "&ecirc;"
| 'ë' -> "&euml;"
| 'ì' -> "&igrave;"
| 'í' -> "&iacute;"
| 'î' -> "&icirc;"
| 'ï' -> "&iuml;"
| 'ð' -> "&eth;"
| 'ñ' -> "&ntilde;"
| 'ò' -> "&ograve;"
| 'ó' -> "&oacute;"
| 'ô' -> "&ocirc;"
| 'õ' -> "&otilde;"
| 'ö' -> "&ouml;"
| '÷' -> "&divide;"
| 'ø' -> "&oslash;"
| 'ù' -> "&ugrave;"
| 'ú' -> "&uacute;"
| 'û' -> "&ucirc;"
| 'ü' -> "&uuml;"
| 'ý' -> "&yacute;"
| 'þ' -> "&thorn;"
| 'ÿ' -> "&yuml;"
| c   -> (r_translate.[0] <- c ; r_translate)
;;

let iso c =
  if !Parse_opts.iso then
    quote_char c
  else
    iso_translate c
;;

(* Calls to other modules that are in the interface *)

let freeze,
  over,
  insert_vdisplay,
  int_sup_sub,
  limit_sup_sub,
  standard_sup_sub ,
  close_vdisplay_row,
  open_vdisplay_row,
  close_vdisplay,
  open_vdisplay,
  erase_display,
  begin_item_display,
  end_item_display,
  force_item_display,
  item_display,
  close_display,
  open_display,
  close_maths,
  open_maths, 
  put_in_math,
  close_flow,
  inside_stack,
  saved_inside,
  ncols_stack,
  math_put,
  math_put_char
    =
  if !Parse_opts.mathml then begin
    MathML.freeze,
    MathML.over,
    MathML.insert_vdisplay,
    MathML.int_sup_sub,
    MathML.limit_sup_sub,
    MathML.standard_sup_sub,
    MathML.close_vdisplay_row,
    MathML.open_vdisplay_row,
    MathML.close_vdisplay,
    MathML.open_vdisplay,
    MathML.erase_display,
    MathML.begin_item_display,
    MathML.end_item_display,
    MathML.force_item_display,
    MathML.item_display,
    MathML.close_display,
    MathML.open_display,
    MathML.close_maths,
    MathML.open_maths,
    MathML.put_in_math,
    MathML.close_flow,
    MathML.inside_stack,
    MathML.saved_inside,
    MathML.ncols_stack,
    MathML.put,
    MathML.put_char
  end else begin
    HtmlMath.freeze,
    HtmlMath.over,
    HtmlMath.insert_vdisplay,
    HtmlMath.int_sup_sub,
    HtmlMath.limit_sup_sub,
    HtmlMath.standard_sup_sub,
    HtmlMath.close_vdisplay_row,
    HtmlMath.open_vdisplay_row,
    HtmlMath.close_vdisplay,
    HtmlMath.open_vdisplay,
    HtmlMath.erase_display,
    HtmlMath.begin_item_display,
    HtmlMath.end_item_display,
    HtmlMath.force_item_display,
    HtmlMath.item_display,
    HtmlMath.close_display,
    HtmlMath.open_display,
    HtmlMath.close_maths,
    HtmlMath.open_maths,
    HtmlMath.put_in_math,
    HtmlMath.close_flow,
    HtmlMath.inside_stack,
    HtmlMath.saved_inside,
    HtmlMath.ncols_stack,
    HtmlMath.put,
    HtmlMath.put_char
  end
;;


let skip_line = skip_line
and flush_out = flush_out
and close_group = close_group
and open_aftergroup = open_aftergroup
and open_group = open_group
and erase_block = erase_block
and insert_block = insert_block
and force_block = force_block
and close_block = close_block
and open_block = open_block
and forget_par = forget_par
and par = par
and close_mods = close_mods
and open_mods = open_mods
and erase_mods = erase_mods
and open_mod = open_mod
and clearstyle = clearstyle
and nostyle = nostyle
and get_fontsize = get_fontsize
and horizontal_line = horizontal_line
;;



let set_out out =  !cur_out.out <- out
;;


(* acces to flags *)
let is_empty () = flags.empty
and get_last_closed () = flags.last_closed
and set_last_closed s = flags.last_closed <- s
;;

    
(* Independant stacks for flags *)  

let delay_stack = ref []
;;


let debug m =
  Printf.fprintf stderr "%s : table_vsize=%d vsize=%d" m flags.table_vsize flags.vsize ;
  prerr_newline ()
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



let put s = 
  if flags.in_math then math_put s
  else HtmlCommon.put s
;;

let put_char c =
  if flags.in_math then math_put_char c
  else HtmlCommon.put_char c
;;

let set_dt s = flags.dt <- s
and set_dcount s = flags.dcount <- s
;;

let item () =
   if !verbose > 2 then begin
    prerr_string "item: stack=" ;
    pretty_stack !out_stack
  end ;
  let mods = !cur_out.pending @ !cur_out.active in
  do_close_mods () ;
  !cur_out.pending <- mods ;
  let saved =
    if flags.nitems = 0 then begin
      let _ = forget_par () in () ;
      Out.to_string !cur_out.out
    end else  "" in
  flags.nitems <- flags.nitems+1;
  try_flush_par () ;
  do_put "\n<LI>" ;
  do_put saved
;;

let nitem = item
;;

let ditem scan arg =
  if !verbose > 2 then begin
    prerr_string "ditem: stack=" ;
    pretty_stack !out_stack
  end ;
  let mods = !cur_out.pending @ !cur_out.active in
  do_close_mods () ;
  let true_scan =
    if flags.nitems = 0 then begin
      let _ = forget_par () in () ;
      let saved = Out.to_string !cur_out.out in
      (fun arg -> do_put saved ; scan arg)
    end else scan in
  try_flush_par ();
  !cur_out.pending <- mods ;
  flags.nitems <- flags.nitems+1;
  do_put "\n<DT>" ;    
  open_group "" ;
  if flags.dcount <> "" then scan ("\\refstepcounter{"^ flags.dcount^"}") ;
  true_scan ("\\makelabel{"^arg^"}") ;
  close_group () ;
  do_put "<DD>"
;;





let rec forget () =
  let ps = pblock () in
  if ps <> "DELAY" then begin
    force_block ps "" ; forget ()
  end else
    force_block "FORGET" ""
;;

let loc_ref s1 s2 =
  put "<A HREF=\"#" ;
  put s2 ;
  put "\">" ;
  put s1 ;
  put "</A>"
;;

let loc_name s1 s2 =
  let pval = forget_par () in
  put "<A NAME=\"" ;
  put s1 ;
  put "\">" ;
  put s2 ;
  put "</A>" ;
  par pval
;;


  
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

let to_string f =
  let old_flags = copy_flags flags in
  let _ = forget_par () in
  open_group "" ;
  f () ;
  let r = Out.to_string !cur_out.out in
  close_group () ;
  set_flags flags old_flags ;
  r
;;

let to_style f =
  let old_flags = copy_flags flags in
  let _ = forget_par () in
  open_group "" ;
  !cur_out.active  <- [] ;
  !cur_out.pending <- [] ;
  f () ;
  let r = !cur_out.active @ !cur_out.pending in
  erase_block "" ;
  set_flags flags old_flags ;
  r
;;

let get_current_output () = Out.to_string !cur_out.out

let check_stack s what =
  if !what <> [] && not !silent then begin
    prerr_endline ("Warning: stack "^s^" is non-empty in Html.finalize") ;
  end
;;

let finalize check =
  if check then begin
    check_stack "out_stack" out_stack ;
    check_stack "inside_stack" inside_stack;
    check_stack "saved_inside" saved_inside;
    check_stack "table_stack" table_stack;
    check_stack "blank_stack" blank_stack;
    check_stack "empty_stack" empty_stack;
    check_stack "vsize_stack" vsize_stack;
    check_stack "nrows_stack" nrows_stack;
    check_stack "delay_stack" delay_stack;
    check_stack "nitems_stack" nitems_stack;
    check_stack "dt_stack" dt_stack;
    check_stack "dcount_stack" dcount_stack;
    check_stack "ncols_stack" ncols_stack ;
    check_stack "after_stack" after_stack
  end ;
  while !out_stack != [] do
    try close_block (pblock ()) with _ -> ()
  done ;
  Out.close !cur_out.out
;;


let put_separator () =
  put "\n"
;;

let unskip () = 
  Out.unskip !cur_out.out;
  if flags.blank then
    flags.empty <- true;
;;
  
let put_tag tag =
  put tag
;;

let put_nbsp () =
  put "&nbsp;"
;;

let put_open_group () =
  put_char '{'
;;

let put_close_group () =
  put_char '}'
;;



let open_table border htmlargs =
  if border then open_block "TABLE" ("BORDER=1 "^htmlargs)
  else open_block "TABLE" htmlargs
;;

let new_row () =
  open_block "TR" ""
;;


let attribut name = function
  | "" -> ""
  | s  -> " "^name^"="^s
and as_colspan = function
  |  1  -> ""
  |  n -> " COLSPAN="^string_of_int n

let as_align f span = match f with
  Tabular.Align {Tabular.vert=v ; Tabular.hor=h ; Tabular.wrap=w ; Tabular.width=size} ->
    attribut "VALIGN" v^
    attribut "ALIGN" h^
    (if w then "" else " NOWRAP")^
    as_colspan span
| _       ->  raise (Misc.Fatal ("as_align"))
;;

let open_cell format span i= open_block "TD" (as_align format span)
;;

let erase_cell () =  erase_block "TD"
and close_cell content = force_block "TD" content
and do_close_cell () = close_block "TD"
and open_cell_group () = open_group ""
and close_cell_group () = close_group ()
and erase_cell_group () = erase_block ""
;;


let erase_row () = erase_block "TR"
and close_row () = close_block "TR"
;;

let close_table () = close_block "TABLE"
;;
let make_border s = ()
;;


let center_format =
  Tabular.Align  {Tabular.hor="center" ; Tabular.vert = "top" ;
		   Tabular.wrap = false ; Tabular.pre = "" ; 
		   Tabular.post = "" ; Tabular.width = None} 
;;

let make_inside s multi =
  if not (multi) then begin
    if pblock ()="TD" then begin
      close_cell "&nbsp";
      open_cell center_format 1 0;
      put s;
    end else begin
      open_cell center_format 1 0;
      put s;
      close_cell "&nbsp"
    end;
  end
;;

let make_hline w noborder =
  if noborder then begin
    new_row ();
    open_cell center_format w 0;
    close_mods () ;
    horizontal_line "NOSHADE" "2" "100" ;
    close_cell "" ;
    close_row ();
  end
;;

let infomenu arg = ()
and infonode opt num arg = ()
;;


let image arg n = 
  put "<IMG " ;
  if arg <> "" then begin
    put arg;
    put_char ' '
  end ;
  put "SRC=\"" ;
  put n ;
  put "\">"
;;


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


let header = "$Id: html.ml,v 1.75 2000-05-23 18:00:38 maranget Exp $" 

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

let iso_buff = Out.create_buff ()

let iso_string s =
  if !Parse_opts.iso then begin
    for i = 0 to String.length s - 1 do
      Out.put iso_buff (iso_translate s.[i])
    done ;
    Out.to_string iso_buff
  end else
    s
(* Calls to other modules that are in the interface *)

let 
  over,
  int_sup_sub,
  limit_sup_sub,
  standard_sup_sub ,
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
  math_put,
  math_put_char,
  left,
  right
    =
  if !Parse_opts.mathml then begin
    MathML.over,
    MathML.int_sup_sub,
    MathML.limit_sup_sub,
    MathML.standard_sup_sub,
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
    MathML.put,
    MathML.put_char,
    MathML.left,
    MathML.right
  end else begin
    HtmlMath.over,
    HtmlMath.int_sup_sub,
    HtmlMath.limit_sup_sub,
    HtmlMath.standard_sup_sub,
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
    HtmlMath.put,
    HtmlMath.put_char,
    HtmlMath.left,
    HtmlMath.right
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
and erase_mods = erase_mods
and open_mod = open_mod
and clearstyle = clearstyle
and nostyle = nostyle
and get_fontsize = get_fontsize
and horizontal_line = horizontal_line
and close_flow = close_flow
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
  Printf.fprintf stderr "%s : table_vsize=%d vsize=%d" m flags.table_vsize flags.vsize ;
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

let set_dt s = flags.dt <- s
and set_dcount s = flags.dcount <- s
;;

let item () =
   if !verbose > 2 then begin
    prerr_string "item: stack=" ;
    pretty_stack out_stack
  end ;
  let mods = to_pending !cur_out.pending !cur_out.active in
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
    pretty_stack out_stack
  end ;
  let mods = to_pending !cur_out.pending !cur_out.active in
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

(*
let loc_ref s1 s2 =
  if flags.in_math && !Parse_opts.mathml then begin
    put "<mrow xml:link=\"simple\" href=\"";
    put s2;
    put "\">";
    put s1;
    put "</mrow>"
  end else begin
    put "<A HREF=\"#" ;
    put s2 ;
    put "\">" ;
    put s1 ;
    put "</A>"
  end
;;


let loc_name s1 s2 =
  let pval = forget_par () in
  if flags.in_math && !Parse_opts.mathml then begin
    put "<mrow xml:link=\"simple\" id=\"";
    put s1;
    put "\">";
    put s2;
    put "</mrow>";
  end else begin
    put "<A NAME=\"" ;
    put s1 ;
    put "\">" ;
    put s2 ;
    put "</A>" ;
  end;
  par pval
;;
*)
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

let to_string f =
(*
  prerr_string "to_string: " ;
  debug_empty flags ;
  Out.debug stderr !cur_out.out ;
  prerr_endline "" ;
*)
  let old_flags = copy_flags flags in
  let _ = forget_par () in
  open_group "" ;
  f () ;
  let r = Out.to_string !cur_out.out in
  flags.empty <- true ;
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
  let r = to_pending !cur_out.pending !cur_out.active in
  erase_block "" ;
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
  if flags.in_math && !Parse_opts.mathml then
    put " "
  else
    put "&nbsp;"
;;

let put_open_group () =
  put_char '{'
;;

let put_close_group () =
  put_char '}'
;;



let open_table border htmlargs =
  let table,arg_b, arg =
    if flags.in_math && !Parse_opts.mathml then
      "mtable","frame = \"solid\"",""
    else "TABLE","BORDER=1",htmlargs
  in
  if border then open_block table (arg_b^" "^arg)
  else open_block table arg
;;

let new_row () =
  if flags.in_math && !Parse_opts.mathml then
    open_block "mtr" ""
  else open_block "TR" ""
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
  Tabular.Align {Tabular.vert=v ; Tabular.hor=h ; Tabular.wrap=w ; Tabular.width=size} ->
    attribut "VALIGN" v^
    attribut "ALIGN" h^
    (if w then "" else " NOWRAP")^
    as_colspan span
| _       ->  raise (Misc.Fatal ("as_align"))
;;

let as_align_mathml f span = match f with
  Tabular.Align {Tabular.vert=v ; Tabular.hor=h ; Tabular.wrap=w ; Tabular.width=size} ->
    attribut "rowalign" ("\""^v^"\"")^
    attribut "columnalign" ("\""^h^"\"")^
    as_colspan_mathml span
| _       ->  raise (Misc.Fatal ("as_align_mathml"))
;;

let open_direct_cell attrs span =
  if flags.in_math && !Parse_opts.mathml then begin
    open_block "mtd" (attrs^as_colspan_mathml span);
    open_display ()
  end else open_block "TD" (attrs^as_colspan span)

let open_cell format span i= 
  if flags.in_math && !Parse_opts.mathml then begin
    open_block "mtd" (as_align_mathml format span);
    open_display ()
  end else open_block "TD" (as_align format span)
;;

let erase_cell () =  
  if flags.in_math && !Parse_opts.mathml then begin
    erase_display ();
    erase_block "mtd"
  end else erase_block "TD"
and close_cell content = 
  if flags.in_math && !Parse_opts.mathml then begin
    close_display ();
    force_block "mtd" ""
  end else force_block "TD" content
and do_close_cell () = 
    if flags.in_math && !Parse_opts.mathml then begin
      close_display ();
      close_block "mtd"
    end else close_block "TD"
and open_cell_group () = open_group ""
and close_cell_group () = close_group ()
and erase_cell_group () = erase_block ""
;;


let erase_row () = 
  if flags.in_math && !Parse_opts.mathml then
    erase_block "mtr"
  else erase_block "TR"
and close_row () = 
  if flags.in_math && !Parse_opts.mathml then
    close_block "mtr"
  else close_block "TR"
;;

let close_table () = 
  if flags.in_math && !Parse_opts.mathml then
    close_block "mtable"
  else close_block "TABLE"
;;
let make_border s = ()
;;


let center_format =
  Tabular.Align  {Tabular.hor="center" ; Tabular.vert = "top" ;
		   Tabular.wrap = false ; Tabular.pre = "" ; 
		   Tabular.post = "" ; Tabular.width = Length.Default} 
;;

let make_inside s multi =
  if not (multi) then begin
    if pblock ()="TD" || pblock() = "mtd" then begin
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
      open_direct_cell "BGCOLOR=black" w ;
      close_mods () ;
      line_in_table 3 ;
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

let infomenu arg = ()
and infonode opt num arg = ()
and infoextranode num arg text = ()
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

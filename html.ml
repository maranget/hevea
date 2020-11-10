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

(* Output function for a strange html model :
   - Text elements can occur anywhere and are given as in latex
   - A new grouping construct is given (open_group () ; close_group ())
*)

open Misc
open HtmlCommon
open Printf

exception Error of string


let addvsize x = flags.vsize <- flags.vsize + x

(* Calls to other modules that are in the interface *)

let
    over,
  erase_display,
  _begin_item_display,
  _end_item_display,
  force_item_display,
  item_display,
  do_close_display,
  do_open_display_varg,
  do_open_display,
  do_close_maths,
  do_open_maths,
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
    (fun () -> HtmlMath.close_display false),
    (HtmlMath.open_display_varg false),
    (fun () -> HtmlMath.open_display false),
    HtmlMath.close_maths,
    HtmlMath.open_maths,
    HtmlMath.put_in_math,
    HtmlMath.put,
    HtmlMath.put_char,
    HtmlMath.left,
    HtmlMath.right
  end

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



let set_out out =  !cur_out.out <- out

and stop () =
  MyStack.push stacks.s_active !cur_out.out ;
  !cur_out.out <- Out.create_null ()

and restart () =
  !cur_out.out <- MyStack.pop stacks.s_active


(* acces to flags *)
let is_empty () = flags.empty



let put s =
  if flags.in_math then math_put s
  else HtmlCommon.put s


let put_char c =
  if flags.in_math then math_put_char c
  else HtmlCommon.put_char c

let put_unicode i =  OutUnicode.html_put put put_char i

let loc_name _ = ()


(* freeze everyting and change output file *)

let open_chan chan =
  open_group "" ;
  !cur_out.out <- Out.create_chan chan

let close_chan () =
  Out.close !cur_out.out ;
  !cur_out.out <- Out.create_buff () ;
  close_group ()


let to_style f =
  let old_flags = copy_flags flags in
  open_block INTERN "" ;
  (*  clearstyle () ; *)
  f () ;
  let r = to_pending !cur_out.pending !cur_out.active in
  erase_block INTERN ;
  set_flags flags old_flags ;
  r

let get_current_output () = Out.to_string !cur_out.out


let finalize check =
  if check then begin
    check_stacks ()
  end else begin
    (* Flush output in case of fatal error *)
    let rec close_rec () =
      if not (MyStack.empty out_stack) then begin
        match MyStack.pop out_stack with
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


let put_separator () = put "\n"

let unskip () =
  Out.unskip !cur_out.out;
  if flags.blank then
    flags.empty <- true

let put_tag tag = put tag

module HorizontalSpace =
  struct
    (* Width of a character ("gauge") given in multiples of an [em]
       and the Unicode which generates the character. *)
    type gauged_character = float * OutUnicode.unichar

    type configuration = {
        normal : gauged_character list;
        minimal : OutUnicode.unichar; (* smallest space possible -- unspecified gauge *)
        zero : OutUnicode.unichar (* zero width but same line-braking behavior as other spaces *)
      }

    (* https://en.wikipedia.org/wiki/whitespace_character#spaces_in_unicode
       https://en.wikipedia.org/wiki/zero-width_space *)
    let html_spaces = {
        normal = [1.0, OutUnicode.emsp;
                  0.5, OutUnicode.ensp;
                  1.0 /. 3.0, OutUnicode.emsp13;
                  0.25, OutUnicode.emsp14;
                  1.0 /. 6.0, OutUnicode.six_per_em_space];
        minimal = OutUnicode.hairsp;
        zero = OutUnicode.zero_width_space
      }

    let approximate_hspace persistent space_configuration length =
      let join_if_persistent () = if persistent then put_unicode OutUnicode.zero_width_joiner in
        let rec iter has_put_normal_space available_normal_spaces remaining_width =
          match available_normal_spaces with
          | [] ->
             (* if remaining_width > 0.0 then
               Printf.eprintf
                 "+ approximate_hspace: %s -- remaining error %fem = %fpx\n"
                 (Length.pretty length) remaining_width (remaining_width *. 16.0); *)
             if remaining_width > 0.0 && not has_put_normal_space then
               begin
                 put_unicode space_configuration.minimal;
                 join_if_persistent ()
               end
          | (space_char_width, space_char) :: remaining_normal_spaces ->
             let number_of_spaces = floor (remaining_width /. space_char_width) in
               let remaining_width' = remaining_width -. number_of_spaces *. space_char_width
               and n = int_of_float number_of_spaces in
                 for _i = 1 to n do
                   put_unicode space_char;
                   join_if_persistent ()
                 done;
                 iter (has_put_normal_space || n >= 1) remaining_normal_spaces remaining_width'
        in
          match length with
          | Length.Char n ->
             (* Printf.eprintf "+ approximate_hspace: Char %d\n" n; *)
             if n = 0 then
               put_unicode space_configuration.zero
             else
               for _i = 1 to n do
                 put_unicode OutUnicode.emsp13;
                 join_if_persistent ()
               done
          | Length.Pixel x ->
             (* Printf.eprintf "+ approximate_hspace: Pixel %d\n" x; *)
             if x < 0 then
               Misc.warning "ignoring \\hspace or \\hspace* with negative length"
             else if x = 0 then
               put_unicode space_configuration.zero
             else
               iter false space_configuration.normal (Length.pixel_to_char_float x)
          | Length.Percent _ | Length.NotALength _ | Length.Default ->
             failwith "approximate_hspace: never reached"
  end (* HorizontalSpace *)

let put_hspace persistent length =
  if !Lexstate.whitepre || (flags.in_math && !Parse_opts.mathml) then
    for _i = 1 to Length.as_number_of_chars length do
      put_char ' '
    done
  else
    HorizontalSpace.(approximate_hspace persistent html_spaces length)

let put_open_group () =
  put_char '{'

let put_close_group () =
  put_char '}'


let infomenu _ = ()
and infonode _opt _num _arg = ()
and infoextranode _num _arg _text = ()


let image arg n =
  if flags.in_pre && !Parse_opts.pedantic then begin
    warning "Image tag inside preformatted block, ignored"
  end else begin
    put "<img " ;
    if arg <> "" then begin
      put arg;
      put_char ' '
    end ;
    put "src=\"" ;
    put n ;
    if !Parse_opts.pedantic then begin
      put "\" alt=\"" ;
      put n
    end ;
    put "\">"
  end

type saved = HtmlCommon.saved

let check = HtmlCommon.check
and hot = HtmlCommon.hot

let forget_par () = None

let rec do_open_par ~attr () = match pblock () with
  | GROUP ->
      let pending = to_pending !cur_out.pending !cur_out.active in
      let a,b,_ = top_out out_stack in
      ignore (close_block_loc check_empty GROUP) ;
      do_open_par ~attr () ;
      open_block a b ;
      !cur_out.pending <- pending
  | P ->
      Misc.warning "Opening P twice" (* An error in fact ! *)
  | s ->
      if !verbose > 2 then
        Printf.eprintf "Opening par below: '%s'\n" (string_of_block s) ;
      open_block P attr

let open_par ?(attr="") () = do_open_par ~attr ()

let rec do_close_par () = match pblock () with
  | GROUP ->
      let pending = to_pending !cur_out.pending !cur_out.active in
      let a,b,_ = top_out out_stack in
      ignore (close_block_loc check_empty GROUP) ;
      let r = do_close_par () in
      open_block a b ;
      !cur_out.pending <- pending ;
      r
  | P ->
      ignore (close_flow_loc check_blank P) ;
      true
  | _ ->
      false


let close_par () = do_close_par ()

(* Find P, maybe above groups *)
let rec find_prev_par () = match pblock () with
  | P -> true
  | GROUP ->
      let x = pop_out out_stack in
      let r = find_prev_par () in
      push_out out_stack x ;
      r
  | _ -> false

let rec do_close_prev_par () = match pblock () with
  | P ->
      ignore (close_flow_loc check_blank P)
  | GROUP ->
      let pending = to_pending !cur_out.pending !cur_out.active in
      let b,a,_ = top_out out_stack in
      ignore (close_block_loc check_empty GROUP) ;
      do_close_prev_par () ;
      open_block b a ;
      !cur_out.pending <- pending
  | _ -> assert false

let close_prev_par () =
  do_close_prev_par () ;
  flags.saw_par <- true

let rec do_par () = match pblock () with
  | P ->
      ignore (close_flow_loc check_blank P) ; open_block P ""
  | GROUP ->
      let pending = to_pending !cur_out.pending !cur_out.active in
      let b,a,_ = top_out out_stack in
      ignore (close_block_loc check_empty GROUP) ;
      do_par () ;
      open_block b a ;
      !cur_out.pending <- pending
  | s ->
      if !verbose > 2 then
        Printf.eprintf "Opening par below: '%s'\n" (string_of_block s) ;
      open_block P ""

let par _ = do_par ()

(* Interface open block: manage par above *)
let open_block_loc = open_block (* save a reference to basic open_block *)

let open_block_with_par ss s a =
  if transmit_par s && find_prev_par () then begin
    if !verbose > 2 then begin
      Printf.eprintf "OPEN: %s, closing par\n" ss ;
      Printf.eprintf "BEFORE: " ;
      pretty_stack out_stack
    end ;
    close_prev_par () ;
    if !verbose > 2 then begin
      Printf.eprintf "AFTER: " ;
      pretty_stack out_stack
    end
  end ;
  open_block_loc s a

let open_block ?(force_inline=false) ss a =
  let s = find_block ss in
    if force_inline then
      open_block_loc s a
    else
      open_block_with_par ss s a

let open_display () =
  if find_prev_par () then begin
    close_prev_par ()
  end ;
  do_open_display ()

and open_display_varg a =
  if find_prev_par () then begin
    close_prev_par ()
  end ;
  do_open_display_varg a

and close_display () =
  do_close_display () ;
  if flags.saw_par then begin
    flags.saw_par <- false ;
    open_par ()
  end

let open_maths display =
  if display && find_prev_par () then begin
    close_prev_par ()
  end ;
  do_open_maths display

and close_maths display =
  do_close_maths display ;
  if flags.saw_par then begin
    flags.saw_par <- false ;
    open_par ()
  end


let wrap_close close_block s =
  let s = find_block s in
  begin match s with GROUP -> () | _ -> ignore (close_par ()) end ;
  begin match s with
    | UL|OL ->
        if flags.nitems > 0 then
          close_block LI
        else
          warning "List with no item"
    | DL ->
        if flags.nitems > 0 then
          close_block DD
        else
          warning "List with no item"
    | _ -> ()
  end ;
  close_block s ;
  if flags.saw_par then begin
    flags.saw_par <- false ;
    if !verbose > 2 then begin
      Misc.warning "RE-OPEN PAR:" ;
      Printf.eprintf "BEFORE: " ;
      pretty_stack out_stack
    end ;
    open_par () ;
    if !verbose > 2 then begin
      Printf.eprintf "AFTER: " ;
      pretty_stack out_stack
    end
  end

let force_block_with_par s content =
  ignore (close_par ()) ;
  force_block s content

and close_block_with_par s =
  ignore (close_par ()) ;
  close_block s

and erase_block_with_par s =
  ignore (close_par ()) ;
  erase_block s

and force_block s content = wrap_close (fun s -> force_block s content) s
and close_block s = wrap_close close_block s
and erase_block s = wrap_close erase_block s
and close_flow s =
  prerr_endline ("FLOW: "^s) ;
  wrap_close close_flow s

let skip_line = skip_line
and flush_out = flush_out
and close_group = close_group
and open_aftergroup = open_aftergroup
and open_group = open_group
and insert_block s attr =
  if find_prev_par () then
    warning "Ignoring \\centering or \\ragged..."
  else
    insert_block (find_block s) attr
and insert_attr s = insert_attr (find_block s)
and erase_mods = erase_mods
and open_mod = open_mod
and has_mod = has_mod
and clearstyle = clearstyle
and nostyle = nostyle
and get_fontsize = get_fontsize
and to_string = to_string

(****************************************)
(* Table stuff, must take P into acount *)
(****************************************)

let open_table border htmlargs =
  let _,arg_b, arg =
    if flags.in_math && !Parse_opts.mathml then
      "mtable","frame = \"solid\"",""
    else "table","border=1",htmlargs
  in
  (* open_block will close P (and record that) if appropriate *)
  if border then open_block_with_par "table" TABLE (arg_b^" "^arg)
  else open_block_with_par "table" TABLE arg

let new_row () =
  if flags.in_math && !Parse_opts.mathml then
    open_block_loc (OTHER "mtr") ""
  else open_block_loc TR ""


let attribut name = function
  | "" -> ""
  | s  -> " "^name^"="^s
and as_colspan = function
  |  1  -> ""
  |  n -> " colspan="^string_of_int n
and as_colspan_mathml = function
  |  1  -> ""
  |  n -> " columnspan= \""^string_of_int n^"\""
and style param value =
  if value = "" then ""
  else sprintf "%s:%s;" param value

let as_align f span border = match f with
    Tabular.Align
      {Tabular.vert=v ; Tabular.hor=h ;
       Tabular.wrap=w ; Tabular.width=_} ->
        sprintf "style=\"%s%s%s%s\" %s"
          (style "vertical-align" v)
          (style "text-align" h)
          (if border then "border:solid 1px;" else "")
          (if w then "" else "white-space:nowrap")
          (as_colspan span)
  | _       ->  raise (Misc.Fatal ("as_align"))

let as_align_mathml f span = match f with
    Tabular.Align
      {Tabular.vert=v ; Tabular.hor=h } ->
        attribut "rowalign" ("\""^v^"\"")^
          attribut "columnalign" ("\""^h^"\"")^
          as_colspan_mathml span
  | _       ->  raise (Misc.Fatal ("as_align_mathml"))

let open_direct_cell attrs span =
  if flags.in_math && !Parse_opts.mathml then begin
    open_block_loc (OTHER "mtd") (attrs^as_colspan_mathml span);
    do_open_display ()
  end else open_block_loc TD (attrs^as_colspan span)

let open_cell format span _ border =
  if flags.in_math && !Parse_opts.mathml then begin
    open_block_loc (OTHER "mtd") (as_align_mathml format span);
    do_open_display ()
  end else open_block_loc TD (as_align format span border)

(* By contrast closing/erasing TD, may in some occasions
   implies closing some internal P => use wrapped close functions *)
let erase_cell () =
  if flags.in_math && !Parse_opts.mathml then begin
    erase_display ();
    erase_block_with_par (OTHER "mtd")
  end else erase_block_with_par TD

and close_cell content =
  if flags.in_math && !Parse_opts.mathml then begin
    do_close_display ();
    force_block_with_par (OTHER "mtd") ""
  end else force_block_with_par TD content

and do_close_cell () =
  if flags.in_math && !Parse_opts.mathml then begin
    do_close_display ();
    close_block_with_par (OTHER "mtd")
  end else close_block_with_par TD

and open_cell_group () = open_group ""
and close_cell_group () = close_group ()
and erase_cell_group () = erase_group ()


let erase_row () =
  if flags.in_math && !Parse_opts.mathml then
    HtmlCommon.erase_block (OTHER "mtr")
  else HtmlCommon.erase_block TR

and close_row () =
  if flags.in_math && !Parse_opts.mathml then
    HtmlCommon.close_block (OTHER "mtr")
  else HtmlCommon.close_block TR

let close_table () =
  begin if flags.in_math && !Parse_opts.mathml then
      HtmlCommon.close_block (OTHER "mtable")
    else HtmlCommon.close_block TABLE
  end ;
  if flags.saw_par then begin
    flags.saw_par <- false ;
    open_par ()
  end

let make_border _ = ()


let inside_format =
  Tabular.Align  {Tabular.hor="center" ; Tabular.vert = "" ;
                  Tabular.wrap = false ; Tabular.pre = "" ;
                  Tabular.post = "" ; Tabular.width = Length.Default}
and hline_format =
  Tabular.Align  {Tabular.hor="center" ; Tabular.vert = "top" ;
                  Tabular.wrap = false ; Tabular.pre = "" ;
                  Tabular.post = "" ; Tabular.width = Length.Default}

let make_inside s multi =
  if not multi then begin
    let pb =  pblock () in
    match pb with
    | TD|OTHER "mtd"|P ->
      close_cell "&nbsp;";
      open_cell inside_format 1 0 false;
      put s
    | _ ->
      open_cell inside_format 1 0 false;
      put s;
      close_cell "&nbsp;"
  end


let make_hline w noborder =
  if noborder then begin
    new_row ();
    if not (flags.in_math && !Parse_opts.mathml) then begin
      open_direct_cell "class=\"hrule\"" w ;
      close_cell ""
    end else begin
      open_cell hline_format w 0 false;
      close_mods () ;
      put "<mo stretchy=\"true\" > &horbar; </mo>";
      force_item_display ();
      close_cell ""
    end;
    close_row ();
  end

(* HR is not correct inside P *)
let horizontal_line attr width height =
  if find_prev_par () then begin
    close_prev_par ()
  end ;
  horizontal_line attr width height ;
  if flags.saw_par then begin
    flags.saw_par <- false ;
    open_par ()
  end

(* Lists also have to take P into account *)
let rec do_li s = match pblock () with
  | P ->
      let pend = to_pending !cur_out.pending !cur_out.active in
      ignore (close_flow_loc check_blank P) ;
      do_li s ;
      !cur_out.pending <- pend
  | LI ->
      ignore (close_flow_loc no_check LI) ;
      open_block_loc LI s
  | GROUP ->
      let pend = to_pending !cur_out.pending !cur_out.active in
      let a,b,_ = top_out out_stack in
      ignore (close_block_loc check_empty GROUP) ;
      do_li s ;
      open_block_loc a b ;
      !cur_out.pending <- pend
  | _ -> assert false



let item s =
  if !verbose > 2 then begin
    prerr_string "=> item: stack=" ;
    pretty_stack out_stack
  end ;
  if flags.nitems > 0 then begin
    do_li s
  end else begin
    let saved =
      let pending = to_pending !cur_out.pending !cur_out.active in
      do_close_mods () ;
      ignore (close_par ()) ; (* in case some par opened before first \item *)
      let r = Out.to_string !cur_out.out in
      !cur_out.pending <- pending ;
      r in
    open_block_loc LI s ;
    do_put saved
  end ;
  if !verbose > 2 then begin
    prerr_string "<= item: stack=" ;
    pretty_stack out_stack
  end ;
  flags.nitems <- flags.nitems+1

let nitem = item

and set_dcount s = flags.dcount <- s

(*********************************************)
(*  s1 and s2 below are attributes to DR/DD  *)
(*********************************************)

let emit_dt_dd scan true_scan arg s1 s2 =
  open_block_loc DT s1 ;
  if flags.dcount <> "" then scan ("\\refstepcounter{"^ flags.dcount^"}") ;
  true_scan ("\\makelabel{"^arg^"}") ;
  ignore (close_block_loc no_check DT) ;
  open_block_loc DD s2


let rec do_dt_dd scan true_scan arg s1 s2 = match pblock () with
  | P ->
      let pend = to_pending !cur_out.pending !cur_out.active in
      ignore (close_flow_loc check_blank P) ;
      do_dt_dd scan true_scan arg s1 s2  ;
      !cur_out.pending <- pend
  | DD ->
      ignore (close_flow_loc no_check DD) ;
      emit_dt_dd scan true_scan arg s1 s2
  | GROUP ->
      let pend = to_pending !cur_out.pending !cur_out.active in
      let a,b,_ = top_out out_stack in
      ignore (close_block_loc check_empty GROUP) ;
      do_dt_dd scan true_scan arg s1 s2 ;
      open_block_loc a b ;
      !cur_out.pending <- pend
  | _ -> assert false

let ditem scan arg s1 s2 =
  if !verbose > 2 then begin
    Printf.eprintf "=> DITEM: `%s' `%s' `%s'\n" arg s1 s2 ;
    prerr_string "ditem: stack=" ;
    pretty_stack out_stack
  end ;
  let true_scan =
    if flags.nitems = 0 then begin
      let pending = to_pending !cur_out.pending !cur_out.active in
      do_close_mods () ;
      ignore (close_par ()) ; (* in case some par opened before first \item *)
      let saved = Out.to_string !cur_out.out in
      !cur_out.pending <- pending ;
      (fun arg -> do_put saved ; scan arg)
    end
    else scan in
  begin if flags.nitems > 0 then
      do_dt_dd scan true_scan arg s1 s2
    else
      emit_dt_dd scan true_scan arg s1 s2
  end ;
  flags.nitems <- flags.nitems+1 ;
  if !verbose > 2 then begin
    Printf.eprintf "<= DITEM: `%s' `%s' `%s'\n" arg s1 s2 ;
    prerr_string "ditem: stack=" ;
    pretty_stack out_stack
  end ;

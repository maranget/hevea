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

let header = "$Id: html.ml,v 1.50 1999-05-20 16:11:41 tessaud Exp $" 

(* Output function for a strange html model :
     - Text elements can occur anywhere and are given as in latex
     - A new grouping construct is given (open_group () ; close_group ())
*)

open Misc
open Parse_opts
open Latexmacros

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

exception Close of string
;;

let failclose s = raise (Close s)
;;

let check_block_closed opentag closetag =
  if opentag <> closetag && not (opentag = "AFTER" && closetag = "") then
    failclose ("hml: ``"^closetag^"'' closes ``"^opentag^"''") ;

type 'a ok  = No | Yes of 'a
;;

(* Saving mods accross blocks *)
let push s e = s := e:: !s
and pop name s = match !s with
  [] -> raise (Misc.Fatal ("Empty stack: "^name^" in Html"))
| e::rs -> s := rs ; e
and see_top name s = match !s with
  [] -> raise (Misc.Fatal ("Empty stack: "^name^" in Html (see)"))
| e::_ -> e
;;

(* output globals *)
type status = {
  mutable nostyle : bool ;
  mutable pending : env list ;
  mutable active : env list ;
  mutable out : Out.t}
;;

(* free list for buffers *)
let free_list = ref []
;;

let free out =
  out.nostyle <- false ;
  out.pending <- [] ;
  out.active <- [] ;
  Out.reset out.out ;
  free_list := out :: !free_list
;;



let new_status nostyle pending active = match !free_list with
  [] ->
   {nostyle=nostyle ;
   pending = pending  ; active = active ; out = Out.create_buff ()}
| x::rest ->
   free_list := rest ;
   x.nostyle <- nostyle ;
   x.pending <- pending ;
   x.active <- active ;
   assert (Out.is_empty x.out) ;
   x
;;

let cur_out = ref {nostyle=false ;
pending = [] ; active = [] ; out = Out.create_null ()}
;;

let pretty_mods mods =
  let rec do_rec = function
    [x]  -> prerr_string (Latexmacros.pretty_env x)
  | x::xs ->
     prerr_string (Latexmacros.pretty_env x^"; ") ;
     do_rec xs
  | [] -> () in
  prerr_string "[" ;
  do_rec mods ;
  prerr_string "]"
;;

     
let pretty_cur {pending = pending ; active = active} =
  prerr_string "pending = " ;
  pretty_mods pending ;
  prerr_string " active = " ;
  pretty_mods active
;;

let set_out out =  !cur_out.out <- out
;;


type stack_item =
  Normal of string * string * status
| Freeze of (unit -> unit)
;;

exception PopFreeze
;;

let push_out s (a,b,c) = push s (Normal (a,b,c))
;;

let pretty_stack s =
  prerr_string "|" ;
  List.iter
   (function Normal (s,args,_) ->
     prerr_string ("["^s^"]-{"^args^"} ")
   | Freeze _   -> prerr_string "Freeze ") s ;
  prerr_endline "|"
;;

let rec pop_out s = match pop "out" s with
  Normal (a,b,c) -> a,b,c
| Freeze f       -> raise PopFreeze
(* begin
  if !verbose > 2 then begin
     prerr_string "unfreeze in pop_out" ;
     pretty_stack !s
  end ;
  f () ; pop_out s end
*)
;;


let out_stack = ref []
;;

let pblock () = match !out_stack with
  Normal (s,_,_)::_ -> s
| _ -> ""
and parg ()  = match !out_stack with
  Normal (_,s,_)::_ -> s
| _ -> ""
;;

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


let do_put_char c =
 if !verbose > 3 then
    prerr_endline ("put_char: |"^String.escaped (String.make 1 c)^"|");
 Out.put_char !cur_out.out c

and do_put s =
 if !verbose > 3 then
    prerr_endline ("put: |"^String.escaped s^"|");
  Out.put !cur_out.out s
;;

(* Flags section *)
type flags_t = {
    mutable table_inside:bool;
    mutable ncols:int;
    mutable empty:bool;
    mutable blank:bool;
    mutable pending_par: int option;
    mutable vsize:int;
    mutable nrows:int;
    mutable table_vsize:int;
    mutable nitems:int;
    mutable dt:string;
    mutable dcount:string;
    mutable last_closed:string;
    mutable in_pre:bool;
    mutable insert: (string * string) option;
} ;;


let flags = {
  table_inside = false;
  ncols = 0;
  empty = true;
  blank = true;
  pending_par = None;
  vsize = 0;
  nrows = 0;
  table_vsize = 0;
  nitems = 0;
  dt = "";
  dcount = "";
  last_closed = "rien";
  in_pre = false;
  insert = None;
} ;;

let copy_flags {
  table_inside = table_inside;
  ncols = ncols;
  empty = empty;
  blank = blank;
  pending_par = pending_par;
  vsize = vsize;
  nrows = nrows;
  table_vsize = table_vsize;
  nitems = nitems;
  dt = dt;
  dcount = dcount;
  last_closed = last_closed;
  in_pre = in_pre;
  insert = insert;
} = {
  table_inside = table_inside;
  ncols = ncols;
  empty = empty;
  blank = blank;
  pending_par = pending_par;
  vsize = vsize;
  nrows = nrows;
  table_vsize = table_vsize;
  nitems = nitems;
  dt = dt;
  dcount = dcount;
  last_closed = last_closed;
  in_pre = in_pre;
  insert = insert;
}
and set_flags f {
  table_inside = table_inside ;
  ncols = ncols;
  empty = empty;
  blank = blank;
  pending_par = pending_par;
  vsize = vsize;
  nrows = nrows;
  table_vsize = table_vsize;
  nitems = nitems;
  dt = dt;
  dcount = dcount;
  last_closed = last_closed;
  in_pre = in_pre;
  insert = insert;
} =
  f.table_inside <- table_inside;
  f.ncols <- ncols;
  f.empty <- empty;
  f.blank <- blank;
  f.pending_par <- pending_par;
  f.vsize <- vsize;
  f.nrows <- nrows;
  f.table_vsize <- table_vsize;
  f.nitems <- nitems;
  f.dt <- dt;
  f.dcount <- dcount;
  f.last_closed <- last_closed;
  f.in_pre <- in_pre;
  f.insert <- insert
;;


(* acces to flags *)
let is_empty () = flags.empty
and get_last_closed () = flags.last_closed
and set_last_closed s = flags.last_closed <- s
;;

    
(* Independant stacks for flags *)  
let inside_stack = ref []
and saved_inside = ref []
;;

let table_stack = ref []
;;

let blank_stack = ref []
and empty_stack = ref []
;;


let vsize_stack = ref []
and nrows_stack = ref []
;;

let delay_stack = ref []
and after_stack = ref []
;;

let nitems_stack = ref []
;;

let dt_stack = ref []
and dcount_stack = ref []

let ncols_stack = ref []
;;

let insert_stack = ref []


let sbool = function true -> "true" | _ -> "false"
;;

let prerr_flags s =
  prerr_endline ("<"^string_of_int (List.length !empty_stack)^"> "^s^
    " empty="^sbool flags.empty^
    " blank="^sbool flags.blank)
and prerr_inside s =
  prerr_endline ("<"^string_of_int (List.length !inside_stack)^" "^string_of_int (List.length !saved_inside)^"> "^s^
    " table_inside="^sbool flags.table_inside)
;;


let is_header s =
  String.length s = 2 && String.get s 0 = 'H'
;;

let is_list = function
  "UL" | "DL" | "OL" -> true
| _ -> false
;;


let par_val last now n =
  if is_list last then begin
    if is_list now then 1 else 0
  end else if
    is_header last || last = "PRE" || last = "BLOCKQUOTE"
  then n-1
  else if last = "DIV" || last = "TABLE" then n
  else n+1
;;

let par  = function
  | Some n as p ->
      flags.pending_par <- p ;
      if !verbose > 2 then
        prerr_endline
          ("par: last_close="^ flags.last_closed^
           " r="^string_of_int n)
  | _ ->  flags.pending_par <- None
;;

let flush_par n =
  flags.pending_par <- None ;
  let p = par_val flags.last_closed (pblock()) n in
  for i = 1 to p do
    do_put "<BR>\n"
  done ;
  if !verbose > 2 then
     prerr_endline
       ("flush_par: last_closed="^ flags.last_closed^
       " p="^string_of_int p);
  flags.vsize <- flags.vsize + p;
  flags.last_closed <- "rien"
;;

let try_flush_par () = match flags.pending_par with
| Some n -> flush_par n
| _      -> ()

let forget_par () =
  let r = flags.pending_par in
  flags.pending_par <- None ;
  r
;;


let debug m =
  Printf.fprintf stderr "%s : table_vsize=%d vsize=%d" m flags.table_vsize flags.vsize ;
  prerr_newline ()
;;

(* styles *)

let do_close_mod = function
  Style m ->  do_put ("</"^m^">")
| (Color _ | Font _)  ->  do_put "</FONT>"

and do_open_mod e =
  if !verbose > 3 then
      prerr_endline ("do_open_mod: "^Latexmacros.pretty_env e) ;
  match e with
  Style m ->  do_put ("<"^m^">")
| Font i  ->
    do_put ("<FONT SIZE="^string_of_int i^">")
| Color s ->  do_put ("<FONT COLOR="^s^">")
;;


let do_close_mods () =
   List.iter do_close_mod !cur_out.active ;
  !cur_out.active <- [] ;
  !cur_out.pending <- []
;;

let do_close_mods_pred pred =
  let rec split = function
    | [] -> [],[]
    | m :: rest ->
        let to_close,to_keep = split rest in
        match to_close with
        | [] -> if pred m then [m],to_keep else [], m::to_keep
        | _  -> m::to_close,to_keep in
  let to_close,to_keep = split !cur_out.active in
  List.iter do_close_mod to_close ;
  !cur_out.active <- to_keep ;
  List.fold_right
    (fun m r -> if pred m then r else m::r)
    to_close []
      
        
let close_mods () = do_close_mods ()
;;

let do_open_mods () =
  let rec do_rec = function
    [] -> ()
  | e :: rest ->
     do_rec rest ;
     do_open_mod e in
  do_rec !cur_out.pending ;
  !cur_out.active <- !cur_out.pending @ !cur_out.active ;
  !cur_out.pending <- []
;;


let do_pending () =  
  begin match flags.pending_par with
  | Some n -> flush_par n
  | _ -> ()
  end ;
  flags.last_closed <- "rien" ;
  do_open_mods ()
;;

let rec first_same x same_constr  = function
  [] -> false
| y::rest ->
    if same_constr y then
       x=y
    else first_same x same_constr rest
;;

let is_style = function
  Style _ -> true
| _ -> false

and is_font = function
  Font _ -> true
| _ -> false

and is_color = function
  Color _ -> true
| _ -> false
;;

let rec cur_size = function
  [] -> 3
| Font i::_ -> i
| _::rest -> cur_size rest
;;

let already_here = function
  Font i ->
   i = cur_size  ( !cur_out.pending @ !cur_out.active )
| x ->
  first_same x
   (match x with
     Style _ ->  is_style
   | Font _ -> is_font
   | Color _ -> is_color)
   ( !cur_out.pending @ !cur_out.active )
;;

let ok_pre = function _ ->  not !pedantic
;;

let rec filter_pre = function
  [] -> []
| e::rest ->
   if ok_pre e then e::filter_pre rest
   else filter_pre rest
;;

let ok_mod e =
  (not flags.in_pre || ok_pre e) && not (already_here e)
;;

let get_fontsize () =
  let rec do_rec = function
    (Font n)::_ -> n
  | _::rest -> do_rec rest
  | []              -> 3 in
  do_rec (!cur_out.pending @ !cur_out.active)
;;

let nostyle () =
  !cur_out.pending <- [] ;
  !cur_out.nostyle <- true    
;;

let clearstyle () =
  !cur_out.pending <- []
;;


let rec erase_rec pred = function
  [] -> []
| s::rest ->
   if pred s then erase_rec pred rest else s::erase_rec pred rest
;;

let erase_mods_pred pred =
  if not !cur_out.nostyle then begin
    let pending = erase_rec pred !cur_out.pending in
    let re_open = do_close_mods_pred pred in
    !cur_out.pending <- re_open @ pending
  end
;;

let erase_mods ms = erase_mods_pred (fun m -> List.mem m ms)
;;
let is_color = function
  Color _ -> true
| _       -> false
;;

let is_size = function
  | Font _ -> true
  | _      -> false

let open_mod  m =
  if not !cur_out.nostyle then begin
    if !verbose > 3 then
          prerr_endline ("open_mod: "^Latexmacros.pretty_env m^" ok="^sbool (ok_mod m)) ;
    if ok_mod m then begin
      begin match m with
        Color _ -> erase_mods_pred is_color
      | Font _  -> erase_mods_pred is_size
      | _ -> () end ;
      !cur_out.pending <- m :: !cur_out.pending
    end
  end
;;

let rec open_mods = function
  m::rest -> open_mods rest ; open_mod m
| []      -> ()
;;

(* Blocks *)

let pstart = function
  "H1" | "H2" | "H3" | "H4" | "H5" | "H6" -> true
| "PRE" -> true
| "DIV" -> true
| "BLOCKQUOTE" -> true
| "UL" | "OL" | "DL"-> true
| "TABLE" -> true
| _ -> false
;;


let rec try_open_block s args =
  if !verbose > 2 then
    prerr_flags ("=> try open ``"^s^"''");  
  if s = "DISPLAY" then begin
    try_open_block "TABLE" args ;
    try_open_block "TR" "VALIGN=middle" ;
  end else begin
    push empty_stack flags.empty ; push blank_stack flags.blank ;
    push insert_stack flags.insert ;
    flags.empty <- true ; flags.blank <- true ;
    flags.insert <- None ;
    if s = "TABLE" then begin
      push table_stack flags.table_vsize ;
      push vsize_stack flags.vsize ;
      push nrows_stack flags.nrows ;
      flags.table_vsize <- 0 ;
      flags.vsize <- 0 ;
      flags.nrows <- 0
    end else if s = "TR"  then begin
      flags.vsize <- 1
    end else if s = "TD" then begin
      push vsize_stack flags.vsize ;
      flags.vsize <- 1
    end else if is_list s then begin
      push nitems_stack flags.nitems;
      flags.nitems <- 0 ;
      if s = "DL" then begin
        push dt_stack flags.dt ;
        push dcount_stack flags.dcount;
        flags.dt <- "";
        flags.dcount <- ""
      end
    end
  end ;
  if !verbose > 2 then
    prerr_flags ("<= try open ``"^s^"''")
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

let do_do_open_block s args =
    if s = "TR" || s = "TABLE" || is_header s then
      do_put "\n";
    do_put_char '<' ;
    do_put s ;
    if args <> "" then begin
      do_put_char ' ' ;
      do_put args
    end ;
    do_put_char '>'

let rec do_open_block insert s args = match s with
|  ""|"DELAY"|"FORGET"|"AFTER" ->
   begin match insert with
   | Some (tag,iargs) -> do_do_open_block tag iargs
   | _ -> ()
   end
| "DISPLAY" ->
   do_open_block insert "TABLE" args ;
   do_open_block None "TR" "VALIGN=middle"
| _  -> begin match insert with
  | Some (tag,iargs) ->
      if is_list s || s = "TABLE" then begin
        do_do_open_block tag iargs ;
        do_do_open_block s args
      end else begin
        do_do_open_block s args ;
        do_do_open_block tag iargs
      end
  | _ -> do_do_open_block s args
end

let rec try_close_block s =
  if !verbose > 2 then
    prerr_flags ("=> try close ``"^s^"''") ;
  if s = "DISPLAY" then begin
    try_close_block "TR" ;
    try_close_block "TABLE"
  end else begin
    let ehere = flags.empty and ethere = pop "empty" empty_stack in
    flags.empty <- (ehere && ethere) ;
    let bhere = flags.blank and bthere = pop "blank" blank_stack in
    flags.blank <- (bhere && bthere) ;
    flags.insert <- pop "insert" insert_stack ;
    if s = "TABLE" then begin
      let p_vsize = pop "vsize" vsize_stack in
      flags.vsize <- max
       (flags.table_vsize + (if flags.nrows > 0 then flags.nrows/3 else 0)) p_vsize ;
      flags.nrows <- pop "nrows" nrows_stack ;
      flags.table_vsize <- pop "table" table_stack
    end else if s = "TR" then begin
      if ehere then begin
        flags.vsize <- 0
      end ;
      flags.table_vsize <- flags.table_vsize + flags.vsize;
      if not ehere then flags.nrows <- flags.nrows + 1
    end else if s = "TD" then begin
      let p_vsize = pop "vskip" vsize_stack in
      flags.vsize <- max p_vsize flags.vsize
    end else if is_list s then begin
      flags.nitems <- pop "nitems" nitems_stack;
      if s = "DL" then begin
        flags.dt <- pop "dt_stack" dt_stack ;
        flags.dcount <- pop "dcount_stack" dcount_stack
      end
    end
  end ;
  if !verbose > 2 then
    prerr_flags ("<= try close ``"^s^"''")
;;

let do_do_close_block s =
  do_put "</" ;
  do_put s ;
  do_put_char '>' ;
  match s with "TD" -> do_put_char '\n' | _ -> ()

let rec do_close_block insert s = match s with
|  ""|"DELAY"|"FORGET"|"AFTER" -> 
   begin match insert with
   | Some (tag,_) -> do_do_close_block tag
   | _ -> ()
   end
| "DISPLAY" ->
    do_close_block None "TR" ;
    do_close_block insert "TABLE"
| s  -> begin match insert with
  | Some (tag,_) ->
      if is_list s || s = "TABLE" then begin
        do_do_close_block s;
        do_do_close_block tag
      end else begin
        do_do_close_block tag;
        do_do_close_block s
      end
  | _ -> do_do_close_block s
end    


let pop_freeze () = match !out_stack with
  Freeze f::rest ->
    let _ = pop "out" out_stack in
    f,true
| _ -> (fun () -> ()),false
;;

let check_empty () = flags.empty
and make_empty () =
  flags.empty <- true ; flags.blank <- true ;
  !cur_out.pending <-  !cur_out.pending @ !cur_out.active ;
  !cur_out.active <- []
;;

let rec force_block s content =
  if !verbose > 2 then begin
    prerr_string ("force_block: "^s^" stack: ");
    pretty_stack !out_stack
  end ;
  let was_empty = flags.empty in
  if s = "FORGET" then begin
    make_empty () ;
  end else if flags.empty then begin
    flags.empty <- false; flags.blank <- false ;
    do_open_mods () ;
    do_put content
  end ;
  if s = "TABLE" || s="DISPLAY" then flags.table_inside <- true;
  if s = "PRE" then flags.in_pre <- false ;
  do_close_mods () ;
  let true_s =
    if s = "FORGET" then pblock() else s in
  let insert = flags.insert in
  try_close_block true_s ;
  do_close_block insert true_s ;
  let ps,args,pout = pop_out out_stack in  
  check_block_closed ps true_s ;
  let old_out = !cur_out in  
  cur_out := pout ;
  if s = "FORGET" then free old_out
  else if ps <> "DELAY" then begin
    let mods = !cur_out.active @ !cur_out.pending in
    do_close_mods () ;
    do_open_block insert s args ;
    if ps = "AFTER" then begin
      let f = pop "after" after_stack in
      Out.copy_fun f old_out.out !cur_out.out
    end else
      Out.copy old_out.out !cur_out.out ;
    free old_out ;    
    !cur_out.pending <- mods
  end else begin (* ps = "DELAY" *)
    raise (Misc.Fatal ("html: unflushed DELAY"))
  end ;
  if not was_empty && true_s <> "" && true_s <> "AFTER" then
    flags.last_closed <- true_s

    
and close_block_loc pred s =
  if !verbose > 2 then
    prerr_string ("close_block_loc: ``"^s^"'' = ");
  if not (pred ()) then begin
    if !verbose > 2 then prerr_endline "do it" ;
    force_block s "";
    true
  end else begin
    if !verbose > 2 then prerr_endline "forget it" ;
    force_block "FORGET" "";
    false
  end

and close_flow_loc s =
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

and close_flow s =
  if !verbose > 2 then
    prerr_flags ("=> close_flow ``"^s^"''");
  let _ = close_flow_loc s in
  if !verbose > 2 then
    prerr_flags ("<= close_flow ``"^s^"''")

and open_block s args =
 if !verbose > 2 then begin
   prerr_flags ("=> open_block ``"^s^"''");
 end ;
 try_flush_par ();
 if s = "PRE" then
    flags.in_pre <- true;
 let cur_mods = !cur_out.pending @ !cur_out.active  in
 push_out out_stack (s,args,!cur_out) ;
 cur_out :=
   new_status
   !cur_out.nostyle
   (if flags.in_pre then filter_pre cur_mods else cur_mods)
   [] ;
 try_open_block s args ;
 if !verbose > 2 then
   prerr_flags ("<= open_block ``"^s^"''")
;;

let insert_block tag arg =
  flags.insert <- Some (tag,arg)

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

let force_flow s content =
  let active = !cur_out.active and pending = !cur_out.pending in
  force_block s content ;
  !cur_out.pending <-  active @ pending
;;


let close_block  s =
  let _ = close_block_loc check_empty s in
  ()
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

let erase_block s =
  if !verbose > 2 then begin
    Printf.fprintf stderr "erase_block: %s" s;
    prerr_newline ()
  end ;
  try_close_block s ;
  let ts,_,tout = pop_out out_stack in
  if ts <> s then
    failclose ("erase_block: "^s^" closes "^ts);
  free !cur_out ;
  cur_out := tout
;;
   

let open_group ss =
  open_block "" "" ;
  let e = Style ss in
    !cur_out.pending <-
       (!cur_out.pending @
        (if ss = "" || (flags.in_pre && not (ok_pre e)) then []
        else [e]))

and open_aftergroup f =
  open_block "AFTER" "" ;
  push after_stack f

and close_group () = close_block ""
;;


(*----------*)
(* DISPLAYS *)
(*----------*)


let begin_item_display f is_freeze =
  if !verbose > 2 then begin
    Printf.fprintf stderr "begin_item_display: ncols=%d empty=%s" flags.ncols (sbool flags.empty) ;
    prerr_newline ()
  end ;
  open_block "TD" "nowrap";
  open_block "" "" ;
  if is_freeze then push out_stack (Freeze f) ;


and end_item_display () =
  let f,is_freeze = pop_freeze () in
  let _ = close_flow_loc "" in
  if close_flow_loc "TD" then
    flags.ncols <- flags.ncols + 1;
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
  erase_block "TD" ;
  erase_block "DISPLAY" ;
  try_close_display ()
;;


let change_block s args =
  erase_block s ;
  open_block s args
;;


(* output requests  *)
let is_blank = function
   ' ' | '\n' -> true
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
  do_put s ;
  if s_blank then flags.last_closed <- save_last_closed
;;

let put_char c =
  let save_last_closed = flags.last_closed in
  let c_blank = is_blank c in
  do_pending () ;
  flags.empty <- false;
  flags.blank <- c_blank && flags.blank ;
  do_put_char c ;
  if c_blank then flags.last_closed <- save_last_closed
;;

let flush_out () = 
  Out.flush !cur_out.out
;;

let skip_line () =
  flags.vsize <- flags.vsize + 1 ;
  put "<BR>"
;;

let set_dt s = flags.dt <- s
and set_dcount s = flags.dcount <- s
;;

let item scan arg =
  if !verbose > 2 then begin
    prerr_string "Item stack=" ;
    pretty_stack !out_stack
  end ;
  if not (is_list (pblock ())) then
    raise (Error "Item not inside a list element") ;

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
  if pblock() = "DL" then begin
    let parg = parg() in
    do_put "\n<DT>" ;    
    open_group "" ;
    if flags.dcount <> "" then scan ("\\refstepcounter{"^ flags.dcount^"}") ;
    if parg <> "" then
      true_scan ("\\makelabel{"^(if arg = "" then flags.dt else arg)^"}")
    else
      true_scan (if arg = "" then flags.dt else arg) ;
    close_group () ;
    do_put "<DD>"
  end else begin
    do_put "\n<LI>" ;
    true_scan arg
  end
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

let horizontal_line s t u =
  open_block "" "" ;
  nostyle () ;
  begin match t with
  | "0" -> put ("<HR "^s^">")
  | _ -> put ("<HR "^s^" SIZE="^t^">") end ;
  close_block ""
;;

let put_separator () =
  put "\n"
;;

let unskip () = Out.unskip !cur_out.out
  
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

let put_in_math s =
  if flags.in_pre && !pedantic then
    put s
  else begin
    put "<I>";
    put s;
    put "</I>"
  end
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

let open_cell format span = open_block "TD" (as_align format span)
;;

let erase_cell () =  erase_block "TD"
and close_cell content =  force_block "TD" content
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
let make_border c = ()
;;

let center_format =
  Tabular.Align  {Tabular.hor="center" ; Tabular.vert = "" ;
		   Tabular.wrap = false ; Tabular.pre = "" ; 
		   Tabular.post = "" ; Tabular.width = None} 
;;

let make_hline w noborder =
  if noborder then begin
    new_row ();
    open_cell center_format w;
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

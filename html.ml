
(* Output function for a strange html model :
     - Text elements can occur anywhere and are given as in latex
     - A new grouping construct is given (open_group () ; close_group ())
     - Paragraph <P> ... </P> are properly closed automatically, when a non
   text element is introduced in then. They are reopened when this element is
   over.
*)

open Parse_opts
open Latexmacros

type 'a ok  = No | Yes of 'a
;;

(* Saving mods accross blocks *)
let push s e = s := e:: !s
and pop s = match !s with
  [] -> failwith "Empty stack"
| e::rs -> s := rs ; e
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

let vsize = ref 0
and vsize_stack = ref []
;;

let nrows = ref 0
and nrows_stack = ref []
;;

let delay_stack = ref []
;;

let nitems = ref 0
and nitems_stack = ref []
;;

let dt = ref ""
and dt_stack = ref []
;;

let dcount = ref ""
and dcount_stack = ref []

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

let rec pop_out s = match pop s with
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
    let _ = pop out_stack in
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

let table_vsize = ref 0
and table_stack = ref []
;;

let empty = ref true
and empty_stack = ref []
;;

let pending_par = ref false
and par_stack = ref []
;;

let is_header s =
  String.length s = 2 && String.get s 0 = 'H'
;;

let is_list = function
  "UL" | "DL" | "OL" -> true
| _ -> false
;;

let last_closed = ref "rien"
;;

let flush_par () =
  last_closed := "rien" ;
  pending_par := false ;
  do_put "<BR><BR>\n" ;
  vsize := !vsize + 2
;;

let forget_par () =
  pending_par := false
;;

let no_par s =
  is_header s || is_list s ||
  (match s with "PRE" -> true | _ -> false)
;;

let par () =
  if not (no_par !last_closed) then pending_par := true
;;

let debug m =
  Printf.fprintf stderr "%s : table_vsize=%d vsize=%d" m !table_vsize !vsize ;
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
  if !pending_par then begin
    flush_par ()
  end ;
  last_closed := "rien" ;
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

let in_pre = ref false
;;

let ok_pre = function
  Style "BIG"| Style "SMALL"| Style "SUP"| Style "SUB" -> false
| Style _ -> false
| Font _ -> false
| _      -> true
;;

let rec filter_pre = function
  [] -> []
| e::rest ->
   if ok_pre e then e::filter_pre rest
   else filter_pre rest
;;

let ok_mod e =
  (not !in_pre || ok_pre e) && not (already_here e)
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

let open_mod  m =
  if not !cur_out.nostyle then begin
    if !verbose > 3 then
          prerr_endline ("open_mod: "^Latexmacros.pretty_env m) ;
    if ok_mod m then
      !cur_out.pending <- m :: !cur_out.pending
  end
;;


let rec erase_rec ms = function
  [] -> []
| s::rest ->
   if List.mem s ms then erase_rec ms rest else s::erase_rec ms rest
;;

let erase_mods ms =
  if not !cur_out.nostyle then begin
    let pending = erase_rec ms !cur_out.pending in
    if List.exists (fun s -> List.mem s ms) !cur_out.active then begin
      let active = erase_rec ms !cur_out.active in
      do_close_mods () ;
      !cur_out.pending <- active @ pending
    end else
      !cur_out.pending <- pending
  end
;;

let rec open_mods = function
  m::rest -> open_mods rest ; open_mod m
| []      -> ()
;;

(* Blocks *)

let sbool = function true -> "+" | _ -> "-"
;;

let pstart = function
  "H1" | "H2" | "H3" | "H4" | "H5" | "H6" -> true
| "PRE" -> true
| "DIV" -> true
| "BLOCKQUOTE" -> true
| "UL" | "OL" | "DL"-> true
| _ -> false
;;


let rec try_open_block s args =
  if !verbose > 2 then
    prerr_endline ("try in "^s^" : "^sbool !empty);  
  if s = "DISPLAY" then begin
    try_open_block "TABLE" args ;
    try_open_block "TR" ""
  end else begin
    push empty_stack !empty ;
    if pstart s then pending_par := false ;
    push par_stack !pending_par ;    
    empty := true ;
    pending_par := false ;
    if s = "TABLE" then begin
      push table_stack !table_vsize ;
      push vsize_stack !vsize ;
      push nrows_stack !nrows ;
      table_vsize := 0 ;
      vsize := 0 ;
      nrows := 0
    end else if s = "TR"  then begin
      vsize := 1
    end else if s = "TD" then begin
      push vsize_stack !vsize ;
      vsize := 1
    end else if is_list s then begin
      push nitems_stack !nitems;
      nitems := 0 ;
      if s = "DL" then begin
        push dt_stack !dt ;
        push dcount_stack !dcount;
        dt := "";
        dcount := ""
      end
    end
  end
;;

let rec do_open_block s args = match s with
  ""|"DELAY"|"FORGET" -> ()
| "DISPLAY" ->
   do_open_block "TABLE" args ;
   do_open_block "TR" ""
| _  ->
    if s = "TR" || s = "TABLE" || is_header s then
      do_put "\n";
    do_put_char '<' ;
    do_put s ;
    if args <> "" then begin
      do_put_char ' ' ;
      do_put args
    end ;
    do_put_char '>'
;;

let rec try_close_block s =
  if !verbose > 2 then
    prerr_string ("try out "^s^" : "^sbool !empty);
  if s = "DISPLAY" then begin
    try_close_block "TR" ;
    try_close_block "TABLE"
  end else begin
    let here = !empty in
    empty := here && pop empty_stack ;
    pending_par := pop par_stack ;
    if !verbose > 2 then
      prerr_string (" -> "^sbool !empty);
    if s = "TABLE" then begin
      let p_vsize = pop vsize_stack in
      vsize := max
       (!table_vsize + (if !nrows > 0 then !nrows/3 else 0)) p_vsize ;
      nrows := pop nrows_stack ;
      table_vsize := pop table_stack
    end else if s = "TR" then begin
      table_vsize := !table_vsize + !vsize;
      nrows := !nrows + 1
    end else if s = "TD" then begin
      let p_vsize = pop vsize_stack in
      vsize := max p_vsize !vsize
    end else if is_list s then begin
      nitems := pop nitems_stack
    end
  end ;
  if !verbose > 2 then
    prerr_endline
      (" nrows="^string_of_int !nrows^" vsize="^string_of_int !vsize)
;;

let rec do_close_block s = match s with
  ""|"DELAY"|"FORGET" -> ()
| "DISPLAY" ->
    do_close_block "TR" ;
    do_close_block "TABLE"
| s  ->
  do_put "</" ;
  do_put s ;
  do_put_char '>' ;
  do_put_char '\n' ;
  if s = "DIV" || s = "PRE" then
    do_put_char '\n' ;
;;


let pop_freeze () = match !out_stack with
  Freeze f::rest ->
    let _ = pop out_stack in
    f,true
| _ -> (fun () -> ()),false
;;


let rec force_block s content =
  if !verbose > 2 then begin
    prerr_string ("force_block: "^s^" stack: ");
    pretty_stack !out_stack
  end ;
  if !empty then begin
    if s <> "FORGET" then begin
      empty := false;
      Out.reset !cur_out.out ;
      do_open_mods () ;
      do_put content
    end
  end ;
  let was_empty = !empty in
  if s = "PRE" then in_pre := false ;
  do_close_mods () ;
  let true_s = if s = "FORGET" then pblock() else s in
  try_close_block true_s ;
  do_close_block true_s ;
  let ps,args,pout = pop_out out_stack in  
  if ps <> true_s then
    failwith ("hml: "^true_s^" closes "^ps) ;
  let old_out = !cur_out in  
  cur_out := pout ;
  if s = "FORGET" then free old_out
  else if ps <> "DELAY" then begin
    let mods = !cur_out.active @ !cur_out.pending in
    do_close_mods () ;
    do_open_block s args ;
    Out.copy old_out.out !cur_out.out ;
(*
    prerr_endline ("Buff status: "^s) ;
    Out.debug stderr !cur_out.out ;
*)
    free old_out ;    
    !cur_out.pending <- mods
  end else begin (* s = "DELAY" *)
    let f = pop delay_stack in
    let x = !vsize in
    f x ; vsize := x ;    
    let mods = !cur_out.active @ !cur_out.pending in
    do_close_mods () ;
    Out.copy old_out.out !cur_out.out ;
    free old_out ;
    !cur_out.pending <- mods
  end ;
  if not was_empty && true_s <> ""  then last_closed := true_s

and close_block_loc pred s =
  if !verbose > 2 then
    prerr_string ("close_block_loc: "^s^" = ");
  if not (pred ()) then begin
    if !verbose > 2 then prerr_endline "+" ;
    force_block s "";
    true
  end else begin
    if !verbose > 2 then prerr_endline "-" ;
    force_block "FORGET" "";
    false
  end

and close_flow_loc s =
  if !verbose > 2 then
    prerr_endline ("close_flow_loc: "^s) ;

  let active  = !cur_out.active
  and pending = !cur_out.pending in
  if close_block_loc (fun () -> !empty) s then begin
    !cur_out.pending <- active @ pending ;
    true
  end else begin
    !cur_out.pending <- active @ pending ;
    false
  end

and close_flow s =
  if !verbose > 2 then
    prerr_endline ("close_flow: "^s) ;
  let _ = close_flow_loc s in ()

and open_block s args =
 if !verbose > 2 then begin
   prerr_string ("open_block: "^s^" "^args^" stack=") ;
   pretty_stack !out_stack
 end ;
 if !pending_par && s = "" then
   flush_par ();
 if s = "PRE" then
    in_pre := true;
 let cur_mods = !cur_out.active @ !cur_out.pending in
 push_out out_stack (s,args,!cur_out) ;
 cur_out :=
   new_status
   !cur_out.nostyle
   (if !in_pre then filter_pre cur_mods else cur_mods)
   [] ;
 try_open_block s args

;;

let force_flow s content =
  let active = !cur_out.active and pending = !cur_out.pending in
  force_block s content ;
  !cur_out.pending <-  active @ pending
;;


let close_block  s =
  let _ = close_block_loc (fun () -> !empty) s in
  ()
;;



let ncols = ref 0
and ncols_stack = ref []
;;

let open_display args =
  if !verbose > 2 then begin
    Printf.fprintf stderr "open_display: %s -> " args
  end ;
  push ncols_stack !ncols ;
  ncols := 0;
  open_block "DISPLAY" args ;
  open_block "TD" "nowrap" ;
  if !verbose > 2 then begin
    pretty_cur !cur_out ;
    prerr_endline ""
  end     
;;

let close_display () =
  if !verbose > 2 then begin
    Printf.fprintf stderr "close_display: ncols=%d stack=" !ncols;
    pretty_stack !out_stack
  end ;
  if not (flush_freeze ()) then begin
    let n = !ncols in
    ncols := pop ncols_stack ;
    if n = 0 && not !empty then begin
      let active = !cur_out.active and pending = !cur_out.pending in
      do_close_mods () ;
      let ps,_,pout = pop_out out_stack in
      if ps <> "TD" then
        failwith ("close_display: "^ps^" closes TD") ;
      do_close_mods () ;
      try_close_block "TD" ;
      let ps,_,ppout = pop_out out_stack in
      if ps <> "DISPLAY" then
        failwith ("close_display: "^ps^" closes DISPLAY") ;
      try_close_block "DISPLAY" ;
      let old_out = !cur_out in
      cur_out := ppout ;
      do_close_mods () ;
      Out.copy old_out.out !cur_out.out ;
      free old_out ; free pout ;
      !cur_out.pending <- active @ pending
    end else begin
      close_block "TD" ;
      close_block "DISPLAY" 
    end
  end
;;

  
  
let item_display () =
  if !verbose > 2 then begin
    Printf.fprintf stderr "item_display: ncols=%d cur: " !ncols;
    pretty_cur !cur_out ;
    prerr_string " stack=" ;
    pretty_stack !out_stack
  end ;
  let f,is_freeze = pop_freeze () in
  if close_flow_loc "TD" then
    ncols := !ncols + 1;
  open_block "TD" "nowrap" ;
  if is_freeze then push out_stack (Freeze f) ;
  if !verbose > 2 then begin
    prerr_string "item display -> stack=" ;
    pretty_stack !out_stack
  end ;

and begin_item_display () =
  if !verbose > 2 then begin
    Printf.fprintf stderr "begin item_display: ncols=%d" !ncols;
    prerr_newline ()
  end ;
  open_block "TD" "nowrap"

and end_item_display () =
  if close_flow_loc "TD" then
    ncols := !ncols + 1;
  if !verbose > 2 then begin
    Printf.fprintf stderr "begin item_display: ncols=%d" !ncols;
    prerr_newline ()
  end
;;

let open_group ss =
  open_block "" "" ;
  !cur_out.pending <-
    (!cur_out.pending @ (if ss = "" then [] else [Style ss]))

and close_group () = close_block ""
;;

let erase_block s =
  if !verbose > 2 then begin
    Printf.fprintf stderr "erase_block: %s" s;
    prerr_newline ()
  end ;
  try_close_block s ;
  let ts,_,tout = pop_out out_stack in
  if ts <> s then
    failwith ("erase_block: "^s^" closes "^ts);
  free !cur_out ;
  cur_out := tout
;;
       
let erase_display () =
  erase_block "TD" ;
  erase_block "DISPLAY"
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
  let blank =
    let r = ref true in
    for i = 0 to String.length s - 1 do
      r := !r && is_blank (String.get s i)
    done ;
    !r in
  let save_last_closed = !last_closed in
  do_pending () ;
  let i = ref 0 in
  empty := !empty && blank ;
  do_put s ;
  if blank then last_closed := save_last_closed
;;

let put_char c =
  let save_last_closed = !last_closed in
  let blank = is_blank c in
  do_pending () ;
  empty := !empty && is_blank c ;
  do_put_char c ;
  if blank then last_closed := save_last_closed
;;

let flush_out () = 
  Out.flush !cur_out.out
;;

let skip_line () =
  vsize := !vsize + 1 ;
  put "<BR>\n"
;;

let set_dt s = dt := s
and set_dcount s = dcount := s
;;

let item scan arg =
  if !verbose > 2 then begin
    prerr_string "Item stack=" ;
    pretty_stack !out_stack
  end ;
  if !pending_par then
    if !nitems > 0 then 
      flush_par ()
    else pending_par := false;
  let mods = !cur_out.pending @ !cur_out.active in
  do_close_mods () ;
  let true_scan =
    if !nitems = 0 then
      let saved = Out.to_string !cur_out.out in
      (fun arg -> do_put saved ; scan arg)
    else scan in
  !cur_out.pending <- mods ;
  nitems := !nitems+1;
  if pblock() = "DL" then begin
    let parg = parg() in
    do_put "\n<DT>" ;    
    open_group "" ;
    if !dcount <> "" then scan ("\\refstepcounter{"^ !dcount^"}") ;
    if parg <> "" then
      true_scan ("\\makelabel{"^(if arg = "" then !dt else arg)^"}")
    else
      true_scan (if arg = "" then !dt else arg) ;
    close_group () ;
    do_put "<DD>"
  end else begin
    do_put "\n<LI>" ;
    true_scan arg
  end
;;


(* delaying output .... *)

let delay f =
  push delay_stack f ;
  open_block "DELAY" ""
;;

let flush () =
  force_block "DELAY" "";
  !vsize
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
  put "<A NAME=\"" ;
  put s1 ;
  put "\">" ;
  put s2 ;
  put "</A>"
;;

let insert_vdisplay open_fun =
  if !verbose > 2 then begin
    prerr_string "insert_vdisplay: stack=" ;
    pretty_stack !out_stack
  end ;
  try
    let mods = !cur_out.pending @ !cur_out.active in
    let ps,pargs,pout = pop_out out_stack in
    if ps <> "TD" then
      failwith ("insert_vdisplay: "^ps^" close TD");
    let pps,ppargs,ppout = pop_out out_stack  in
    if pps <> "DISPLAY" then
      failwith ("insert_vdisplay: "^pps^" close DISPLAY");
    let new_out = new_status false [] [] in
    push_out out_stack (pps,ppargs,new_out) ;
    push_out out_stack (ps,pargs,pout) ;
    close_display () ;
    cur_out := ppout ;
    open_fun () ;
    do_put (Out.to_string new_out.out) ; empty := false ;
    free new_out ;    
    if !verbose > 2 then begin
      prerr_string "insert_vdisplay -> " ;
      pretty_mods mods ;
      prerr_newline ()
    end ;
    mods
  with PopFreeze ->
    failwith "\\over should be properly parenthesized"
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
  open_group "" ;
  f () ;
  let r = Out.to_string !cur_out.out in
  close_group () ;
  r
;;

let finalize () =
  if !out_stack != [] then begin
    prerr_string "Non empty stack in Html.finalize" ;
    pretty_stack !out_stack ;
    prerr_endline ""
  end ;
  Out.close !cur_out.out
;;



(* Output function for a strange html model :
     - Text elements can occur anywhere and are given as in latex
     - A new grouping construct is given (open_group () ; close_group ())
     - Paragraph <P> ... </P> are properly closed automatically, when a non
   text element is introduced in then. They are reopened when this element is
   over.
*)

open Latexmacros
;;

let verbose = ref 0
;;
(* Saving mods accross blocks *)
let push s e = s := e:: !s
and pop s = match !s with
  [] -> failwith "Empty stack"
| e::rs -> s := rs ; e
;;

(* output globals *)
type status = {
  mutable pending : env list ;
  mutable active : env list ;
  mutable out : Out.t}
;;

(* free list for buffers *)
let free_list = ref []
;;

let free out =
  out.pending <- [] ;
  out.active <- [] ;
  Out.reset out.out ;
  free_list := out :: !free_list
;;



let new_status pending active = match !free_list with
  [] ->
   {pending = pending  ; active = active ; out = Out.create_buff ()}
| x::rest ->
   free_list := rest ;
   x.pending <- pending ;
   x.active <- active ;
   x
;;

let cur_out = ref {pending = [] ; active = [] ; out = Out.create_null ()}
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



type stack_item =
  Normal of string * string * status
| Par of string * (string) list
;;

exception PopFreeze
;;

let push_out s (a,b,c) = push s (Normal (a,b,c))
and pop_out s = match pop s with
  Normal (a,b,c) -> a,b,c
| _              -> raise PopFreeze
;;

  
let out_stack = ref []
;;

let pretty_stack s =
  prerr_string "|" ;
  List.iter
   (function Normal (s,args,_) ->
     prerr_string ("["^s^"]-{"^args^"} ")
   | _   -> prerr_string "Freeze ") s ;
  prerr_endline "|"
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

let already_here x =
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

let rm_erases = function
 Style "I" | Style "B" | Style "TT"
  | Style "EM" | Style "STRONG" | Style "CITE" -> true
| _ -> false
;;
     
let rec perform_rm = function
  [] -> []
| x::rest ->
    if rm_erases x then perform_rm rest
    else x::perform_rm rest
;;

     
let open_mod = function
  Style "RM" ->
    let pending = perform_rm !cur_out.pending in
    if List.exists rm_erases  !cur_out.active then begin
      let active = perform_rm !cur_out.active in
      do_close_mods () ;
      !cur_out.pending <- active @ pending
    end else
      !cur_out.pending <- pending
| e ->
    if !verbose > 3 then
          prerr_endline ("open_mod: "^Latexmacros.pretty_env e) ;
    if ok_mod e then
      !cur_out.pending <- e :: !cur_out.pending
;;


(* Blocks *)

let sbool = function true -> "+" | _ -> "-"
;;

let rec try_open_block s args =
  if !verbose > 1 then
    prerr_endline ("try in "^s^" : "^sbool !empty);
  if s = "DISPLAY" then begin
    try_open_block "TABLE" args ;
    try_open_block "TR" ""
  end else begin
    push empty_stack !empty ;
    empty := true ;
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
    end
  end
;;

let is_header s =
  String.length s = 2 && String.get s 0 = 'H'
;;

let rec do_open_block s args = match s with
  ""|"DELAY"|"FORGET" -> ()
| "DISPLAY" ->
   do_open_block "TABLE" args ;
   do_open_block "TR" ""
| _  ->
    if s = "TR" || s = "TABLE" || is_header s then
      do_put_char '\n';
    do_put_char '<' ;
    do_put s ;
    if args <> "" then begin
      do_put_char ' ' ;
      do_put args
    end ;
    do_put_char '>'
;;

let rec try_close_block s =
  if !verbose > 1 then
    prerr_string ("try out "^s^" : "^sbool !empty);
  if s = "DISPLAY" then begin
    try_close_block "TR" ;
    try_close_block "TABLE"
  end else begin
    let here = !empty in
    empty := here && pop empty_stack ;
    if !verbose > 1 then
      prerr_endline (" -> "^sbool !empty);
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
    end
  end
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
  if s = "P" || s = "DIV" || s = "PRE" then
    do_put_char '\n' ;
;;

let pblock () = match !out_stack with
  Normal (s,_,_)::_ -> s
| []                -> ""
| _ -> raise PopFreeze
;;


let pstart = function
  "H1" | "H2" | "H3" | "H4" | "H5" | "H6" -> true
| "PRE" -> true
| "DIV" -> true
| "BLOCKQUOTE" -> true
| _ -> false
;;


let rec force_block s content =
  if !verbose > 1 then begin
    prerr_string ("force_block: "^s^" stack: ");
    pretty_stack !out_stack
  end ;
  if pblock () = "P" && (s <> "P" && s <> "FORGET") then close_par () ;
  if !empty then begin
    empty := s = "FORGET" ;
    Out.reset !cur_out.out ;
    do_open_mods () ;
    do_put content
  end ;
  if s = "PRE" then in_pre := false ;
  do_close_mods () ;  
  try_close_block s ;
  do_close_block s ;
  let vsize = !vsize in
  let ps,args,pout = pop_out out_stack in  
  if ps <> s && s <> "FORGET" then
    failwith ("hml: "^s^" closes "^ps) ;
  let old_out = !cur_out in  
  cur_out := pout ;
  if ps <> "DELAY" then begin
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
  end else if s = "DELAY" then begin
    let f = pop delay_stack in
    f vsize ; 
    let mods = !cur_out.active @ !cur_out.pending in
    do_close_mods () ;
    Out.copy old_out.out !cur_out.out ;
    free old_out ;
    !cur_out.pending <- mods
  end else begin (* s = "FORGET" *)
    free old_out
  end ;
  if pstart s then
    open_par ()


and close_block_loc pred s =
  if !verbose > 1 then
    prerr_string ("close_block_loc: "^s^" = ");
  if not (pred ()) then begin
    if !verbose > 1 then prerr_endline "+" ;
    force_block s "";
    true
  end else begin
    if !verbose > 1 then prerr_endline "-" ;
    force_block "FORGET" "";
    false
  end

and close_flow_loc s =
  if !verbose > 1 then
    prerr_endline ("close_flow_loc: "^s) ;

  let active  = !cur_out.active
  and pending = !cur_out.pending in
  if close_block_loc (fun () -> !empty) s then begin
    !cur_out.pending <- active @ pending ;
    true
  end else begin
    !cur_out.pending <- pending ;
    false
  end

and close_flow s =
  if !verbose > 1 then
    prerr_endline ("close_flow: "^s) ;
  let _ = close_flow_loc s in ()

and open_block s args =
 if !verbose > 1 then begin
   prerr_string ("open_block: "^s^" "^args^" stack=") ;
   pretty_stack !out_stack
 end ;
 if pblock () = "P" && s <> "" then close_par () ;
 if s = "PRE" then
    in_pre := true;
 let cur_mods = !cur_out.active @ !cur_out.pending in
 push_out out_stack (s,args,!cur_out) ;
 cur_out :=
   new_status
   (if !in_pre then filter_pre cur_mods else cur_mods)
   [] ;
 try_open_block s args
(*
and close_fullpar () =
  let rec test = function
    (Normal ("",_,_) as x)::rest -> begin match test rest with
      Yes l -> Yes (x::l)
    | _ -> No end
  | Normal ("P",_,_) as x)::_   -> 
  | _              -> No in

  if test !out_stack then begin
    let styles =
      (List.map
        (fun (,args,{pending=p ; active=a}) ->
          b,args,a@p)
        !out_stack ;
    while pblock() = "" do
      close_block ""
    done ;
    close_flow "P"
  end else
    ()
*)
and open_par () =
  if !verbose > 1 then begin
    prerr_string "open_par stack=" ;
    pretty_stack !out_stack
  end ;
  open_block "P" ""
and close_par () =
  if !verbose > 1 then begin
    prerr_string "close_par stack=" ;
    pretty_stack !out_stack
  end ;
  close_flow "P"
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
  if !verbose > 1 then begin
    Printf.fprintf stderr "open_display: %s" args;
    prerr_newline ()
  end ;
  push ncols_stack !ncols ;
  ncols := 0;
  open_block "DISPLAY" args ;
  open_block "TD" "" ;
;;

let close_display () =
  if !verbose > 1 then begin
    Printf.fprintf stderr "close_display: ncols=%d" !ncols;
    prerr_newline ()
  end ;
  let n = !ncols in
  ncols := pop ncols_stack ;
  if n = 0 && not !empty then begin
    do_close_mods () ;
    let active = !cur_out.active and pending = !cur_out.pending in
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
;;

let item_display () =
  if !verbose > 1 then begin
    Printf.fprintf stderr "item_display: ncols=%d" !ncols;
    prerr_newline ()
  end ;
  if close_flow_loc "TD" then
    ncols := !ncols + 1;
  open_block "TD" ""

and begin_item_display () =
  if !verbose > 1 then begin
    Printf.fprintf stderr "begin item_display: ncols=%d" !ncols;
    prerr_newline ()
  end ;
  open_block "TD" ""

and end_item_display () =
  if close_flow_loc "TD" then
    ncols := !ncols + 1;
  if !verbose > 1 then begin
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
  if !verbose > 1 then begin
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
  do_pending () ;
  let i = ref 0 in
  while !empty && !i < String.length s do
    empty := is_blank (String.get s !i) ;
    i := !i + 1
  done ;
  do_put s
;;

let put_char c =
  do_pending () ;
  empty := !empty && is_blank c ;
  do_put_char c
;;

let par () =
  if pblock () = "P" then begin
    close_par () ; open_par ()
  end else begin
   prerr_endline "Warning: bad par"  ;
   pretty_stack !out_stack ;
   put "\n<BR>\n"
  end
;;

let item f =
  let mods = !cur_out.pending @ !cur_out.active in
  do_close_mods () ;
  !cur_out.pending <- mods ;
  if pblock() = "DL" then begin
    do_put "\n<DT>" ;
    f () ;
    do_put "<DD>"
  end else begin
    do_put "\n<LI>" ;
    f () ;
  end
;;

let skip_line () =
  vsize := !vsize + 1 ;
  put "<BR>\n"
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



let forget () = force_block "FORGET" ""
;;

let loc_ref s =
  put "<A HREF=\"#" ;
  put s ;
  put "\">" ;
  put s ;
  put "</A>"
;;

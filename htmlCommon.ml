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

let header = "$Id: htmlCommon.ml,v 1.20 2000-05-03 17:50:27 maranget Exp $" 

(* Output function for a strange html model :
     - Text elements can occur anywhere and are given as in latex
     - A new grouping construct is given (open_group () ; close_group ())
*)

open Misc
open Parse_opts
open Latexmacros
open Stack
open Length


let failclose s = raise (Misc.Close s)
;;

let check_block_closed opentag closetag =
  if opentag <> closetag && not (opentag = "AFTER" && closetag = "") then
    failclose ("html: ``"^closetag^"'' closes ``"^opentag^"''") 
;;

(* output globals *)

type t_env = {here : bool ; env : env}

type status = {
  mutable nostyle : bool ;
  mutable pending : env list ;
  mutable active : t_env list ;
  mutable out : Out.t}
;;

let as_env  {env=env} =  env
let as_envs tenvs r  =
    List.fold_right (fun x r -> as_env x::r) tenvs r

let to_pending pending active = pending @ as_envs active []

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

let cur_out =
  ref {nostyle=false ;
        pending = [] ; active = [] ; out = Out.create_null ()}
;;


let do_pretty_mods f mods =
  let rec do_rec = function
    [x]  -> prerr_string (f x)
  | x::xs ->
     prerr_string (f x^"; ") ;
     do_rec xs
  | [] -> () in
  prerr_string "[" ;
  do_rec mods ;
  prerr_string "]"

let tbool = function
  | true -> "+"
  | false -> "-"

let pretty_mods = do_pretty_mods Latexmacros.pretty_env 
and pretty_tmods = 
  do_pretty_mods
    (function {here=here ; env = env} ->
      tbool here^Latexmacros.pretty_env env)
     
let pretty_cur {pending = pending ; active = active} =
  prerr_string "pending = " ;
  pretty_mods pending ;
  prerr_string " active = " ;
  pretty_tmods active
;;

type stack_item =
  Normal of string * string * status
| Freeze of (unit -> unit)
;;

exception PopFreeze
;;

let push_out s (a,b,c) = push s (Normal (a,b,c))
;;

let pretty_stack s = Stack.pretty 
   (function Normal (s,args,_) -> "["^s^"]-{"^args^"} "
   | Freeze _   -> "Freeze ") s
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


let out_stack = Stack.create "out_stack"
;;

type saved_out = status * stack_item Stack.saved

let save_out () = !cur_out, Stack.save out_stack
and restore_out (a,b) =
  if !cur_out != a then begin
    free !cur_out ;
    Stack.finalize out_stack
      (function
        | Normal (_,_,x) -> x == a
        | _ -> false)
      (function
        | Normal (_,_,out) -> free out
        | _ -> ())
  end ;  
  cur_out := a ;
  Stack.restore out_stack b

let pblock () =
  if Stack.empty out_stack then ""
  else
    match Stack.top out_stack with
    | Normal (s,_,_) -> s
    | _ -> ""
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
    mutable in_math : bool;
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
  in_math = false;
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
  in_math = in_math;
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
  in_math = in_math;
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
  in_math = in_math;
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
  f.in_math <- in_math;
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


    
(* Independant stacks for flags *)
type stack_t = {
  s_table_inside : bool Stack.t ;
  s_saved_inside : bool Stack.t ;
  s_in_math : bool Stack.t ;
  s_ncols : int Stack.t ;
  s_empty : bool Stack.t ;
  s_blank : bool Stack.t ;
  s_pending_par : int option Stack.t ;
  s_vsize : int Stack.t ;
  s_nrows : int Stack.t ;
  s_table_vsize : int Stack.t ;
  s_nitems : int Stack.t ;
  s_dt : string Stack.t ;
  s_dcount : string Stack.t ;
  s_insert : (string * string) option Stack.t ;
(* Other stacks, not corresponding to flags *)
  s_active : Out.t Stack.t ;
  s_after : (string -> string) Stack.t
  } 

let stacks = {
  s_table_inside = Stack.create "inside" ;
  s_saved_inside = Stack.create "saved_inside" ;
  s_in_math = Stack.create "in_math" ;
  s_ncols = Stack.create "ncols" ;
  s_empty = Stack.create "empty" ;
  s_blank = Stack.create "blank" ;
  s_pending_par = Stack.create "pending_par" ;
  s_vsize = Stack.create "vsize" ;
  s_nrows = Stack.create "nrows" ;
  s_table_vsize = Stack.create "table_vsize" ;
  s_nitems = Stack.create "nitems" ;
  s_dt = Stack.create "dt" ;
  s_dcount = Stack.create "dcount" ;
  s_insert = Stack.create "insert" ;
  s_active = Stack.create "active" ;
  s_after = Stack.create "after"
} 

type saved_stacks = {
  ss_table_inside : bool Stack.saved ;
  ss_saved_inside : bool Stack.saved ;
  ss_in_math : bool Stack.saved ;
  ss_ncols : int Stack.saved ;
  ss_empty : bool Stack.saved ;
  ss_blank : bool Stack.saved ;
  ss_pending_par : int option Stack.saved ;
  ss_vsize : int Stack.saved ;
  ss_nrows : int Stack.saved ;
  ss_table_vsize : int Stack.saved ;
  ss_nitems : int Stack.saved ;
  ss_dt : string Stack.saved ;
  ss_dcount : string Stack.saved ;
  ss_insert : (string * string) option Stack.saved ;
(* Other stacks, not corresponding to flags *)
  ss_active : Out.t Stack.saved ;
  ss_after : (string -> string) Stack.saved
  } 

let save_stacks () =
{
  ss_table_inside = Stack.save stacks.s_table_inside ;
  ss_saved_inside = Stack.save stacks.s_saved_inside ;
  ss_in_math = Stack.save stacks.s_in_math ;
  ss_ncols = Stack.save stacks.s_ncols ;
  ss_empty = Stack.save stacks.s_empty ;
  ss_blank = Stack.save stacks.s_blank ;
  ss_pending_par = Stack.save stacks.s_pending_par ;
  ss_vsize = Stack.save stacks.s_vsize ;
  ss_nrows = Stack.save stacks.s_nrows ;
  ss_table_vsize = Stack.save stacks.s_table_vsize ;
  ss_nitems = Stack.save stacks.s_nitems ;
  ss_dt = Stack.save stacks.s_dt ;
  ss_dcount = Stack.save stacks.s_dcount ;
  ss_insert = Stack.save stacks.s_insert ;
  ss_active = Stack.save stacks.s_active ;
  ss_after = Stack.save stacks.s_after
}   

and restore_stacks
{
  ss_table_inside = saved_table_inside ;
  ss_saved_inside = saved_saved_inside ;
  ss_in_math = saved_in_math ;
  ss_ncols = saved_ncols ;
  ss_empty = saved_empty ;
  ss_blank = saved_blank ;
  ss_pending_par = saved_pending_par ;
  ss_vsize = saved_vsize ;
  ss_nrows = saved_nrows ;
  ss_table_vsize = saved_table_vsize ;
  ss_nitems = saved_nitems ;
  ss_dt = saved_dt ;
  ss_dcount = saved_dcount ;
  ss_insert = saved_insert ;
  ss_active = saved_active ;
  ss_after = saved_after
}   =
  Stack.restore stacks.s_table_inside saved_table_inside ;
  Stack.restore stacks.s_saved_inside saved_saved_inside ;
  Stack.restore stacks.s_in_math saved_in_math ;
  Stack.restore stacks.s_ncols saved_ncols ;
  Stack.restore stacks.s_empty saved_empty ;
  Stack.restore stacks.s_blank saved_blank ;
  Stack.restore stacks.s_pending_par saved_pending_par ;
  Stack.restore stacks.s_vsize saved_vsize ;
  Stack.restore stacks.s_nrows saved_nrows ;
  Stack.restore stacks.s_table_vsize saved_table_vsize ;
  Stack.restore stacks.s_nitems saved_nitems ;
  Stack.restore stacks.s_dt saved_dt ;
  Stack.restore stacks.s_dcount saved_dcount ;
  Stack.restore stacks.s_insert saved_insert ;
  Stack.restore stacks.s_active saved_active ;
  Stack.restore stacks.s_after saved_after


let check_stack what =
  if not (Stack.empty what)  && not !silent then begin
    prerr_endline
      ("Warning: stack "^Stack.name what^" is non-empty in Html.finalize") ;
  end
;;

let check_stacks () = match stacks with
{
  s_table_inside = s_table_inside ;
  s_saved_inside = s_saved_inside ;
  s_in_math = s_in_math ;
  s_ncols = s_ncols ;
  s_empty = s_empty ;
  s_blank = s_blank ;
  s_pending_par = s_pending_par ;
  s_vsize = s_vsize ;
  s_nrows = s_nrows ;
  s_table_vsize = s_table_vsize ;
  s_nitems = s_nitems ;
  s_dt = s_dt ;
  s_dcount = s_dcount ;
  s_insert = s_insert ;
  s_active = s_active ;
  s_after = s_after
}  ->  
  check_stack s_table_inside ;
  check_stack s_saved_inside ;
  check_stack s_in_math ;
  check_stack s_ncols ;
  check_stack s_empty ;
  check_stack s_blank ;
  check_stack s_pending_par ;
  check_stack s_vsize ;
  check_stack s_nrows ;
  check_stack s_table_vsize ;
  check_stack s_nitems ;
  check_stack s_dt ;
  check_stack s_dcount ;
  check_stack s_insert ;
  check_stack s_active ;
  check_stack s_after

(*
  Full state saving
*)

type saved = flags_t * saved_stacks * saved_out

let check () =
  let saved_flags = copy_flags flags
  and saved_stacks = save_stacks ()
  and saved_out = save_out () in
  saved_flags, saved_stacks, saved_out

  
and hot (f,s,o) =
  set_flags flags f ;
  restore_stacks s ;
  restore_out o
  

let sbool = function true -> "true" | _ -> "false"
;;

let prerr_flags s =
  prerr_endline ("<"^string_of_int (Stack.length stacks.s_empty)^"> "^s^
    " empty="^sbool flags.empty^
    " blank="^sbool flags.blank)

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
  end
  else if last = "P" then
    0
  else if
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
  | _ ->  ()
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



(* styles *)

let do_close_mod = function
  Style m ->  
    if flags.in_math && !Parse_opts.mathml then 
      if m="mtext" then do_put ("</"^m^">")
      else do_put "</mstyle>"
    else do_put ("</"^m^">")
| (Color _ | Font _)  -> 
    if flags.in_math && !Parse_opts.mathml then 
      do_put "</mstyle>"
    else do_put "</FONT>"

and do_open_mod e =
  if !verbose > 3 then
      prerr_endline ("do_open_mod: "^Latexmacros.pretty_env e) ;
  match e with
  Style m ->  
    if flags.in_math && !Parse_opts.mathml then 
      if m="mtext" then do_put ("<"^m^">")
      else do_put ("<mstyle style = \""^
		   (match m with
		     "B" -> "font-weight: bold "
		   | "I" -> "font-style: italic "
		   | "TT" -> "font-family: courier "
		   | "EM" -> "font-style: italic "
		   | _ -> m)^
		   "\">")
    else do_put ("<"^m^">")
| Font i  ->
    if flags.in_math && !Parse_opts.mathml then 
      do_put ("<mstyle style = \"font-size: "^string_of_int i^"\">")
    else do_put ("<FONT SIZE="^string_of_int i^">")
| Color s ->
    if flags.in_math && !Parse_opts.mathml then 
      do_put ("<mstyle style = \"color: "^s^"\">")
    else do_put ("<FONT COLOR="^s^">")
;;


let do_close_tmod = function
  | {here = true ; env = env} -> do_close_mod env
  | _ -> ()

let do_close_mods () =
   List.iter do_close_tmod !cur_out.active ;
  !cur_out.active <- [] ;
  !cur_out.pending <- []
;;


let do_close_mods_pred pred same_constr =
  let tpred {env=env} = pred env in

  let rec split_again = function
    | [] -> [],None,[]
    | {here = false ; env=env} :: rest
      when same_constr env && not (pred env) ->
        [],Some env,rest
    | m :: rest ->
        let to_close,to_open,to_keep = split_again rest in
        match to_open with
        | Some _ -> m::to_close,to_open,to_keep
        | None   -> to_close,to_open,m::to_keep in
        
  let rec split = function
    | [] -> [],None,[]
    | m :: rest ->
        let to_close,close,to_keep = split rest in
        match close with
        | None ->
            if tpred m then
              if m.here then [],Some m.env,to_keep
              else
                [],None,to_keep
            else [], None, m::to_keep
        | Some _ ->
            m::to_close,close,to_keep in

  let rec filter_pred = function
    | [] -> []
    | x :: rest ->
        if pred x then filter_pred rest
        else x::filter_pred rest in
          
  let to_close,close,to_keep = split !cur_out.active in

  
  filter_pred
    (match close with
    | None -> []
    | Some env ->      
        List.iter do_close_tmod to_close ;
        do_close_mod env ;
        let (to_close_open,to_open,to_keep) = split_again to_keep in
        begin match to_open with
        | None ->
            !cur_out.active <- to_keep ;
            as_envs to_close []
        | Some env ->
            !cur_out.active <- to_keep ;
            List.iter do_close_tmod to_close_open ;
            as_envs to_close
              (as_envs to_close_open [env])
        end)

        
let close_mods () = do_close_mods ()
;;

let do_open_mods () =
  let rec do_rec color size = function
    |   [] -> []
    | Color _ as e :: rest  ->
        if color then
          let rest = do_rec true size rest in
          {here=false ; env=e}::rest
        else begin
          let rest = do_rec true size rest in
          do_open_mod e ;
          {here=true ; env=e}::rest
        end
    | Font _ as e :: rest ->
        if size then
          let rest = do_rec color true rest in
          {here=false ; env=e}::rest
        else
          let rest = do_rec color true rest in
          do_open_mod e ;
          {here=true ; env=e}::rest
    | e :: rest ->
        let rest = do_rec color size rest in
        do_open_mod e ;
        {here=true ; env=e} :: rest in
  
  let now_active = do_rec false false !cur_out.pending in
  if !verbose > 3 && !cur_out.pending <> [] then begin
    prerr_string "do_open_mods: " ;
    pretty_mods !cur_out.pending ;
    prerr_string " -> " ;
    pretty_tmods now_active ;
    prerr_endline ""
  end ;
  !cur_out.active <- now_active @ !cur_out.active ;
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


let cur_size pending active =
  let rec cur_size_active = function
    | [] -> 3
    | {here=true ; env=Font i}::_ -> i
    | _::rest -> cur_size_active rest in

  let rec cur_size_pending = function
    | [] -> cur_size_active active
    | Font i::_ -> i
    | _::rest -> cur_size_pending rest in
  cur_size_pending pending
;;

let first_same x same_constr pending active =
  let rec same_active = function
    | {here=true ; env=y} :: rest ->
        if same_constr y then x=y
        else same_active rest
    | _::rest -> same_active rest
    | [] -> false in
  let rec same_pending = function
    | [] -> same_active active
    | y::rest ->
        if same_constr y then x=y
        else same_pending rest in
  same_pending pending
;;

let already_here = function
  Font i ->
   i = cur_size  !cur_out.pending !cur_out.active  
| x ->
  first_same x
   (match x with
     Style _ ->  is_style
   | Font _ -> is_font
   | Color _ -> is_color)
   !cur_out.pending !cur_out.active
;;

let ok_pre x = match x with
| Color _ | Font _ | Style "SUB" | Style "SUP" ->  not !Parse_opts.pedantic
| _ -> true
;;

let rec filter_pre = function
  [] -> []
| e::rest ->
   if ok_pre e then e::filter_pre rest
   else filter_pre rest
;;

let ok_mod e =
  (not flags.in_pre || ok_pre e) &&
  (not (already_here e))
;;

let get_fontsize () = cur_size !cur_out.pending !cur_out.active

let nostyle () =
  !cur_out.pending <- [] ;
  !cur_out.nostyle <- true    
;;

let clearstyle () =
  !cur_out.pending <- []
;;


let rec erase_rec pred = function
  [] -> None
| s::rest ->
   if pred s then
     Some rest
   else
     match erase_rec pred rest with
     | Some rest -> Some (s::rest)
     | None -> None
;;

let erase_mod_pred pred same_constr =
  if not !cur_out.nostyle then begin
    match erase_rec pred !cur_out.pending with
    | Some pending ->
        !cur_out.pending <- pending
    | None ->
        let re_open = do_close_mods_pred pred same_constr in
        !cur_out.pending <- !cur_out.pending @ re_open
  end
;;

let same_env = function
  | Style s1 -> (function | Style s2 -> s1 = s2 | _ -> false)
  | Font i1 ->
      (function | Font i2 -> i1 = i2 | _ -> false)
  | Color s1 ->
      (function | Color s2 -> s1 = s2 | _ -> false)

and same_constr = function
  | Color _ -> is_color
  | Font _ -> is_font
  | Style _ -> is_style

let erase_mods ms =
  let rec erase_rec = function
    | [] -> ()
    | m :: ms ->
        erase_mod_pred (same_env m) (same_constr m) ;
        erase_rec ms in
  erase_rec ms
;;

let open_mod  m =
  if not !cur_out.nostyle then begin
    if !verbose > 3 then begin
          prerr_endline ("open_mod: "^Latexmacros.pretty_env m^" ok="^sbool (ok_mod m)) ;
      prerr_string "pending = " ; pretty_mods !cur_out.pending ;
      prerr_endline "" ;
      prerr_string "active = " ; pretty_tmods !cur_out.active ;
      prerr_endline ""
    end ;
    begin match m with
    | Style "EM" ->
        if already_here m then
          erase_mods [m]
        else
          !cur_out.pending <- m :: !cur_out.pending
    | _ ->
        if ok_mod m then begin
          match m with
          | Style _ ->
              !cur_out.pending <- m :: !cur_out.pending
          | _ ->
              erase_mod_pred (same_env m) (same_constr m) ;
              !cur_out.pending <- (m :: !cur_out.pending)
        end
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
    push stacks.s_empty flags.empty ; push stacks.s_blank flags.blank ;
    push stacks.s_insert flags.insert ;
    flags.empty <- true ; flags.blank <- true ;
    flags.insert <- None ;
    if s = "TABLE" then begin
      push stacks.s_table_vsize flags.table_vsize ;
      push stacks.s_vsize flags.vsize ;
      push stacks.s_nrows flags.nrows ;
      flags.table_vsize <- 0 ;
      flags.vsize <- 0 ;
      flags.nrows <- 0
    end else if s = "TR"  then begin
      flags.vsize <- 1
    end else if s = "TD" then begin
      push stacks.s_vsize flags.vsize ;
      flags.vsize <- 1
    end else if is_list s then begin
      push stacks.s_nitems flags.nitems;
      flags.nitems <- 0 ;
      if s = "DL" then begin
        push stacks.s_dt flags.dt ;
        push stacks.s_dcount flags.dcount;
        flags.dt <- "";
        flags.dcount <- ""
      end
    end
  end ;
  if !verbose > 2 then
    prerr_flags ("<= try open ``"^s^"''")
;;

let do_do_open_block s args =
    if s = "TR" || is_header s then
      do_put "\n";
    do_put_char '<' ;
    do_put s ;
    if args <> "" then begin
      if args.[0] <> ' ' then do_put_char ' ' ;
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
    let ehere = flags.empty and ethere = pop  stacks.s_empty in
    flags.empty <- (ehere && ethere) ;
    let bhere = flags.blank and bthere = pop  stacks.s_blank in
    flags.blank <- (bhere && bthere) ;
    flags.insert <- pop  stacks.s_insert ;
    if s = "TABLE" then begin
      let p_vsize = pop stacks.s_vsize in
      flags.vsize <- max
       (flags.table_vsize + (if flags.nrows > 0 then flags.nrows/3 else 0)) p_vsize ;
      flags.nrows <- pop  stacks.s_nrows ;
      flags.table_vsize <- pop stacks.s_table_vsize
    end else if s = "TR" then begin
      if ehere then begin
        flags.vsize <- 0
      end ;
      flags.table_vsize <- flags.table_vsize + flags.vsize;
      if not ehere then flags.nrows <- flags.nrows + 1
    end else if s = "TD" then begin
      let p_vsize = pop stacks.s_vsize in
      flags.vsize <- max p_vsize flags.vsize
    end else if is_list s then begin
      flags.nitems <- pop stacks.s_nitems ;
      if s = "DL" then begin
        flags.dt <- pop stacks.s_dt ;
        flags.dcount <- pop  stacks.s_dcount
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

let check_empty () = flags.empty
and make_empty () =
  flags.empty <- true ; flags.blank <- true ;
  !cur_out.pending <-  to_pending !cur_out.pending !cur_out.active ;
  !cur_out.active <- []
;;

let rec force_block s content =
  if !verbose > 2 then begin
    prerr_string ("force_block: "^s^" stack: ");
    pretty_stack out_stack
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
    let mods = to_pending !cur_out.pending !cur_out.active in
    do_close_mods () ;
    do_open_block insert s args ;
    if ps = "AFTER" then begin
      let f = pop stacks.s_after in
      Out.copy_fun f old_out.out !cur_out.out
    end else begin
        Out.copy old_out.out !cur_out.out
    end ;
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

and open_block s args =
 if !verbose > 2 then begin
   prerr_flags ("=> open_block ``"^s^"''");
 end ;
 try_flush_par ();
 if s = "PRE" then
    flags.in_pre <- true;
 let cur_mods = to_pending !cur_out.pending !cur_out.active in
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


let close_block  s =
  let _ = close_block_loc check_empty s in
  ()
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
  push stacks.s_after f

and close_group () = close_block ""
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

let put_length which  = function
  | Pixel x -> put (which^string_of_int x)
  | Char x -> put (which^string_of_int (Length.font * x))
  | Percent x  -> put (which^"\""^string_of_int x^"%\"")
  | Default    -> ()
  | No s       -> raise (Misc.Fatal ("No-length ``"^s^"'' in outManager"))

let horizontal_line attr width height =
  open_block "" "" ;
  nostyle () ;
  put "<HR" ;
  begin match attr with "" -> () | _ -> put_char ' ' ; put attr end ;
  put_length " WIDTH=" width ;
  put_length " SIZE=" height ;
  put_char '>' ;
  close_block ""
;;

let line_in_table h =
  let pad = (h-1)/2 in
  put "<TABLE BORDER=0 WIDTH=\"100%\" CELLSPACING=0 CELLPADING=" ;
  put (string_of_int pad) ;
  put "><TR><TD></TD></TR></TABLE>"

let freeze f =
  push out_stack (Freeze f) ;
  if !verbose > 2 then begin
    prerr_string "freeze: stack=" ;
    pretty_stack out_stack
  end
;;

let flush_freeze () = match top out_stack with
  Freeze f ->
    let _ = pop out_stack in
    if !verbose > 2 then begin
      prerr_string "flush_freeze" ;
      pretty_stack out_stack
    end ;
    f () ; true
| _ -> false
;;

let pop_freeze () = match top  out_stack with
  Freeze f -> 
    let _ = pop out_stack in
    f,true
| _ -> (fun () -> ()),false
;;


let try_open_display () =
  push stacks.s_ncols flags.ncols ;
  push stacks.s_table_inside flags.table_inside ;
  push stacks.s_saved_inside false ;
  flags.table_inside <- false ;
  flags.ncols <- 0

and try_close_display () =
  flags.ncols <- pop stacks.s_ncols ;
  flags.table_inside <- pop stacks.s_saved_inside || flags.table_inside ;
  flags.table_inside <- pop stacks.s_table_inside || flags.table_inside
;;


let close_flow_loc s =
  if !verbose > 2 then
    prerr_endline ("close_flow_loc: "^s) ;

  let active  = !cur_out.active
  and pending = !cur_out.pending in
  if close_block_loc check_empty s then begin
    !cur_out.pending <- to_pending pending active ;
    true
  end else begin
    !cur_out.pending <- to_pending pending active ;
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
  let pempty = top stacks.s_empty
  and pblank = top stacks.s_blank
  and pinsert = top stacks.s_insert in
  try_close_block (pblock ()) ;
  flags.empty <- pempty ; flags.blank <- pblank ; flags.insert <- pinsert;
  do_close_block None s ;
  let _,_,pout = pop_out out_stack in  
  let old_out = !cur_out in  
  cur_out := new_status pout.nostyle pout.pending pout.active;
  let mods = as_envs !cur_out.active !cur_out.pending in
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




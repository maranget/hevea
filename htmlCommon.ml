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
open Element
open MyStack
open Length
open Printf


type block =
  | H1 | H2 | H3 | H4 | H5 | H6
  | PRE
  | TABLE | TR | TD
  | DISPLAY of bool | DFLOW
  | QUOTE | BLOCKQUOTE
  | DIV
  | UL | OL | DL
  | LI | DD | DT
  | GROUP | AFTER | DELAY | FORGET
  | INTERN
  | P
  | NADA
  | OTHER of string

let string_of_block = function
  | H1 -> "h1"
  | H2 -> "h2"
  | H3  -> "h3"
  | H4 -> "h4"
  | H5 -> "h5"
  | H6 -> "h6"
  | PRE -> "pre"
  | TABLE -> "table"
  | TR -> "tr"
  | TD  -> "td"
  | DISPLAY false -> "display"
  | DISPLAY true -> "display (center)"
  | DFLOW -> "dflow"
  | QUOTE -> "quote"
  | BLOCKQUOTE -> "blockquote"
  | DIV -> "div"
  | UL -> "ul"
  | OL -> "ol"
  | DL -> "dl"
  | GROUP -> ""
  | AFTER -> "after"
  | DELAY -> "delay"
  | FORGET -> "forget"
  | P     -> "p"
  | NADA  -> "nada"
  | INTERN -> "intern"
  | LI -> "li"
  | DD -> "dd"
  | DT -> "dt"
  | OTHER s -> s

let block_t = Hashtbl.create 17

let no_opt = false


let add b =
  Hashtbl.add block_t (string_of_block b) b

and add_verb s b = Hashtbl.add block_t s b

let () =
  add H1 ;
  add H2 ;
  add H3 ;
  add H4 ;
  add H5 ;
  add H6 ;
  add PRE ;
  add TABLE ;
  add TR ;
  add TD ;
  add (DISPLAY false) ;
  add QUOTE ;
  add BLOCKQUOTE ;
  add DIV ;
  add UL ;
  add OL ;
  add DL ;
  begin
    if no_opt then
      Hashtbl.add block_t "" INTERN
    else
      add GROUP
  end ;
  add AFTER ;
  add DELAY ;
  add FORGET ;
  add P ;
  add NADA

let failclose s b1 b2=
  raise (Misc.Close (s^": '"^string_of_block b1^"' closes '"^
                       string_of_block b2^"'"))

let find_block s =
  let s = String.lowercase s in
  try Hashtbl.find block_t s with
    | Not_found -> OTHER s

let eq_tags t1 t2 = match t1, t2 with
  | DISPLAY _, DISPLAY _ -> true
  | _, _ -> t1=t2

let check_block_closed opentag closetag =
  if not (eq_tags opentag closetag) &&
    not (opentag = AFTER && closetag = GROUP) then
    failclose "html" closetag opentag

let display_arg centering _verbose =
  let cl =
    if !displayverb then "vdisplay"
    else "display" in
  let cl =
    if centering then
      cl^(if  !displayverb then " vdcenter" else " dcenter")
    else cl in
  let arg = "class=\""^cl^"\"" in
  arg

(* output globals *)
type t_env = {here : bool ; env : text}

type t_top =
    {top_pending : text list ; top_active : t_env list ;}

type style_info =
  | Nothing of t_top
  | Activate of t_top
  | Closed of t_top * int
  | ActivateClosed of t_top
  | NotMe
  | Insert of bool * text list

let get_top_lists = function
  | Nothing x -> x | Activate x -> x
  | _ -> raise Not_found

let do_pretty_mods stderr f mods =
  let rec do_rec stderr = function
  [x]  -> f stderr x
    | x::xs ->
        Printf.fprintf stderr "%a; %a" f x do_rec xs
    | [] -> () in
  Printf.fprintf stderr "[%a]" do_rec mods


let tbool = function
  | true -> "+"
  | false -> "-"

let pretty_mods stderr = do_pretty_mods stderr
  (fun stderr text -> Printf.fprintf stderr "%s" (pretty_text text))

and pretty_tmods stderr =
  do_pretty_mods stderr
    (fun stderr {here=here ; env = env} ->
      Printf.fprintf stderr "%s%s" (tbool here) (pretty_text env))

let pretty_top_styles stderr {top_pending = pending ; top_active = active} =
  Printf.fprintf stderr
    "{top_pending=%a, top_active=%a}"
    pretty_mods pending
    pretty_tmods active

let tbool = function
  | true -> "+"
  | false -> "-"

let pretty_top stderr = function
  | Nothing x -> Printf.fprintf stderr "Nothing %a"  pretty_top_styles x
  | Activate x -> Printf.fprintf stderr "Activate %a" pretty_top_styles x
  | Closed _ -> Printf.fprintf stderr "Closed"
  | ActivateClosed _ -> Printf.fprintf stderr "ActivateClosed"
  | NotMe -> Printf.fprintf stderr "NotMe"
  | Insert (b,active) ->
      Printf.fprintf stderr "Insert %s %a" (tbool b) pretty_mods active

type status = {
  mutable nostyle : bool ;
  mutable pending : text list ;
  mutable active : t_env list ;
  mutable top : style_info ;
  mutable out : Out.t}

let as_env  {env=env} =  env
let as_envs tenvs r  =
  List.fold_right (fun x r -> as_env x::r) tenvs r

let to_pending pending active = pending @ as_envs active []

let with_new_out out =  {out with out = Out.create_buff ()}

let cur_out =
  ref {nostyle=false ;
       pending = [] ; active = [] ;
       top = NotMe ;
       out = Out.create_null ()}

type stack_item =
    Normal of block * string * status
  | Freeze of (unit -> unit)

exception PopFreeze

let push_out s (a,b,c) = push s (Normal (a,b,c))

let pretty_stack s = MyStack.pretty
  (function Normal (s,args,_) -> "["^string_of_block s^"]-{"^args^"}"
    | Freeze _   -> "Freeze") s

let pop_out s = match pop s with
  | Normal (a,b,c) -> a,b,c
  | Freeze _       -> raise PopFreeze

and top_out s = match top s with
  | Normal (a,b,c) -> a,b,c
  | Freeze _       -> raise PopFreeze


let out_stack =
  MyStack.create_init "out_stack" (Normal (NADA,"",!cur_out))

type saved_out = status * stack_item MyStack.saved

let save_out () = !cur_out, MyStack.save out_stack
and restore_out (a,b) =
  if !cur_out != a then begin
    MyStack.finalize out_stack
      (function
        | Normal (_,_,x) -> x == a
        | _ -> false)
      (function
        | Normal (_,_,_out) -> ()
        | _ -> ())
  end ;
  cur_out := a ;
  MyStack.restore out_stack b

let pblock () = match MyStack.top out_stack with
  | Normal (s,_,_) -> s
  | _ -> NADA

and p2block () = MyStack.top2 out_stack


let do_put_char c =
  if !verbose > 3 then
    prerr_endline ("put_char: |"^String.escaped (String.make 1 c)^"|");
  Out.put_char !cur_out.out c

and do_put s =
  if !verbose > 3 then
    prerr_endline ("put: |"^String.escaped s^"|");
  Out.put !cur_out.out s ;
  ()


(* Flags section *)
(* Style information for caller *)

type flags_t = {
  mutable table_inside:bool;
  mutable in_math : bool;
  mutable ncols:int;
  mutable empty:bool;
  mutable blank:bool;
  mutable saw_par: bool ;
  mutable vsize:int;
  mutable nrows:int;
  mutable table_vsize:int;
  mutable nitems:int;
  mutable dt:string;
  mutable dcount:string;
  mutable in_pre:bool;
  mutable insert: (block * string) option;
  mutable insert_attr: (block * string) option;
}
let pretty_cur {pending = pending ; active = active ;
                top = top} =
  Printf.fprintf stderr "pending=%a, active=%a\n"
    pretty_mods pending
    pretty_tmods active ;
  Printf.fprintf stderr "top = %a" pretty_top top ;
  prerr_endline ""


let activate_top out = match out.top with
  | Nothing x -> out.top <- Activate x
  |  _      -> ()

and close_top n out = match  out.top with
  | Nothing top  -> out.top <- Closed (top, n+Out.get_pos out.out)
  | Activate top -> out.top <- ActivateClosed top
  |  _       -> ()

let debug_attr stderr = function
  | None -> Printf.fprintf stderr "None"
  | Some (tag,attr) ->
      Printf.fprintf stderr "'%s' '%s'"
        (string_of_block tag) attr

let debug_flags f =
  Printf.fprintf stderr "attr=%a\n" debug_attr f.insert_attr ;
  flush stderr


let flags = {
  table_inside = false;
  ncols = 0;
  in_math = false;
  empty = true;
  blank = true;
  saw_par = false ;
  vsize = 0;
  nrows = 0;
  table_vsize = 0;
  nitems = 0;
  dt = "";
  dcount = "";
  in_pre = false;
  insert = None;
  insert_attr = None;
}
let copy_flags {
  table_inside = table_inside;
  ncols = ncols;
  in_math = in_math;
  empty = empty;
  blank = blank;
  saw_par = saw_par ;
  vsize = vsize;
  nrows = nrows;
  table_vsize = table_vsize;
  nitems = nitems;
  dt = dt;
  dcount = dcount;
  in_pre = in_pre;
  insert = insert;
  insert_attr = insert_attr;
} = {
  table_inside = table_inside;
  ncols = ncols;
  in_math = in_math;
  empty = empty;
  blank = blank;
  saw_par = saw_par ;
  vsize = vsize;
  nrows = nrows;
  table_vsize = table_vsize;
  nitems = nitems;
  dt = dt;
  dcount = dcount;
  in_pre = in_pre;
  insert = insert;
  insert_attr = insert_attr;
}
and set_flags f {
  table_inside = table_inside ;
  ncols = ncols;
  in_math = in_math;
  empty = empty;
  blank = blank;
  saw_par = saw_par ;
  vsize = vsize;
  nrows = nrows;
  table_vsize = table_vsize;
  nitems = nitems;
  dt = dt;
  dcount = dcount;
  in_pre = in_pre;
  insert = insert;
  insert_attr = insert_attr;
} =
  f.table_inside <- table_inside;
  f.ncols <- ncols;
  f.in_math <- in_math;
  f.empty <- empty;
  f.blank <- blank;
  f.saw_par <- saw_par ;
  f.vsize <- vsize;
  f.nrows <- nrows;
  f.table_vsize <- table_vsize;
  f.nitems <- nitems;
  f.dt <- dt;
  f.dcount <- dcount;
  f.in_pre <- in_pre;
  f.insert <- insert ;
  f.insert_attr <- insert_attr ;



(* Independant stacks for flags *)
type stack_t = {
  s_table_inside : bool MyStack.t ;
  s_saved_inside : bool MyStack.t ;
  s_in_math : bool MyStack.t ;
  s_ncols : int MyStack.t ;
  s_empty : bool MyStack.t ;
  s_blank : bool MyStack.t ;
  s_saw_par : bool MyStack.t ;
  s_vsize : int MyStack.t ;
  s_nrows : int MyStack.t ;
  s_table_vsize : int MyStack.t ;
  s_nitems : int MyStack.t ;
  s_dt : string MyStack.t ;
  s_dcount : string MyStack.t ;
  s_insert : (block * string) option MyStack.t ;
  s_insert_attr : (block * string) option MyStack.t ;
  (* Other stacks, not corresponding to flags *)
  s_active : Out.t MyStack.t ;
  s_after : (string -> string) MyStack.t
}

let stacks = {
  s_table_inside = MyStack.create "inside" ;
  s_saved_inside = MyStack.create "saved_inside" ;
  s_in_math = MyStack.create_init "in_math" false ;
  s_ncols = MyStack.create "ncols" ;
  s_empty = MyStack.create_init "empty" false;
  s_blank = MyStack.create_init "blank" false ;
  s_saw_par = MyStack.create "saw_par" ;
  s_vsize = MyStack.create "vsize" ;
  s_nrows = MyStack.create_init "nrows" 0 ;
  s_table_vsize = MyStack.create_init "table_vsize" 0 ;
  s_nitems = MyStack.create_init "nitems" 0 ;
  s_dt = MyStack.create_init "dt" "" ;
  s_dcount = MyStack.create_init "dcount" "" ;
  s_insert = MyStack.create_init "insert" None;
  s_insert_attr = MyStack.create_init "insert_attr" None;
  s_active = MyStack.create "Html.active" ;
  s_after = MyStack.create "Html.after"
}

type saved_stacks = {
  ss_table_inside : bool MyStack.saved ;
  ss_saved_inside : bool MyStack.saved ;
  ss_in_math : bool MyStack.saved ;
  ss_ncols : int MyStack.saved ;
  ss_empty : bool MyStack.saved ;
  ss_blank : bool MyStack.saved ;
  ss_saw_par : bool MyStack.saved ;
  ss_vsize : int MyStack.saved ;
  ss_nrows : int MyStack.saved ;
  ss_table_vsize : int MyStack.saved ;
  ss_nitems : int MyStack.saved ;
  ss_dt : string MyStack.saved ;
  ss_dcount : string MyStack.saved ;
  ss_insert : (block * string) option MyStack.saved ;
  ss_insert_attr : (block * string) option MyStack.saved ;
  (* Other stacks, not corresponding to flags *)
  ss_active : Out.t MyStack.saved ;
  ss_after : (string -> string) MyStack.saved
}

let save_stacks () =
  {
    ss_table_inside = MyStack.save stacks.s_table_inside ;
    ss_saved_inside = MyStack.save stacks.s_saved_inside ;
    ss_in_math = MyStack.save stacks.s_in_math ;
    ss_ncols = MyStack.save stacks.s_ncols ;
    ss_empty = MyStack.save stacks.s_empty ;
    ss_blank = MyStack.save stacks.s_blank ;
    ss_saw_par = MyStack.save stacks.s_saw_par ;
    ss_vsize = MyStack.save stacks.s_vsize ;
    ss_nrows = MyStack.save stacks.s_nrows ;
    ss_table_vsize = MyStack.save stacks.s_table_vsize ;
    ss_nitems = MyStack.save stacks.s_nitems ;
    ss_dt = MyStack.save stacks.s_dt ;
    ss_dcount = MyStack.save stacks.s_dcount ;
    ss_insert = MyStack.save stacks.s_insert ;
    ss_insert_attr = MyStack.save stacks.s_insert_attr ;
    ss_active = MyStack.save stacks.s_active ;
    ss_after = MyStack.save stacks.s_after
  }

and restore_stacks
    {
      ss_table_inside = saved_table_inside ;
      ss_saved_inside = saved_saved_inside ;
      ss_in_math = saved_in_math ;
      ss_ncols = saved_ncols ;
      ss_empty = saved_empty ;
      ss_blank = saved_blank ;
      ss_saw_par = saved_saw_par ;
      ss_vsize = saved_vsize ;
      ss_nrows = saved_nrows ;
      ss_table_vsize = saved_table_vsize ;
      ss_nitems = saved_nitems ;
      ss_dt = saved_dt ;
      ss_dcount = saved_dcount ;
      ss_insert = saved_insert ;
      ss_insert_attr = saved_insert_attr ;
      ss_active = saved_active ;
      ss_after = saved_after
    }   =
  MyStack.restore stacks.s_table_inside saved_table_inside ;
  MyStack.restore stacks.s_saved_inside saved_saved_inside ;
  MyStack.restore stacks.s_in_math saved_in_math ;
  MyStack.restore stacks.s_ncols saved_ncols ;
  MyStack.restore stacks.s_empty saved_empty ;
  MyStack.restore stacks.s_blank saved_blank ;
  MyStack.restore stacks.s_saw_par saved_saw_par ;
  MyStack.restore stacks.s_vsize saved_vsize ;
  MyStack.restore stacks.s_nrows saved_nrows ;
  MyStack.restore stacks.s_table_vsize saved_table_vsize ;
  MyStack.restore stacks.s_nitems saved_nitems ;
  MyStack.restore stacks.s_dt saved_dt ;
  MyStack.restore stacks.s_dcount saved_dcount ;
  MyStack.restore stacks.s_insert saved_insert ;
  MyStack.restore stacks.s_insert_attr saved_insert_attr ;
  MyStack.restore stacks.s_active saved_active ;
  MyStack.restore stacks.s_after saved_after


let check_stack what =
  if not (MyStack.empty what)  && not !silent then begin
    prerr_endline
      ("Warning: stack "^MyStack.name what^" is non-empty in Html.finalize") ;
  end

let check_stacks () = match stacks with
    {
      s_table_inside = s_table_inside ;
      s_saved_inside = s_saved_inside ;
      s_in_math = s_in_math ;
      s_ncols = s_ncols ;
      s_empty = s_empty ;
      s_blank = s_blank ;
      s_saw_par = s_saw_par ;
      s_vsize = s_vsize ;
      s_nrows = s_nrows ;
      s_table_vsize = s_table_vsize ;
      s_nitems = s_nitems ;
      s_dt = s_dt ;
      s_dcount = s_dcount ;
      s_insert = s_insert ;
      s_insert_attr = s_insert_attr ;
      s_active = s_active ;
      s_after = s_after
    }  ->
      check_stack s_table_inside ;
      check_stack s_saved_inside ;
      check_stack s_in_math ;
      check_stack s_ncols ;
      check_stack s_empty ;
      check_stack s_blank ;
      check_stack s_saw_par ;
      check_stack s_vsize ;
      check_stack s_nrows ;
      check_stack s_table_vsize ;
      check_stack s_nitems ;
      check_stack s_dt ;
      check_stack s_dcount ;
      check_stack s_insert ;
      check_stack s_insert_attr ;
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

let prerr_flags s =
  prerr_endline ("<"^string_of_int (MyStack.length stacks.s_empty)^"> "^s^
                    " empty="^sbool flags.empty^
                    " blank="^sbool flags.blank^
                    " table="^sbool flags.table_inside)

let is_header = function
  | H1 | H2 | H3 | H4 | H5 | H6 -> true
  | _ -> false

let is_list = function
UL | DL | OL -> true
  | _ -> false

let string_of_into = function
  | Some n -> "+"^string_of_int n
  | None -> "-"


(* styles *)

let trim_quotes s =
  (* used for ensuring colors are passed without ""  *)
  let (i, l) =
    if s.[0] = '"' then (1, String.length s - 2) else (0, String.length s)
  in String.sub s i l

(* '"' *)
let size_html5 = function
  | 1 -> "xx-small"
  | 2 -> "small"
  | 3 -> "medium"
  | 4 -> "large"
  | 5 -> "x-large"
  | 6 -> "xx-large"
  | 7 -> "xx-large"
  | _ -> raise (Misc.fatal "size_html5")

let do_close_mod = function
  | Style m ->
      if flags.in_math && !Parse_opts.mathml then
        if m="mtext" then do_put ("</"^m^">")
        else do_put "</mstyle>"
      else do_put ("</"^m^">")
  | StyleAttr (t,_) ->
      if flags.in_math && !Parse_opts.mathml then
        ()
      else
        do_put ("</"^t^">")
  | (Color _ | Font _)  ->
      if flags.in_math && !Parse_opts.mathml then
        do_put "</mstyle>"
      else do_put "</span>"

and do_open_mod e =
  if !verbose > 3 then
    prerr_endline ("do_open_mod: "^pretty_text e) ;
  match e with
    | Style m ->
        if flags.in_math && !Parse_opts.mathml then
          if m="mtext" then do_put ("<"^m^">")
          else do_put ("<mstyle style = \""^
                          (match m with
                              "b" -> "font-weight: bold "
                            | "i" -> "font-style: italic "
                            | "tt" -> "font-family: monospace "
                            | "em" -> "font-style: italic "
                            | _ -> m)^
                          "\">")
        else do_put ("<"^m^">")
    | StyleAttr (t,a) ->
        if flags.in_math && !Parse_opts.mathml then ()
        else
          do_put ("<"^t^" "^a^">")
    | Font i  ->
        if flags.in_math && !Parse_opts.mathml then
          do_put ("<mstyle style = \"font-size: "^string_of_int i^"\">")
        else
          (* Convert size from 1 to 7 in percentage, following standard *)
          do_put (sprintf "<span style=\"font-size:%s\">" (size_html5 i))
    | Color s ->
        if flags.in_math && !Parse_opts.mathml then
          do_put ("<mstyle style = \"color: "^s^"\">")
        else do_put (sprintf "<span style=\"color:%s\">" (trim_quotes s))


let do_close_tmod = function
  | {here = true ; env = env} -> do_close_mod env
  | _ -> ()

let close_active_mods active = List.iter do_close_tmod active

let do_close_mods () =
  close_active_mods !cur_out.active ;
  !cur_out.active <- [] ;
  !cur_out.pending <- []


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
          end),
  close


let close_mods () = do_close_mods ()


let is_style = function
  |  Style _|StyleAttr (_,_) -> true
  | _ -> false

and is_font = function
Font _ -> true
  | _ -> false

and is_color = function
Color _ -> true
  | _ -> false


let do_open_these_mods do_open_mod pending =
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
  do_rec
    false
    false
    pending

let activate caller pending =
  let r = do_open_these_mods (fun _ -> ()) pending in
  if !verbose > 2 then begin
    prerr_string ("activate: ("^caller^")") ;
    pretty_mods stderr pending ; prerr_string " -> " ;
    pretty_tmods stderr r ;
    prerr_endline ""
  end ;
  r

let get_top_active = function
  | Nothing {top_active = active} -> active
  | Activate {top_pending = pending ; top_active = active} ->
      activate "get_top_active" pending @ active
  | _ -> []

let all_to_pending out =
  try
    let top = get_top_lists out.top in
    to_pending out.pending out.active @
      to_pending top.top_pending top.top_active
  with
    | Not_found ->
        to_pending out.pending out.active

let all_to_active out = activate "all_to_active" (all_to_pending out)

(* Clear styles *)
let clearstyle () =
  close_active_mods !cur_out.active ;
  close_active_mods (get_top_active !cur_out.top) ;
  close_top 0 !cur_out ;
  !cur_out.pending <- [] ;
  !cur_out.active <- []

(* Avoid styles *)
let nostyle () =
  clearstyle () ;
  !cur_out.nostyle <- true

(* Create new statuses, with appropriate pending lists *)

let create_status_from_top out = match out.top with
  | NotMe|Closed _|ActivateClosed _|Insert (_,_) ->
      {nostyle=out.nostyle ; pending = []  ; active = []  ;
       top =
          Nothing
            {top_pending = out.pending ; top_active = out.active} ;
       out = Out.create_buff ()}
  | Nothing {top_pending = top_pending ; top_active=top_active} ->
      assert (out.active=[]) ;
      {nostyle=out.nostyle ; pending = [] ; active = [] ;
       top =
          Nothing
            {top_pending = out.pending @ top_pending ;
             top_active = top_active} ;
       out = Out.create_buff ()}
  | Activate {top_pending = top_pending ; top_active=top_active} ->
      {nostyle=out.nostyle ; pending = [] ; active = [] ;
       top=
          Nothing
            {top_pending = out.pending ;
             top_active = out.active @ activate "top" top_pending @ top_active} ;
       out=Out.create_buff ()}


let create_status_from_scratch nostyle pending =
  {nostyle=nostyle ;
   pending =pending  ; active = []  ;
   top=NotMe ;
   out = Out.create_buff ()}

let do_open_mods () =
  if !verbose > 2 then begin
    prerr_string "=> do_open_mods: " ;
    pretty_cur !cur_out
  end ;

  let now_active =
    do_open_these_mods do_open_mod !cur_out.pending in
  activate_top !cur_out ;
  !cur_out.active <- now_active @ !cur_out.active ;
  !cur_out.pending <- [] ;

  if !verbose > 2 then begin
    prerr_string "<= do_open_mods: " ;
    pretty_cur !cur_out
  end




let do_pending () =  do_open_mods ()


let one_cur_size pending active =
  let rec cur_size_active = function
    | [] -> raise Not_found
    | {here=true ; env=Font i}::_ -> i
    | _::rest -> cur_size_active rest in

  let rec cur_size_pending = function
    | [] -> cur_size_active active
    | Font i::_ -> i
    | _::rest -> cur_size_pending rest in
  cur_size_pending pending

let cur_size out =
  try one_cur_size out.pending out.active
  with Not_found ->
    try
      let top_out = get_top_lists out.top in
      one_cur_size top_out.top_pending top_out.top_active
    with Not_found -> 3

let one_first_same x same_constr pending active =
  let rec same_active = function
    | {here=true ; env=y} :: rest ->
        if same_constr y then x=y
        else same_active rest
    | _::rest -> same_active rest
    | [] -> raise Not_found in
  let rec same_pending = function
    | [] -> same_active active
    | y::rest ->
        if same_constr y then x=y
        else same_pending rest in
  same_pending pending

let first_same x same_constr out =
  try
    one_first_same x same_constr out.pending out.active
  with Not_found ->
    try
      let top_out = get_top_lists out.top in
      one_first_same x same_constr top_out.top_pending top_out.top_active
    with
      | Not_found -> false

let already_here = function
  | Font i ->
      i = cur_size !cur_out
  | x ->
      first_same x
        (match x with
            Style _|StyleAttr (_,_) ->  is_style
          | Font _ -> is_font
          | Color _ -> is_color)
        !cur_out

let ok_pre x = match x with
  | Color _ | Font _ | Style "sub" | Style "sup" ->  not !Parse_opts.pedantic
  | _ -> true

let rec filter_pre = function
[] -> []
  | e::rest ->
      if ok_pre e then e::filter_pre rest
      else filter_pre rest

let ok_mod e =
  (not flags.in_pre || ok_pre e) &&
    (not (already_here e))

let get_fontsize () = cur_size !cur_out


let rec erase_rec pred = function
[] -> None
  | s::rest ->
      if pred s then
        Some rest
      else
        match erase_rec pred rest with
          | Some rest -> Some (s::rest)
          | None -> None


let erase_mod_pred pred same_constr =
  if not !cur_out.nostyle then begin
    match erase_rec pred !cur_out.pending with
      | Some pending ->
          !cur_out.pending <- pending
      | None ->
          let re_open,closed = do_close_mods_pred pred same_constr in
          match closed with
            | Some _ ->
                !cur_out.pending <- !cur_out.pending @ re_open
            | None ->
                activate_top !cur_out ;
                try
                  let tops = get_top_lists !cur_out.top in
                  !cur_out.active <-
                    !cur_out.active @
                    activate "erase" tops.top_pending @
                    tops.top_active ;
                  close_top 0 !cur_out ;
                  let re_open,_ = do_close_mods_pred pred same_constr in
                  !cur_out.pending <- !cur_out.pending @ re_open
                with
                  | Not_found -> ()
  end

let same_env = function
  | Style s1 -> (function | Style s2 -> s1 = s2 | _ -> false)
  | StyleAttr (t1,a1) ->
      (function | StyleAttr (t2,a2)-> t1 = t2 && a1=a2 | _ -> false)
  | Font i1 ->
      (function | Font i2 -> i1 = i2 | _ -> false)
  | Color s1 ->
      (function | Color s2 -> s1 = s2 | _ -> false)

and same_constr = function
  | Color _ -> is_color
  | Font _ -> is_font
  | Style _|StyleAttr (_,_) -> is_style

let erase_mods ms =
  let rec erase_rec = function
    | [] -> ()
    | m :: ms ->
        erase_mod_pred (same_env m) (same_constr m) ;
        erase_rec ms in
  erase_rec ms

let open_mod  m =
  if not !cur_out.nostyle then begin
    if !verbose > 3 then begin
      prerr_endline ("=> open_mod: "^pretty_text m^" ok="^sbool (ok_mod m)) ;
      pretty_cur !cur_out
    end ;
    begin match m with
      | Style "em" ->
          if already_here m then
            erase_mods [m]
          else
            !cur_out.pending <- m :: !cur_out.pending
      | _ ->
          if ok_mod m then begin
            !cur_out.pending <- m :: !cur_out.pending
          end
    end ;
    if !verbose > 3 then begin
      prerr_endline ("<= open_mod: "^pretty_text m) ;
      pretty_cur !cur_out
    end ;
  end

let rec open_mods = function
m::rest -> open_mods rest ; open_mod m
  | []      -> ()

let has_mod m = already_here m

(* Blocks *)

let pstart = function
  |  H1 | H2 | H3 | H4 | H5 | H6
  | PRE
  | DIV
  | BLOCKQUOTE
  | UL | OL | DL
  | TABLE -> true
  | _ -> false

let transmit_par s = match s with
  | GROUP|AFTER|INTERN|DFLOW|P|LI -> false
  | _ -> true

let is_group = function
  | GROUP -> true
  | _ -> false

and transmit_par_or_par = function
  | P -> true
  | s -> transmit_par s

and is_pre = function
  | PRE -> true
  | _ -> false

let rec do_try_open_block s =
  if !verbose > 2 then
    prerr_flags ("=> try open '"^string_of_block s^"'");
  begin match s with
    | DISPLAY _ ->
        do_try_open_block TABLE ;
        do_try_open_block TR
    | _  ->
        push stacks.s_empty flags.empty ; push stacks.s_blank flags.blank ;
        push stacks.s_insert flags.insert ; push stacks.s_saw_par flags.saw_par ;
        flags.empty <- true ; flags.blank <- true ;
        flags.insert <- None ; flags.saw_par <- false ;
        begin match s with
          | PRE -> flags.in_pre <- true (* No stack, cannot nest *)
          | TABLE ->
              push stacks.s_table_vsize flags.table_vsize ;
              push stacks.s_vsize flags.vsize ;
              push stacks.s_nrows flags.nrows ;
              flags.table_vsize <- 0 ;
              flags.vsize <- 0 ;
              flags.nrows <- 0
          |  TR ->
              flags.vsize <- 1
          |  TD ->
              push stacks.s_vsize flags.vsize ;
              flags.vsize <- 1
          | _ ->
              if is_list s then begin
                push stacks.s_nitems flags.nitems;
                flags.nitems <- 0 ;
                if s = DL then begin
                  push stacks.s_dt flags.dt ;
                  push stacks.s_dcount flags.dcount;
                  flags.dt <- "";
                  flags.dcount <- ""
                end
              end
        end
  end ;
  if !verbose > 2 then
    prerr_flags ("<= try open '"^string_of_block s^"'")

let try_open_block s _ =
  push stacks.s_insert_attr flags.insert_attr ;
  begin match flags.insert_attr with
    | Some (TR,_) when s <> TR -> ()
    | _ -> flags.insert_attr <- None
  end ;
  do_try_open_block s

let do_do_open_block s args =
  do_put_char '<' ;
  do_put (string_of_block s) ;
  if args <> "" then begin
    if args.[0] <> ' ' then do_put_char ' ' ;
    do_put args
  end ;
  do_put_char '>'


let rec do_open_block insert s args = match s with
  | GROUP|DELAY|FORGET|AFTER|INTERN|DFLOW ->
      begin match insert with
        | Some (tag,iargs) -> do_do_open_block tag iargs
        | _ -> ()
      end
  | DISPLAY centering ->
      do_open_block insert TABLE (display_arg centering !verbose) ;
      do_open_block None TR args
  | _  -> begin match insert with
      | Some (tag,iargs) ->
          if is_list s || s = TABLE || s = P then begin
            do_do_open_block tag iargs ;
            do_do_open_block s args
          end else begin
            do_do_open_block s args ;
            do_do_open_block tag iargs
          end
      | _ -> do_do_open_block s args
  end

let rec do_try_close_block s =
  if !verbose > 2 then
    prerr_flags ("=> try close '"^string_of_block s^"'") ;
  begin match s with
    | DISPLAY _ ->
        do_try_close_block TR ;
        do_try_close_block TABLE
    | _ ->
        let ehere = flags.empty and ethere = pop  stacks.s_empty in
        flags.empty <- (ehere && ethere) ;
        let bhere = flags.blank and bthere = pop  stacks.s_blank in
        flags.blank <- (bhere && bthere) ;
        flags.insert <- pop  stacks.s_insert ;
        flags.saw_par <- pop stacks.s_saw_par ;
        begin match s with
          | PRE   -> flags.in_pre <- false (* PRE cannot nest *)
          | TABLE ->
              let p_vsize = pop stacks.s_vsize in
              flags.vsize <-
                max
                (flags.table_vsize + (flags.nrows+1)/3)
                p_vsize ;
              flags.nrows <- pop  stacks.s_nrows ;
              flags.table_vsize <- pop stacks.s_table_vsize
          |  TR ->
              if ehere then begin
                flags.vsize <- 0
              end ;
              flags.table_vsize <- flags.table_vsize + flags.vsize;
              if not ehere then flags.nrows <- flags.nrows + 1
          | TD ->
              let p_vsize = pop stacks.s_vsize in
              flags.vsize <- max p_vsize flags.vsize
          | _ ->
              if is_list s then begin
                flags.nitems <- pop stacks.s_nitems ;
                if s = DL then begin
                  flags.dt <- pop stacks.s_dt ;
                  flags.dcount <- pop  stacks.s_dcount
                end
              end
        end
  end ;
  if !verbose > 2 then
    prerr_flags ("<= try close '"^string_of_block s^"'")

let try_close_block s =
  begin match flags.insert_attr with
    | Some (tag,_) when tag = s ->
        flags.insert_attr <- pop stacks.s_insert_attr
    | _ -> match pop stacks.s_insert_attr with
        | None -> ()
        | Some (_,_) as x -> flags.insert_attr <- x
  end ;
  do_try_close_block s

let do_do_close_block s =
  do_put "</" ;
  do_put (string_of_block s) ;
  do_put_char '>' ;
  match s with TR -> do_put_char '\n' | _ -> ()

let rec do_close_block insert s = match s with
  | GROUP|DELAY|FORGET|AFTER|INTERN|DFLOW ->
      begin match insert with
        | Some (tag,_) -> do_do_close_block tag
        | _ -> ()
      end
  | DISPLAY _ ->
      do_close_block None TR ;
      do_close_block insert TABLE
  | s  -> begin match insert with
      | Some (tag,_) ->
          if is_list s || s = TABLE || s = P then begin
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
  flags.table_inside <- false ;
  !cur_out.top <- NotMe ;
  !cur_out.pending <-  to_pending !cur_out.pending !cur_out.active ;
  !cur_out.active <- []

let check_blank () = flags.blank

let no_check () = false

let rec open_top_styles = function
  | NotMe|Insert (_,_) -> (* Real block, inserted block *)
      begin match !cur_out.top with
        | Nothing tops ->
            let mods =
              to_pending !cur_out.pending !cur_out.active @
                to_pending tops.top_pending tops.top_active in
            assert (!cur_out.active=[]) ;
            close_active_mods tops.top_active ;
            !cur_out.top <- Closed (tops,Out.get_pos !cur_out.out);
            Some mods
        | Activate tops ->
            !cur_out.top <- ActivateClosed tops ;
            let mods =
              to_pending !cur_out.pending !cur_out.active @
                to_pending tops.top_pending tops.top_active in
            close_active_mods !cur_out.active ;
            close_active_mods (activate "open_top_styles" tops.top_pending) ;
            close_active_mods tops.top_active ;
            Some mods
        | _ ->
            let mods = to_pending !cur_out.pending !cur_out.active in
            close_active_mods !cur_out.active ;
            Some mods
      end
  | Closed (_,n) -> (* Group that closed top_styles (all of them) *)
      let out = !cur_out in
      let mods = all_to_pending out in
      close_top n out ;
      Some mods
  | Nothing _ -> (* Group with nothing to do *)
      None
  | Activate _ -> (* Just activate styles *)
      do_open_mods () ;
      None
  | ActivateClosed tops ->
      do_open_mods () ;
      let r = open_top_styles (Closed (tops,Out.get_pos !cur_out.out)) in
      r

let rec force_block s content =
  if !verbose > 2 then begin
    prerr_endline ("=> force_block: ["^string_of_block s^"]");
    pretty_stack out_stack ;
    pretty_cur !cur_out ;
    Out.debug stderr !cur_out.out ;
    prerr_newline ()
  end ;
  let pempty = top stacks.s_empty in
  if s = FORGET then begin
    make_empty () ;
  end else begin
    begin match s with
      | TABLE|DISPLAY _ -> flags.table_inside <- true
      | _ -> ()
    end ;
    if flags.empty then begin
      flags.empty <- false; flags.blank <- false ;
      do_open_mods () ;
      do_put content
    end ;
    (* check pending display material in DFLOW
       More precisely
       * closed block s, has a table inside
       * previous block is DFLOW, with some pending material (pempty = false)
       A Freeze can be present, then the previous block is DFLOW
       Then, we need to flush the pending material...
    *)
    if not pempty && flags.table_inside then begin
      let p2 = p2block () in
      match p2 with
        |	(Normal (DFLOW,_,_)) | Freeze _ ->
            let _,_,pout = top_out out_stack in
            if !verbose > 2 then begin
              Printf.eprintf "CLOSING: '%s': " (string_of_block s) ;
              Out.debug stderr pout.out ;
              pretty_stack out_stack ;
              prerr_endline ""
            end ;
            let saved_flags = copy_flags flags in
            try_close_block s ;
            let a,b,pout = pop_out out_stack in
            let saved_out = !cur_out in
            cur_out := pout ;
            let fo = match p2 with
              | Normal (_,_,_) -> force_block DFLOW "" ; None
              | Freeze f ->
                  let _ = pop out_stack in
                  force_block DFLOW "" ;
                  Some (f) in
            let _,args,_ = top_out out_stack in
            force_block TD "" ;
            open_block TD args ;
            open_block DFLOW "" ;
            begin match fo with
              | Some f -> push out_stack (Freeze f)
              | None -> ()
            end ;
            push_out out_stack (a,b,!cur_out) ;
            try_open_block s b ;
            cur_out := saved_out ;
            set_flags flags saved_flags ;
            flags.ncols <- flags.ncols + 1
        |	_ -> ()
    end else if !verbose > 2 && not pempty && flags.table_inside then begin
      Printf.eprintf "NOT CLOSING: '%s': " (string_of_block s) ;
      pretty_stack out_stack ;
      let _,_,pout = top_out out_stack in
      Out.debug stderr pout.out ;
      prerr_newline ()
    end
  end ;
  let true_s = if s = FORGET then pblock() else s in
  let insert = flags.insert
  and insert_attr = flags.insert_attr
  and was_top = !cur_out.top in
  do_close_mods () ;
  try_close_block true_s ;
  do_close_block insert true_s ;
  let ps,args,pout = pop_out out_stack in
  check_block_closed ps true_s ;

  let old_out = !cur_out in
  cur_out := pout ;
  if s = FORGET then ()
  else if ps <> DELAY then begin

    let mods = open_top_styles was_top in
    do_open_block insert ps
      (match insert_attr with
        | Some (this_tag,attr) when this_tag = s -> args^" "^attr
        | _ -> args) ;

    begin match was_top with
      | Insert (_,mods) ->
          ignore (do_open_these_mods do_open_mod mods)
      | _ -> ()
    end ;

    (*
      prerr_endline "****** NOW *******" ;
      pretty_cur !cur_out ;
      Out.debug stderr old_out.out ;
      prerr_endline "\n**********" ;
    *)
    if ps = AFTER then begin
      let f = pop stacks.s_after in
      Out.copy_fun f old_out.out !cur_out.out
    end else begin
      Out.copy old_out.out !cur_out.out
    end ;
    begin match mods with
      | Some mods ->
          !cur_out.active  <- [] ;
          !cur_out.pending <- mods
      | _ -> ()
    end
  end else begin (* ps = DELAY *)
    raise (Misc.Fatal ("html: unflushed DELAY"))
  end ;

  if !verbose > 2 then begin
    prerr_endline ("<= force_block: ["^string_of_block s^"]");
    pretty_cur !cur_out
  end


and close_block_loc pred s =
  if !verbose > 2 then
    prerr_string ("close_block_loc: '"^string_of_block s^"' = ");
  if not (pred ()) then begin
    if !verbose > 2 then prerr_endline "do it" ;
    force_block s "";
    true
  end else begin
    if !verbose > 2 then prerr_endline "forget it" ;
    force_block FORGET "";
    false
  end

and open_block s args =
  if !verbose > 2 then begin
    prerr_endline ("=> open_block '"^string_of_block s^"'"^" arg="^args);
    pretty_cur !cur_out ;
  end ;

  push_out out_stack (s,args,!cur_out) ;
  cur_out :=
    begin if false && is_group s then
        create_status_from_top !cur_out
      else
        create_status_from_scratch
          !cur_out.nostyle
          (let cur_mods = all_to_pending !cur_out in
           if flags.in_pre || is_pre s then filter_pre cur_mods else cur_mods)
    end ;
  try_open_block s args ;

  if !verbose > 2 then begin
    prerr_endline ("<= open_block '"^string_of_block s^"'");
    pretty_cur !cur_out ;
  end

and close_flow_loc check_empty s =
  if !verbose > 2 then
    prerr_endline ("close_flow_loc: "^string_of_block s) ;

  let active  = !cur_out.active
  and pending = !cur_out.pending in
  if close_block_loc check_empty s then begin
    !cur_out.pending <- to_pending pending active ;
    true
  end else begin
    !cur_out.pending <- to_pending pending active ;
    false
  end


let close_flow s =
  assert (s <> GROUP) ;
  if !verbose > 2 then
    prerr_flags ("=> close_flow '"^string_of_block s^"'");
  let _ = close_flow_loc check_empty s in
  if !verbose > 2 then
    prerr_flags ("<= close_flow '"^string_of_block s^"'")

let insert_block tag arg =
  begin match !cur_out.top with
    | Nothing {top_pending=pending ; top_active=active} ->
        !cur_out.pending <- !cur_out.pending @ to_pending pending active ;
        assert (!cur_out.active = []) ;
        !cur_out.top <- Insert (false,[])
    | Activate {top_pending=pending ; top_active=active} ->
        let add_active = activate "insert_block" pending @ active in
        !cur_out.active <- !cur_out.active @ add_active ;
        !cur_out.top <- Insert (true,to_pending [] add_active)
    | Closed (_,n) ->
        Out.erase_start n !cur_out.out ;
        !cur_out.top <- Insert (false,[])
    | ActivateClosed {top_active=active ; top_pending=pending}->
        !cur_out.top <- Insert (false,to_pending pending active)
    | NotMe
    | Insert _ -> ()
  end ;
  flags.insert <- Some (tag,arg)

let insert_attr tag attr =
  match tag,flags.insert_attr with
    | TD, Some (TR,_) -> ()
    | _, _ -> flags.insert_attr <- Some (tag,attr)

let close_block  s =
  let _ = close_block_loc check_empty s in
  ()

let erase_block s =
  if !verbose > 2 then begin
    Printf.fprintf stderr "erase_block: %s" (string_of_block s);
    prerr_newline ()
  end ;
  try_close_block s ;
  let ts,_,tout = pop_out out_stack in
  if ts <> s then failclose "erase_block" s ts;
  cur_out := tout


let open_group ss =
  let e = Style ss in
  if no_opt || (ss  <> "" && (not flags.in_pre || (ok_pre e))) then begin
    open_block INTERN "" ;
    if ss <> "" then
      !cur_out.pending <- !cur_out.pending @ [e]
  end else
    open_block GROUP ""

and open_aftergroup f =
  open_block AFTER "" ;
  flags.empty <- false ;
  push stacks.s_after f

and close_group () = match pblock () with
  | INTERN -> close_block INTERN
  | DFLOW ->  close_block DFLOW
  | AFTER  -> force_block AFTER ""
  | _      -> close_block GROUP

and erase_group () = match pblock () with
  | INTERN -> erase_block INTERN
  | DFLOW ->  erase_block DFLOW
  | AFTER  -> erase_block AFTER
  | _      -> erase_block GROUP



(* output requests  *)
let is_blank = function
  | ' ' | '\n' -> true
  | _ -> false

let put s =
  let block = pblock () in
  match block with
    | TABLE|TR -> ()
    | _ ->
        let s_blank =
          let r = ref true in
          for i = 0 to String.length s - 1 do
            r := !r && is_blank (String.unsafe_get s i)
          done ;
          !r in
        do_pending () ;
        flags.empty <- false;
        flags.blank <- s_blank && flags.blank ;
        do_put s

let put_char c =
  let s = pblock () in
  match s with
    | TABLE|TR -> ()
    | _ ->
        let c_blank = is_blank c in
        do_pending () ;
        flags.empty <- false;
        flags.blank <- c_blank && flags.blank ;
        do_put_char c


let flush_out () = Out.flush !cur_out.out

let skip_line () =
  flags.vsize <- flags.vsize + 1 ;
  put "<br>\n"

let put_length which  = function
  | Pixel x -> put (which ^ string_of_int x ^ "px")
  | Char x -> put (which ^ string_of_int (Length.base_font_size * x) ^ "pt")
  | Percent x -> put (which ^ string_of_int x ^ "%")
  | Default -> ()
  | NotALength s -> raise (Misc.Fatal ("No-length '" ^ s ^ "' in outManager"))

let horizontal_line attr width height =
  open_block GROUP "";
  nostyle ();
  put "<hr";
  begin
    match attr with
    | "" -> ()
    | _ ->
       put_char ' ';
       put attr
  end;
  begin
    match width, height with
    | Default, Default ->
       put_char '>'
    | _, _ ->
       put " class=\"horizontal-rule\" style=\"";
       put_length "width:" width;
       if width <> Default then put ";";
       put_length "height:" height;
       put "\">"
  end;
  close_block GROUP

let line_in_table () =
  put "<hr class=\"hrule\">";
  flags.vsize <- flags.vsize - 1

let freeze f =
  push out_stack (Freeze f) ;
  if !verbose > 2 then begin
    prerr_string "freeze: stack=" ;
    pretty_stack out_stack
  end

let flush_freeze () = match top out_stack with
    Freeze f ->
      let _ = pop out_stack in
      if !verbose > 2 then begin
        prerr_string "flush_freeze" ;
        pretty_stack out_stack
      end ;
      f () ; true
  | _ -> false

let pop_freeze () = match top  out_stack with
    Freeze f ->
      let _ = pop out_stack in
      f,true
  | _ -> (fun () -> ()),false


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
  cur_out := with_new_out pout ;
  let mods = as_envs !cur_out.active !cur_out.pending in
  do_close_mods () ;
  do_open_block None s args ;
  Out.copy old_out.out !cur_out.out ;
  !cur_out.pending <- mods ;
  let r = !cur_out in
  cur_out := pout ;
  if !verbose > 2 then begin
    Out.debug stderr r.out ;
    prerr_endline "";
    prerr_flags "<= get_block"
  end ;
  r

let hidden_to_string f =
  (*
    prerr_string "to_string: " ;
    Out.debug stderr !cur_out.out ;
    prerr_endline "" ;
  *)
  let old_flags = copy_flags flags in
  open_block INTERN "" ;
  f () ;
  do_close_mods () ;
  let flags_now = copy_flags flags in
  let r = Out.to_string !cur_out.out in
  flags.empty <- true ;
  close_block INTERN ;
  set_flags flags old_flags ;
  r,flags_now

let to_string f =
  let r,_ = hidden_to_string f in
  r

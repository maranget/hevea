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

(* $Id: latexscan.mll,v 1.324 2012-06-05 14:55:39 maranget Exp $ *)


{

open Printf

module type S =
  sig
    (* external entry points *)
    val no_prelude : unit -> unit
    val translate_put_unicode : char -> (unit -> int) -> unit
    val translate_put_unicode_string : string -> unit
    val main : Lexing.lexbuf -> unit
    val expand_command : string -> Lexing.lexbuf -> unit
    val expand_command_no_skip : string -> Lexing.lexbuf -> unit
    val print_env_pos : unit -> unit

    (* additional resources needed for extension modules. *)
    val cur_env : string ref
    val new_env : string -> unit
    val close_env : string -> unit
    val echo_toimage : unit -> bool
    val echo_global_toimage : unit -> bool

    val fun_register : (unit -> unit) -> unit
    val newif_ref : string -> bool ref -> unit
    val top_open_block : string -> string -> unit
    val top_close_block : string -> unit
    val top_open_group : unit -> unit
    val top_close_group : unit -> unit
    val check_alltt_skip : Lexing.lexbuf -> unit
    val skip_pop : Lexing.lexbuf -> unit
(* 'def' functions for initialisation only *)
    val def_code : string -> (Lexing.lexbuf -> unit) -> unit
    val def_name_code : string -> (string -> Lexing.lexbuf -> unit) -> unit
    val def_fun : string -> (string -> string) -> unit
    val get_this_main : string -> string
    val get_this_arg_mbox : string Lexstate.arg -> string
    val get_prim_onarg : string Lexstate.arg -> string
    val check_this_main : string -> bool
    val get_prim : string -> string
    val get_prim_arg : Lexing.lexbuf -> string
    val get_prim_opt : string -> Lexing.lexbuf -> string
    val get_csname : Lexing.lexbuf -> string
  end

module Make
  (Dest : OutManager.S) (Image : ImageManager.S) =
struct
open Misc
open Parse_opts
open Element
open Lexing
open Latexmacros
open Save

open Lexstate
open MyStack
open Subst

let error_subst_top lxm =
  raise
    (Error (sprintf "macro argument '%s' at top level" lxm))
    
let sbool = function
  | false -> "false"
  | true  -> "true"



let last_letter name =
  let c = String.get name (String.length name-1) in
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
;;

let top_par n =
  if not (!display || !in_math) then Dest.par n
;;

let if_level = ref 0
;;

let cur_env = ref ""
and after = ref [] 
and stack_env = MyStack.create "stack_env"
;;

let echo_toimage () =  get_level () = 0 && top_level ()
and echo_global_toimage () = top_level ()

let stack_env_pretty () =  MyStack.pretty (fun (x,_,_) -> x) stack_env

let fun_register f =
  if get_level () > 0 then after := f :: !after
;;


let inc_size i =
  let n = Dest.get_fontsize () in
  let new_size =
    if n+i <= 1 then 1
    else if n+i >= 7 then 7
    else n+i in
  Dest.open_mod (Font new_size)
;;

(* Horizontal display *)
let pre_format = ref None

let top_open_display () =
  if !display then begin
    if !verbose > 1 then
       prerr_endline "open display" ;
    match !pre_format with
    | Some (Tabular.Align {Tabular.vert=s})   ->
        Dest.open_display_varg (sprintf "style=\"vertical-align:%s\"" s)
    | _ ->
        Dest.open_display ()        
  end

and top_item_display () =
  if !display then begin
    Dest.item_display ()
  end

and top_force_item_display () =
  if !display then begin
    Dest.force_item_display ()
  end
;;

let top_close_display () =
  if !display then begin
    Dest.close_display ()
  end


(* Latex environment stuff *)

let print_env_pos () =
  if MyStack.empty stack_env then begin
    prerr_endline "No Latex environement is pending"
  end else begin
    let _,_,pos = MyStack.pop stack_env in
    Location.print_this_pos pos ;
    prerr_endline ("Latex environment '"^ !cur_env^"' is pending")
  end
;;

let new_env env =
  Latexmacros.open_group () ;
  push stack_env (!cur_env, !after, Location.get_pos ()) ;
  cur_env := env ;
  after := [] ;
  if !verbose > 1 then begin
    Location.print_pos () ;
    fprintf stderr "Begin: %s (%d)\n" env (get_level ())
  end

let error_env close_e open_e =
  raise
    (Misc.Close
       ("Latex env error: '"^close_e^"' closes '"^open_e^"'"))

let close_env env  =
  if !verbose > 1 then begin
    fprintf stderr "End: %s (%d)\n" env (get_level ())
  end ;
  if env = !cur_env then begin  
    let e,a,_ = pop stack_env in    
    let af = !after in
(*    List.iter (fun f -> f ()) !after ; *)
    cur_env := e ;
    after := a ;
    Latexmacros.close_group () ;
    List.iter (fun f -> f ()) af
  end else
    error_env env !cur_env
;;

type env_saved =
    string * (unit -> unit) list *
      (string * (unit -> unit) list * Location.t) MyStack.saved

let env_check () = !cur_env, !after, MyStack.save stack_env

and env_hot (e,a,s) =
  cur_env := e ;
  after := a ;
  MyStack.restore stack_env s
        
type full_save =
    { hot : Hot.saved;
      location : Location.saved;
      env : env_saved ;
      lexstate : Lexstate.saved_lexstate ;
      dest : Dest.saved;
      get : Get.saved;
      aux : Auxx.saved;      
    }
;;

(* Complete internal check/hot start *)
let check_all () =
  {
   location = Location.check () ;
   env = env_check () ;
   hot = Hot.checkpoint () ;
   lexstate = Lexstate.check_lexstate () ;
   dest = Dest.check () ;
   get = Get.check () ;
   aux = Auxx.check () ;
  }

and hot_all s =
  Location.hot s.location ;
  env_hot s.env ;
  Lexstate.hot_lexstate s.lexstate ;
  Dest.hot s.dest ;
  Get.hot s.get ;
  Auxx.hot s.aux ;
  Hot.start s.hot ;
  ()
;;

(* Top functions for blocks *)

type array_type = {math : bool ; border : bool}
type in_table = Table of array_type | NoTable | Tabbing
;;

let cur_format = ref [||]
and stack_format = MyStack.create "stack_format"
and cur_col = ref 0
and stack_col = MyStack.create "stack_col"
and in_table = ref NoTable
and stack_table = MyStack.create_init "stack_table" NoTable
and first_col = ref false
and first_border = ref false
and stack_first = MyStack.create "stack_first"
and stack_first_b = MyStack.create "stack_first_b"
and in_multi = ref false
and stack_multi_flag = MyStack.create "stack_multi_flag"
and stack_multi = MyStack.create "stack_multi"
;;


let pretty_array_type = function
  | Table {math = m ; border = b} ->
      "Table math="^(if m then "+" else "-")^
      " border="^(if b then "+" else "-")
  | NoTable -> "NoTable"
  | Tabbing -> "Tabbing"

let prerr_array_state () =
  prerr_endline (pretty_array_type !in_table) ;
  prerr_string "  format:";
  Tabular.pretty_formats !cur_format ;
  prerr_endline "" ;
  prerr_endline ("  cur_col="^string_of_int !cur_col) ;
  prerr_endline ("  first_col="^
      (if !first_col then "true" else "false"))
;;

let save_array_state () =
  push stack_format !cur_format ;
  push stack_col !cur_col ;
  push stack_table !in_table ;
  push stack_first !first_col;
  push stack_first_b !first_border;
  push stack_multi_flag !in_multi ;
  in_multi := false ;
  if !verbose > 1 then begin
    prerr_endline "Save array state:" ;
    prerr_array_state ()
  end    

and restore_array_state () =
  in_table := pop stack_table ;
  cur_col := pop stack_col ;
  cur_format := pop stack_format ;
  first_col := pop stack_first ;
  first_border := pop stack_first_b;
  in_multi := pop stack_multi_flag ;
  if !verbose > 1 then begin
    prerr_endline "Restore array state:" ;
    prerr_array_state ()
  end  
;;

let top_open_block block args =
  push stack_table !in_table ;
  in_table := NoTable ;
  begin match block with
  | "pre" ->
      push stack_display !display ;
      if !display then begin
        Dest.item_display () ;
        display := false
      end ;
      Dest.open_block "pre" args
  | "display" ->
      push stack_display !display ;
      display := true ;
      Dest.open_display_varg args
  | "span@inline@block" ->
      Dest.open_block ~force_inline:true "span" args
  | "table" ->
      save_array_state () ;
      in_table := NoTable ;
      top_force_item_display () ;
      Dest.open_block "table" args
  | "tr" ->
      Dest.open_block "tr" args      
  | "td" ->
      Dest.open_block "td" args ;
      top_open_display ()
  | _ ->
      if !display then begin
        Dest.item_display () ; Dest.open_block block args ;
        Dest.open_display ()
      end else
        Dest.open_block block args
  end

and top_close_block_aux close_fun block =
  if !verbose > 2 then prerr_endline ("Top close: "^block) ;
  in_table := pop stack_table ;
  begin match block with
  | "pre" ->
      display := pop stack_display ;
      close_fun block ;
      top_item_display ()
  | "display" ->
      Dest.close_display () ;
      display := pop stack_display
  | "span@inline@block" ->
      close_fun "span"
  | "table" ->
      close_fun "table" ;
      top_force_item_display () ;
      restore_array_state ()
  | "tr" ->
      close_fun "tr"
  | "td" ->
      top_close_display () ;
      close_fun "td"
  | _ ->
      if !display then begin
        Dest.close_display () ; close_fun block ; Dest.item_display ()
      end else
        close_fun block
  end
;;

let top_close_block block = top_close_block_aux Dest.close_block block
and top_force_block block =
  top_close_block_aux (fun name -> Dest.force_block name "") block
and top_close_flow block = top_close_block_aux Dest.close_flow block

let top_open_group () = top_open_block "" "" ; new_env ""

and top_close_group () =
  if !cur_env = "*mbox" then begin
    top_close_block "" ;
    in_math := pop stack_in_math ; display := pop stack_display ;
    if !display then Dest.item_display () ;
    close_env "*mbox"
  end else begin
    top_close_block "" ;
    close_env ""
  end
;;

let start_mbox () =
  push stack_table !in_table ; in_table := NoTable ;
  push stack_in_math !in_math ; in_math := false ;
  if !display then Dest.item_display () ;
  push stack_display !display ; display := false ;
  Dest.open_block "" "" ;
  new_env "*mbox"
;;

let get_fun_result f lexbuf =
   if !verbose > 1 then
    prerr_endline ("get_fun") ;
  let r = Dest.to_string (fun () ->
    top_open_group () ;
    Dest.nostyle () ;
    f lexbuf ;
    top_close_group ()) in
  if !verbose > 1 then begin
    prerr_endline ("get_fun -> '"^r^"'")
  end ;
  r


let do_get_this start_lexstate restore_lexstate
    make_style  lexfun {arg=s ; subst=subst} =
  start_lexstate subst;
  if !verbose > 1 then
    prerr_endline ("get_this : '"^s^"'") ;  
  verbose := !verbose - 1;
  let lexer = MyLexing.from_string s in
  let r = Dest.to_string (fun () ->
    if !display then  Dest.open_display () ;
    top_open_group () ;
    make_style () ;
    lexfun lexer ;
    top_close_group () ;
    if !display then Dest.close_display ()) in
  verbose := !verbose + 1 ;
  if !verbose > 1 then begin
    prerr_endline ("get_this '"^s^"' -> '"^r^"'")
  end ;
  restore_lexstate () ;
  r

let do_get_this_list start_lexstate restore_lexstate
    make_style  lexfun {arg=s ; subst=subst} =
  start_lexstate subst;
  if !verbose > 1 then
    eprintf "get_this_list get_this : '%a'\n" pretty_body s ;  
  verbose := !verbose - 1;
  let lexer = MyLexing.from_list s in
  let r = Dest.to_string (fun () ->
    if !display then  Dest.open_display () ;
    top_open_group () ;
    make_style () ;
    lexfun lexer ;
    top_close_group () ;
    if !display then Dest.close_display ()) in
  verbose := !verbose + 1 ;
  if !verbose > 1 then begin
    if !verbose > 1 then
      eprintf "get_this_list get_this : '%a' -> '%s'\n"
        pretty_body s r
  end ;
  restore_lexstate () ;
  r

let get_this_arg =
  do_get_this start_lexstate_subst restore_lexstate (fun () -> ())

and get_this_string main s =
  do_get_this start_lexstate_subst restore_lexstate (fun () -> ())
    main (string_to_arg s)

let more_buff = Out.create_buff ()
;;

let default_format =
  Tabular.Align
    {Tabular.hor="left" ; vert = "" ; wrap = false ;
      pre = "" ; post = "" ; width = Length.Default}
;;


let is_table = function
  | Table _ -> true
  | _       -> false

and is_noborder_table = function
  | Table {border = b} -> not b
  | _                  -> false

and is_border_table = function
  | Table {border = b} -> b
  | _ -> raise (Misc.Fatal "is_border_table")

and is_tabbing = function
  | Tabbing -> true
  | _ -> false

and math_table = function
  | Table {math = m} -> m
  | _ -> raise (Misc.Fatal "Array construct outside an array")
;;


exception EndInside
;;

let is_inside = function
    Tabular.Inside _ -> true
  | _ -> false

let is_border = function
  | Tabular.Border _ -> true
  | _ -> false

and as_wrap = function
  | Tabular.Align {Tabular.wrap = w} -> w
  | _ -> false

and as_pre = function
  | Tabular.Align {Tabular.pre=s} -> s
  | _ -> raise (Misc.Fatal "as_pre")

and as_post = function
  | Tabular.Align {Tabular.post=s} -> s
  | f -> raise (Misc.Fatal ("as_post "^Tabular.pretty_format f))
;;

let get_col format i =
  let r = 
    if i >= Array.length format+1 then
      raise (Misc.ScanError ("This array/tabular column has no specification"))
    else if i = Array.length format then default_format
    else format.(i) in
  if !verbose > 2 then begin
   fprintf stderr "get_col : %d: " i ;
   prerr_endline (Tabular.pretty_format r) ;
   prerr_string " <- " ;
   Tabular.pretty_formats format ;
   prerr_newline ()
  end ;
  r
;;

(* Paragraph breaks are different in tables *)
let par_val t =
  if is_table t then
    match get_col !cur_format !cur_col with
    | Tabular.Align {Tabular.wrap=false} -> None
    | _                          -> Some 0
  else
    Some 1

let show_inside main format i closing =
(*
  if !verbose > -1 then begin
    prerr_string ("show_inside: "^string_of_int i)
  end ;
*)
  let t = ref i in
  begin try while true do
    begin match get_col format !t with
      Tabular.Inside s ->
        let saved_table = !in_table in
        if math_table saved_table then
          scan_this main "$"
        else
          scan_this main "{" ;
        let s = get_this_string main s in
        if math_table saved_table then
          scan_this main "$"
        else
          scan_this main "}" ;
	Dest.make_inside s !in_multi;
    | Tabular.Border s -> 
	Dest.make_border s;
	if !first_border then first_border := false;
    | _ -> raise EndInside
    end ;
    t := !t+1
  done with EndInside ->
    if (!t = i) && (closing || !first_border)  then
      Dest.make_border " ";
  end ;
(*
  if !verbose > -1 then
    prerr_endline (" -> "^string_of_int !t) ;
*)
  !t
;;

let rec eat_inside format i b insides =
  if i >= Array.length format then (i , b , insides)
  else begin
    let f = get_col format i in
    if is_inside f then
      eat_inside format (i+1) b (insides+1)
    else if is_border f then
      eat_inside format (i+1) (b+1) insides
    else i, b, insides
  end
;;

let rec find_end n format i b insides = match n with
  0 -> eat_inside format i b insides
| _ ->
   let f = get_col format i in
   if is_inside f then
     find_end n format (i+1) b (insides +1)
   else if is_border f then
     find_end n format (i+1) (b+1) insides
   else
     find_end (n-1) format (i+1) b insides
;;


let find_start i = if !first_border then 0 else i


let next_no_border format n =
  let t = ref n in
  while is_border (get_col format !t) do
    t:= !t+1
  done;
  !t
;;


let do_open_col main format span insides =
  let save_table = !in_table in
  Dest.open_cell format span insides (is_border_table !in_table);
  if not (as_wrap format) && math_table !in_table then begin
    display  := true ;
    Dest.open_display ()
  end ;
  if math_table !in_table && not (as_wrap format) then begin
    scan_this main "$"
  end else
    scan_this main "{" ;
  pre_format := Some format ;
  scan_this main (as_pre format) ;
  pre_format := None ;
  in_table := save_table 

let open_col main  =
  let _ = Dest.forget_par () in
  Dest.open_cell_group () ;
  cur_col :=  show_inside main !cur_format !cur_col false;
  let format = (get_col !cur_format !cur_col) in
  do_open_col main format 1 0
;;

let open_first_col main =
  first_col := true ;
  first_border := true;
  open_col main  
;;

let erase_col main =
  let old_format = get_col !cur_format !cur_col in
  scan_this main (as_post old_format) ;
  if math_table !in_table  && not (as_wrap old_format) then
    scan_this main "$"
  else
    scan_this main "}" ;
  if !display then begin
    Dest.close_display () ;
    display := false
  end ;
  Dest.erase_cell () ;
  Dest.erase_cell_group ()
;;


let open_row () =
  cur_col := 0 ;
  Dest.new_row ()

and close_row () = Dest.close_row ()
;;


let do_hline main =
  if !verbose > 2 then begin
    fprintf stderr "hline: %d %d" !cur_col (Array.length !cur_format) ;
    prerr_newline ()
  end ;
  erase_col main ;
  Dest.erase_row () ;  
  Dest.make_hline (Array.length !cur_format) (is_noborder_table !in_table);
  open_row () ;
  open_first_col main
;;

let do_multi n format main =
  if !verbose > 2 then begin
    prerr_string
      ("multicolumn: n="^string_of_int n^" format:") ;
    Tabular.pretty_formats format ;
    prerr_endline ""
  end ;

  erase_col main ;

  let start_span = find_start !cur_col
  and k,b,insides = find_end n !cur_format !cur_col 0 0 in
  let end_span = k - b in

  in_multi := true;

  let i = show_inside main format 0 true in

  Dest.open_cell_group () ;
  do_open_col main (get_col format i) (end_span - start_span) insides ;
  push stack_multi (!cur_format,k) ;
  cur_format := format ;
  cur_col := i ;
;;


let close_col_aux main content is_last =
  let old_format = get_col !cur_format !cur_col in
  scan_this main (as_post old_format) ;
  if math_table !in_table && not (as_wrap old_format) then
    scan_this main "$"
  else
    scan_this main "}" ;
  if !display then begin
    Dest.close_display () ;
    display := false
  end ;
  if is_last && Dest.is_empty () then Dest.erase_cell ()
  else begin
    if !in_multi then begin
      let _ = show_inside main !cur_format (!cur_col+1) true in
      in_multi := false ;
      let f,n = pop stack_multi in
      cur_format := f ;
      cur_col := next_no_border f n;
      cur_col := show_inside main !cur_format !cur_col false;
    end else begin
      cur_col := !cur_col + 1;
      cur_col := show_inside main !cur_format !cur_col true;
    end;
    Dest.close_cell content;
    if !first_col then begin
      first_col := false;
      first_border := false;
    end
  end ;
  Dest.close_cell_group ()
;;

let close_col main content = close_col_aux main content false
and close_last_col main content = close_col_aux main content true

and close_last_row () =
  if !first_col then
    Dest.erase_row ()
  else
    Dest.close_row ()
;;

(* Compute functions *)

let get_style lexfun {arg=s ; subst=env} =
  start_normal env ;
  let lexer = MyLexing.from_string s in
  let r = Dest.to_style (fun () -> lexfun lexer) in
  end_normal () ;
  r

(* Image stuff *)

let stack_entry = MyStack.create "stack_entry"
and stack_out = MyStack.create  "stack_out"
;;

let start_other_scan env lexfun lexbuf =
  if !verbose > 1 then begin
    prerr_endline ("Start other scan ("^env^")") ;
    stack_env_pretty () ;
    prerr_endline ("Current env is: '"^ !cur_env^"'") ;
    pretty (fun x -> x) stack_entry
  end;
  save_lexstate () ;
  push stack_entry env ;
  rev stack_entry ;
  lexfun lexbuf
;;

let start_image_scan s image lexbuf =
  start_other_scan "toimage" (fun b -> Image.dump s image b) lexbuf
;;

let complete_scan main lexbuf =
  main lexbuf ;
  close_env (pop stack_out) ;
  top_close_block "" ;
  if !verbose > 1 then begin
    prerr_endline "Complete scan" ;
    stack_env_pretty () ;
    prerr_endline ("Current env is: '"^ !cur_env^"'")
  end
;;


let stop_other_scan comment main lexbuf =
  if !verbose > 1 then begin
    prerr_endline "Stop image: env stack is" ;
    stack_env_pretty () ;
    prerr_endline ("Current env is: '"^ !cur_env^"'")
  end;
  let _ = pop stack_entry in
  if not comment then close_env !cur_env ;
  if not (MyStack.empty stack_out) then begin
    complete_scan main lexbuf ;
    while not (MyStack.empty stack_out) do
      let lexbuf = previous_lexbuf () in
      complete_scan main lexbuf
    done
  end ;
  restore_lexstate ()
;;

let includes_table = Hashtbl.create 17
and check_includes = ref false
;;

let add_includes l =
  check_includes := true ;
  List.iter (fun x -> Hashtbl.add includes_table x ()) l
;;


let check_include s =
  not !check_includes ||
  begin  try
    Hashtbl.find includes_table s ; true
  with Not_found -> false
  end
;;


let mk_out_file () = match Parse_opts.name_out,!Parse_opts.destination with
| (""|"-"), Parse_opts.Info ->  Out.create_buff ()
| (""|"-"), _ -> Out.create_chan stdout
| x , Parse_opts.Info -> Out.create_chan (open_out (x^".tmp"))
| x , _  -> Out.create_chan (open_out x)
;;

let no_prelude () =
  if !verbose > 1 then prerr_endline "Filter mode" ;
  flushing := true ;
  let _ = Dest.forget_par () in () ;
  Dest.set_out (mk_out_file ())
;;

let macro_depth = ref 0
;;

let rec expand_toks main = function
  | [] -> ()
  | s::rem ->
      expand_toks main rem ;
(*      eprintf "Executing tokens: '%s'\n" s ; *)
      scan_this main s

let rec do_expand_command main skip_blanks name lexbuf =
  let loca = Location.get_pos () in
  try
    if !verbose > 1 then begin
      fprintf stderr "expand_command: '%s'\n" name
    end ;
    let cur_subst = get_subst () in
    let exec =
      if !alltt_loaded then
        function
          | Subst body ->
              if !verbose > 2 then begin
                eprintf "user macro: [%a]\n%!" Lexstate.pretty_body body
              end ;
              let old_alltt = !alltt in
              MyStack.push stack_alltt old_alltt ;        
              alltt :=
                 (match old_alltt with
                 | Not -> Not
                 | _   -> Macro) ;
(*
  fprintf stderr
  "Enter: %s, %s -> %s\n" name (debug old_alltt) (debug !alltt) ;
  *)
              scan_this_list_may_cont main lexbuf cur_subst (string_to_arg body) ;
              let _ =  MyStack.pop stack_alltt in
              alltt :=
                  (match old_alltt, !alltt with
                 | Not, Lexstate.Inside         -> Inside
                 | (Macro|Lexstate.Inside), Not -> Not
                 | _, _                -> old_alltt)
(*
  fprintf stderr
  "After: %s, %s -> %s\n" name (debug old_alltt) (debug !alltt)
  *)
          | Toks l -> expand_toks main l            
          | CamlCode f -> f lexbuf
      else
        function
          | Subst body ->
              if !verbose > 2 then begin
                eprintf "user macro: [%a]\n%!" Lexstate.pretty_body body
              end ;
              scan_this_list_may_cont main lexbuf cur_subst (string_to_arg body)
          | Toks l -> expand_toks main l            
          | CamlCode f -> f lexbuf in

    let pat,body = Latexmacros.find name in
    let saw_par =
      if
        (if !in_math then Latexmacros.invisible name
        else
	  not (effective !alltt) &&
	  is_subst body && last_letter name)
      then begin
        if !verbose > 2 then
	  prerr_endline ("skipping blanks ("^name^")");
        skip_blanks lexbuf
      end else begin
        if !verbose > 2 then begin
	  prerr_endline ("not skipping blanks ("^name^")")
        end ;
        false
      end in
    let args = make_stack name pat lexbuf in
    if !verbose > 1 then begin
      eprintf "Expanding macro '%s' {%i}\n" name !macro_depth ;
      macro_depth := !macro_depth + 1
    end ;
    scan_body exec body args ;
    if !verbose > 1 then begin
      eprintf "Cont after macro '%s', display=%B\n" name !display ;
      macro_depth := !macro_depth - 1
    end ;
    if saw_par then do_expand_command main skip_blanks "\\par" lexbuf
  with
  | Misc.EndDocument|Misc.EndInput as e -> raise e
  | e ->
      Location.print_this_pos loca ;
      eprintf "Giving up command: %s\n" name ;
      raise e
;;

let count_newlines s =
  let l = String.length s in
  let rec c_rec i =
    if i >= l then 0
    else match s.[i] with
    | '\n'  -> 1 + c_rec (i+1)
    | _     ->  c_rec (i+1) in
  c_rec 0
;;

let check_case s = match !case with
| Lower ->  String.lowercase s
| Upper ->  String.uppercase s
| Neutral -> s

and check_case_char c = match !case with
| Lower -> Char.lowercase c
| Upper -> Char.uppercase c
| Neutral -> c


let translate_put_unicode c next =
  if !raw_chars then
    Dest.put_char c
  else begin
    let uni =
      try OutUnicode.translate_in c next
      with OutUnicode.CannotTranslate ->
        raise
          (Misc.ScanError
             (if Latexmacros.exists "\\inputencodingname" then
               sprintf
                 "Encoding %s failed on '%c'"
                    (match Latexmacros.find "\\inputencodingname" with
                    | _,Subst s -> Lexstate.body_to_string s
                    | _,_ -> assert false) c
             else
               sprintf
                  "Non-ascii '%c' in input, consider using package inputenc"
                  c)) in
    try Dest.put_unicode uni
    with Misc.CannotPut ->
      Misc.warning
        (sprintf
           "Cannot output unicode %s (%c)" (OutUnicode.show uni) c) ;
      Dest.put_char c
  end

let rec translate_next next = match next () with
| -1 -> ()
| c  ->
    translate_put_unicode (Char.chr c) next ;
    translate_next next

let translate_put_unicode_string s =
  let next = Misc.next_of_string s in
  translate_next next

let top_open_maths main dodo =  
  if !jaxauto then begin
    scan_this main "\\bgroup\\@nostyle" ;
    Dest.put (if dodo then "\\[" else "\\(")
  end else begin
    push stack_in_math !in_math ;
    in_math := true ;
    if !display then  Dest.item_display () ;
    push stack_display !display ;
    if dodo then begin
      display  := true ;
      Dest.open_maths dodo;
    end else begin
      Dest.open_maths dodo;
      top_open_display () ;
    end ;
    scan_this main "\\normalfont"
  end
      
and top_close_maths dodo =
  if !jaxauto then begin
    Dest.put (if dodo then "\\]" else "\\)") ;
    top_close_group ()
  end else begin
    in_math := pop stack_in_math ;
    if dodo then begin
      Dest.close_maths dodo
    end else begin
      top_close_display () ;
      Dest.close_maths dodo
    end ;
    display := pop stack_display ;
    if !display then begin
      Dest.item_display ()
    end
  end
;;

} 

let command_name =
  '\\' (( ['@''A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'] | "\\*")

let hexa = ['0'-'9''a'-'f']

(* Horreur malheur, enfin sait-on jamais (mauvais transcodage) *)
let newline = '\n' | ('\r' '\n')

rule  main = parse
(* comments *)
| '%'
    {do_expand_command main skip_blanks "\\@hevea@percent" lexbuf ;
      main lexbuf}

(* Paragraphs *)
| newline
    {do_expand_command main skip_blanks "\\@hevea@newline" lexbuf ;
      main lexbuf}
(* subscripts and superscripts *)
| '_'
    {do_expand_command main skip_blanks "\\@hevea@underscore" lexbuf ;
      main lexbuf}
| '^'
    {do_expand_command main skip_blanks "\\@hevea@circ" lexbuf ;
      main lexbuf}
(* Math mode *)
| "$" | "$$" as lxm
    {let dodo = lxm = "$$" in
     if effective !alltt || not (is_plain '$') then begin
       Dest.put lxm ;
       main lexbuf
     (* vicious case '$x$$y$' *)
    end else if dodo && not !display && !in_math then begin
      scan_this main "${}$" ;
      main lexbuf
    end else begin (* General case *)
      let math_env = if dodo then "*display" else "*math" in
      if !in_math then begin
	top_close_maths dodo ;
        close_env math_env
      end else begin
        new_env math_env ;
	top_open_maths main dodo ;
	if dodo then ignore (skip_blanks lexbuf)
      end ;
      if !jaxauto then begin
        injaxauto := (if dodo then JaxDisplay else JaxInline) ;
        inmathjax dodo lexbuf
      end else main lexbuf
    end }

(* Definitions of  simple macros *)
(* inside tables and array *)
| [' ''\n']* '&'
    {do_expand_command main skip_blanks "\\@hevea@amper" lexbuf ;
      main lexbuf}
(* Substitution  *)
| '#' ['1'-'9']
    {let lxm = lexeme lexbuf in
    begin if effective !alltt || not (is_plain '#') then
      Dest.put lxm
    else
      let i = Char.code lxm.[1] - Char.code '1' in
      begin try
        scan_arg
          (if !alltt_loaded then
            (fun arg ->
              let old_alltt = !alltt in
              alltt := MyStack.pop stack_alltt ;
              scan_this_list_may_cont main lexbuf (get_subst ()) arg ;
              alltt := old_alltt ;
              MyStack.push stack_alltt old_alltt)
          else
            (fun arg -> scan_this_list_may_cont main lexbuf (get_subst ()) arg))
          i
      with SubstTop -> error_subst_top lxm
      end
    end ;
    main lexbuf}
(* Commands *)
| command_name
    {let name = lexeme lexbuf in
    do_expand_command main skip_blanks name lexbuf ;
    begin match !injaxauto with
    | JaxOut -> main lexbuf
    | JaxInline -> inmathjax false lexbuf
    | JaxDisplay -> inmathjax true lexbuf
    end }
(* Groups *)
| '{'
    {do_expand_command main skip_blanks "\\@hevea@obrace" lexbuf ;
      main lexbuf} 
| '}' 
    {do_expand_command main skip_blanks "\\@hevea@cbrace" lexbuf ;
      main lexbuf} 
| eof {()}
| ' '+ as lxm
    {if effective !alltt then
       Dest.put lxm
     else
       begin
         if !display then
           Dest.put_hspace false (Length.Char (String.length lxm))
         else
	   Dest.put_char ' '
       end;
     main lexbuf}
(* Alphabetic characters *)
| ['a'-'z' 'A'-'Z']+ as lxm
    {let lxm = check_case lxm in
    if !in_math then begin
      scan_this main "{\\it" ;
      Dest.put lxm;
      scan_this main "}"
    end else
      Dest.put lxm ;
    main lexbuf}
(* Numbers *)
| ['0'-'9']+
    {let lxm = lexeme lexbuf in
    Dest.put lxm;
    main lexbuf}
(* Active characters *)
| '-'
    {do_expand_command main skip_blanks "\\@hevea@minus" lexbuf ;
      main lexbuf }
| '`'
    {do_expand_command main skip_blanks "\\@hevea@backquote" lexbuf ;
      main lexbuf } 
| '''
    {do_expand_command main skip_blanks "\\@hevea@quote" lexbuf ;
      main lexbuf } 
| '~'
    {do_expand_command main skip_blanks "\\@hevea@tilde" lexbuf ;
      main lexbuf }
(* Spanish stuff *)
| '?'
    {do_expand_command main skip_blanks "\\@hevea@question" lexbuf ;
      main lexbuf}
| '!'
    {do_expand_command main skip_blanks "\\@hevea@excl" lexbuf ;
      main lexbuf}
(* German stuff *)
| '"'
    {if is_plain '"' then 
      Dest.put_char '"' (* '"' *)
    else
      do_expand_command main skip_blanks "\\@hevea@dquote" lexbuf ;
      main lexbuf}
(* One character *)
| "^^" (hexa as lxm1) (hexa as lxm2)
   {let c = Char.chr (hexa_code lxm1 lxm2) in
   translate_put_unicode c (fun () -> read_lexbuf lexbuf) ;
   main lexbuf}
| _  as lxm
    {let lxm = check_case_char lxm in
    translate_put_unicode lxm (fun () -> read_lexbuf lexbuf) ;
    main lexbuf}

and read_lexbuf = parse
| eof      { -1 }
| "^^" (hexa as lxm1) (hexa as lxm2) { hexa_code lxm1 lxm2 }
| _ as lxm { Char.code lxm }

and complete_newline = parse
|  (' ' | newline)* as lxm { lxm }

and latex2html_latexonly = parse
| '%' + [ ' ' '\t' ] * "\\end{latexonly}" [ ^ '\n' ] * '\n'
    { () }
| _ 
    {latex2html_latexonly lexbuf}
| eof
    {fatal "End of file in latex2html_latexonly"}

and latexonly = parse
    '%'+ ' '* ("END"|"end") ' '+ ("LATEX"|"latex")  [^'\n']* '\n'
    {stop_other_scan true main lexbuf}
|  '%'+ ' '* ("HEVEA"|"hevea") ' '*
    {latexonly lexbuf}
|  '%'
    {latex_comment lexbuf ; latexonly lexbuf}
|  "\\end"
    {let {arg=arg} = save_arg lexbuf in
    if arg = "latexonly" then begin
      top_close_block "" ;
      stop_other_scan false main lexbuf
    end else if arg = top stack_entry then begin
      let _ = pop stack_entry in
      push stack_out arg ;
      begin match Latexmacros.find (end_env arg) with
        _,(Subst body) ->
          scan_this_list_may_cont latexonly lexbuf (get_subst ())
            (string_to_arg body)
      |  _,_ ->
          raise (Misc.ScanError ("Bad closing macro in latexonly: '"^arg^"'"))
      end
    end else
      latexonly lexbuf}
| command_name  | _ {latexonly lexbuf}
| eof
    {if empty stack_lexbuf then ()
    else begin
      let lexbuf = previous_lexbuf () in
      latexonly lexbuf
    end}

and latex_comment = parse
    '\n' | eof  {()}
  | [^'\n']+    {latex_comment lexbuf}

and image = parse
    '%'+ ' '* ("END"|"end") ' '+ ("IMAGE"|"image")  [^'\n']* '\n'
    {stop_other_scan true main lexbuf}
  |  '%'+ ' '* ("HEVEA"|"hevea") ' '*
      {image lexbuf}
  |  '%'
      {let lxm = lexeme lexbuf in
      Image.put lxm ;
      image_comment lexbuf ;
      image lexbuf}
(* Substitution in image *)
  | '#' ['1'-'9']
      {let lxm = lexeme lexbuf in
      let i = Char.code (lxm.[1]) - Char.code '1' in
      begin try scan_arg (scan_this_arg_list image) i
      with SubstTop -> error_subst_top lxm end ;
      image lexbuf}
  |  "\\end"
      {let lxm = lexeme lexbuf in
      Save.start_echo () ;
      let {arg=arg} = save_arg lexbuf in
      let true_arg = Save.get_echo () in
      if arg = "toimage" then begin
	top_close_block "" ;
	stop_other_scan false main lexbuf
      end else if arg = top stack_entry then begin
	let _ = pop stack_entry in
	push stack_out arg ;
	begin match Latexmacros.find (end_env arg) with
          _,(Subst body) ->
            scan_this_list_may_cont  image lexbuf (get_subst ())
              (string_to_arg body)
	|  _,_ -> raise (Misc.ScanError ("Bad closing macro in image: '"^arg^"'"))
	end
      end else begin
	Image.put lxm ; Image.put true_arg ;
	image lexbuf
      end}
  |  command_name as lxm
      {begin match lxm with
(* Definitions of  simple macros, bodies are not substituted *)
      | "\\def" | "\\gdef" ->
          Save.start_echo () ;
          skip_csname lexbuf ;
          ignore (skip_blanks lexbuf) ;
          let _ = Save.defargs lexbuf in
          Image.put lxm ;
          if (Lexstate.top_level()) then begin
            let _ = save_arg lexbuf in
            ()
          end ;
          let saved = Save.get_echo () in
          Image.put saved
      | "\\renewcommand" | "\\newcommand" | "\\providecommand"
      | "\\renewcommand*" | "\\newcommand*" | "\\providecommand*" ->
          Save.start_echo () ;
          let _ = save_arg lexbuf in
          let _ = save_opts ["0" ; ""] lexbuf in
          let _ = save_arg lexbuf in
          Image.put lxm ;
          let saved = Save.get_echo () in
          Image.put saved
      | "\\newenvironment" | "\\renewenvironment"
      | "\\newenvironment*" | "\\renewenvironment*" ->
          Save.start_echo () ;
          let _ = save_arg lexbuf in
          let _ = save_opts ["0" ; ""] lexbuf in
          let _ = save_arg lexbuf in
          let _ = save_arg lexbuf in
          Image.put lxm ;
          Image.put (Save.get_echo ())
      | _ -> Image.put lxm end ;
      image lexbuf}
  | _
      {let s = lexeme lexbuf in
      Image.put s ;
      image lexbuf}
  | eof
      {if empty stack_lexbuf then begin
	if not filter && top_lexstate () then
          raise (Misc.ScanError ("No \\end{document} found"))
      end else begin
	let lexbuf = previous_lexbuf () in
	image lexbuf
      end}


and image_comment = parse
    '\n' {Image.put_char '\n'}
  | eof  {()}
  | [^'\n']+
      {let lxm = lexeme lexbuf in
      Image.put lxm ;
      image_comment lexbuf}

and mbox_arg = parse
  | ' '+ | '\n' {mbox_arg lexbuf}
  | eof
      {if not (empty stack_lexbuf) then begin
	let lexbuf = previous_lexbuf () in
	if !verbose > 2 then begin
	  prerr_endline "Poping lexbuf in mbox_arg" ;
	  pretty_lexbuf lexbuf
	end ;
	mbox_arg lexbuf
      end else raise (Misc.ScanError "End of file in \\mbox argument")}
  | '{' | ("\\bgroup" ' '* '\n'? ' '*)
      {start_mbox ()}
  | ""
      {raise (Misc.ScanError "Cannot find a \\mbox argument here, use braces")}

and no_skip = parse
  | "" { false }

and skip_blanks_pop = parse
    ' '+ {skip_blanks_pop lexbuf}
  | '\n' {()}
  | ""   {()}
  | eof
      {if not (empty stack_lexbuf) then begin
	let lexbuf = previous_lexbuf () in
	if !verbose > 2 then begin
	  prerr_endline "Poping lexbuf in skip_blanks" ;
	  pretty_lexbuf lexbuf
	end ;
	skip_blanks_pop lexbuf
      end else ()}

and to_newline = parse
  |  '\n' {()}
  | _     {Out.put_char more_buff (Lexing.lexeme_char lexbuf 0) ;
            to_newline lexbuf}
  | eof
      {if not (empty stack_lexbuf) then
	let lexbuf = previous_lexbuf () in
	to_newline lexbuf}

and skip_blanks = parse
    ' '+ {skip_blanks lexbuf}
  | '\n' {more_skip lexbuf}
  | ""   { false }

and more_skip = parse
  | ' '+ { false }
  | (' '* '\n')+ ' '* { true }
  | "" { false }

and skip_spaces = parse
    ' ' * {()}
  | eof   {()}


and skip_false = parse
  |  '%'
      {if is_plain '%' then skip_comment lexbuf ;
	skip_false lexbuf}
  |  "\\ifthenelse"
      {skip_false lexbuf}
  |  "\\if" ['a'-'z' 'A'-'Z''@']+
      {if_level := !if_level + 1 ;
	skip_false lexbuf}
  | "\\else" ['a'-'z' 'A'-'Z''@']+
      {skip_false lexbuf}
  | "\\else"
      {if !if_level = 0 then begin
	if skip_blanks lexbuf then
	  do_expand_command main no_skip "\\par" lexbuf
      end else skip_false lexbuf}
  | "\\fi" ['a'-'z' 'A'-'Z']+
      {skip_false lexbuf}
  | "\\fi"
      {if !if_level = 0 then begin
        if skip_blanks lexbuf then
	  do_expand_command main no_skip "\\par" lexbuf
      end else begin
	if_level := !if_level -1 ;
	skip_false lexbuf
      end}
  | _  {skip_false lexbuf}
  | "" {raise
          (Misc.ScanError "End of entry while skipping TeX conditional macro")}

and comment = parse
|  ['%'' ']* ("BEGIN"|"begin") ' '+ ("IMAGE"|"image")
    {skip_comment lexbuf ; start_image_scan "" image lexbuf ; () }
(* Backward compatibility with latex2html *)
| [ ' ' '\t' ] * "\\begin{latexonly}"
    {latex2html_latexonly lexbuf }
| ['%'' ']* ("HEVEA"|"hevea") ' '*
   { () }
| ['%'' ']* ("BEGIN"|"begin") ' '+ ("LATEX"|"latex")
    { skip_to_end_latex lexbuf}
| ""
    { skip_comment lexbuf }

and skip_comment = parse    
|  [^ '\n']*
   {if !verbose > 1 then
     prerr_endline ("Comment:"^lexeme lexbuf) ;
   if !flushing then Dest.flush_out () }


and skip_to_end_latex = parse
| '%' ['%'' ']* ("END"|"end") ' '+ ("LATEX"|"latex")
    {skip_comment lexbuf ; skip_spaces lexbuf}
| _ 
    {skip_to_end_latex lexbuf}
| eof {fatal ("End of file in %BEGIN LATEX ... %END LATEX")}

and inmathjax dodo = parse
| "$$"|"\\]" as lxm
    { if dodo then begin
      top_close_maths true ;
      close_env "*display";
      injaxauto := JaxOut ;
      main lexbuf
    end else begin
      Dest.put lxm ;
      inmathjax dodo lexbuf
    end }
| '$'|"\\)" as lxm
    { if not dodo then begin
      top_close_maths false ;
      close_env "*math" ;
      injaxauto := JaxOut ;
      main lexbuf
    end else begin
      Dest.put lxm ;
      inmathjax dodo lexbuf
    end }
(* Substitution  *)
| '#' ['1'-'9']
    {let lxm = lexeme lexbuf in
    let i = Char.code lxm.[1] - Char.code '1' in
    begin try
      scan_arg
        (fun arg ->
          scan_this_list_may_cont (inmathjax dodo) lexbuf (get_subst ()) arg)
        i
    with SubstTop -> Dest.put lxm
    end ;
    inmathjax dodo lexbuf }
| _ as lxm
  { Dest.put_char lxm ; inmathjax dodo lexbuf }
| eof { () }

{

let () = ()
;;

let check_alltt_skip lexbuf =
  if not (effective !alltt) then begin
    if skip_blanks lexbuf then
      do_expand_command main no_skip "\\par" lexbuf
  end

and skip_pop lexbuf =
  save_lexstate () ;
  skip_blanks_pop lexbuf ;
  restore_lexstate ()
;;

let def_code name f = def_init name f
let def_name_code name f = def_init name (f name)
let expand_command name lexbuf = do_expand_command main skip_blanks name lexbuf
and expand_command_no_skip name lexbuf = do_expand_command main no_skip name lexbuf
;;

(* Direct display math, no env opened *)
def_code "\\displaymath"
  (fun lexbuf -> top_open_maths main true ; skip_pop lexbuf) ;
def_code "\\enddisplaymath"
  (fun _lexbuf -> top_close_maths true) ;
()
;;

def_code "\\@skip@blanks" (fun lexbuf -> skip_pop lexbuf)
;;

def_code "\\@hevea@percent"
    (fun lexbuf ->
      if effective !alltt || not (is_plain '%') then begin
        let lxm = lexeme lexbuf in
        Dest.put lxm ;
        main lexbuf
      end else begin
	comment lexbuf ;
        if skip_blanks lexbuf then
	  do_expand_command main no_skip "\\par" lexbuf
      end)
;;

def_code "\\@hevea@newline"
    (fun lexbuf ->
      let lxm = complete_newline lexbuf in
      let nlnum = count_newlines lxm in
      if !Lexstate.withinLispComment
      then begin
        if !verbose > 2 then prerr_endline "NL caught after LispComment" ;
        raise (Misc.EndOfLispComment nlnum) (* QNC *)
      end else begin
        if effective !alltt then begin
          Dest.put_char '\n' ;
          Dest.put lxm
        end else if nlnum >= 1 then
          expand_command "\\par" lexbuf
        else
          Dest.put_separator ()
       end)
;;

let warn_under = ref true
;;

let sub_sup lxm lexbuf =
  if effective !alltt || not (is_plain lxm) then Dest.put_char lxm
  else if not !in_math then begin
    if !warn_under then
      warning ("'"^Char.escaped lxm^"' occurring outside math mode") ;
    Dest.put_char lxm
  end else begin
    let sup,sub = match lxm with
      '^' ->
        let sup = save_arg lexbuf in
        let sub = save_sub lexbuf in
        sup,unoption sub
    | '_'   ->
        let sub = save_arg lexbuf in
        let sup = save_sup lexbuf in
        unoption sup,sub
    | _ -> assert false in
    Dest.standard_sup_sub (scan_this_arg main) (fun () -> ()) sup sub !display
  end
;;

def_code "\\@hevea@underscore" (fun lexbuf -> sub_sup '_' lexbuf) ;
def_code "\\@hevea@circ" (fun lexbuf -> sub_sup '^' lexbuf)
;;

def_code "\\mathop"
  (fun lexbuf ->
    let symbol = save_arg lexbuf in
    let {limits=limits ; sup=sup ; sub=sub} = save_sup_sub lexbuf in
    begin match limits with
    | (Some Limits|None) when !display ->
        Dest.limit_sup_sub
          (scan_this_arg main)
          (fun _ -> scan_this_arg main symbol) sup sub !display
    | (Some IntLimits) when !display ->
        Dest.int_sup_sub true 3
          (scan_this_arg main)
          (fun () -> scan_this_arg main symbol)
          sup sub !display        
    | _ ->
        scan_this_arg main symbol ;
        Dest.standard_sup_sub
          (scan_this_arg main)
          (fun _ -> ()) sup sub !display
    end) ;
def_code "\\@mathop"
  (fun lexbuf ->
    let symbol = save_arg lexbuf in
    let {limits=limits ; sup=sup ; sub=sub} = save_sup_sub lexbuf in
    begin match limits with
    | (Some Limits) when !display ->
        Dest.limit_sup_sub
          (scan_this_arg main)
          (fun _ -> scan_this_arg main symbol) sup sub !display
    | (Some IntLimits|None) when !display ->
        Dest.int_sup_sub true 3
          (scan_this_arg main)
          (fun () -> scan_this_arg main symbol)
          sup sub !display        
    | _ ->
        scan_this_arg main symbol ;
        Dest.standard_sup_sub
          (scan_this_arg main)
          (fun _ -> ()) sup sub !display
    end)
;;


def_code "\\@hevea@obrace"
    (fun _ ->
      if !activebrace && is_plain '{' then
        top_open_group ()
      else begin
        Dest.put_char '{'
      end) ;

def_code "\\bgroup"
    (fun lexbuf ->
      top_open_group () ;
      check_alltt_skip lexbuf)
;;

def_code "\\@hevea@cbrace"
    (fun _ ->
      if !activebrace && is_plain '}' then begin
        top_close_group ()
      end else begin
        Dest.put_char '}'
      end) ;
def_code "\\egroup"
    (fun lexbuf ->
      top_close_group () ;
      check_alltt_skip lexbuf)
;;


def_code "\\@hevea@tilde"
  (fun _lexbuf ->
    if effective !alltt || not (is_plain '~') then
      Dest.put_char '~'
    else
      Dest.put_hspace true (Length.Char 1))
;;

def_code "\\@hevea@question"
  (fun lexbuf ->
    if if_next_char '`' lexbuf then begin
      gobble_one_char lexbuf ;
      if effective !alltt then Dest.put "?`"
      else
        Dest.put_unicode OutUnicode.iques
    end else
      Dest.put_char  '?')
;;
def_code "\\@hevea@excl"
  (fun lexbuf ->
     if if_next_char '`' lexbuf then begin
       gobble_one_char lexbuf ;
       if effective !alltt then Dest.put "!`"
       else Dest.put_unicode OutUnicode.iexcl 
     end else
       Dest.put_char '!')
;;

def_code "\\@hevea@dquote" (fun _lexbuf -> Dest.put_char '"') (* '"' *)
;;



let get_this_main arg = get_this_string main arg

let check_this_main s =
  if !verbose > 1 then
    prerr_endline ("check_this: '"^s^"'");
  start_normal (get_subst ()) ;
  Dest.open_block "temp" "";
  let r =
    try
      scan_this main s ;
      true
    with
    |  _ -> false in
  Dest.erase_block "temp" ;
  end_normal () ;
  if !verbose > 1 then
    prerr_endline ("check_this: '"^s^"' = "^sbool r);
  r

(* '"' *)

let do_get_prim_onarg f arg =
  let plain_sub = is_plain '_'
  and plain_sup = is_plain '^'
  and plain_dollar = is_plain '$'
  and plain_amper = is_plain '&'
  and plain_quote = is_plain '\''
  and plain_backquote = is_plain '`'
  and plain_minus = is_plain '-'
  and plain_dquote = is_plain '"' in
  unset_plain '_' ; unset_plain '^' ; unset_plain '$' ; unset_plain '&' ;
  unset_plain '\'' ; unset_plain '`' ; unset_plain  '-' ;
  set_plain '"' ; (* For double quotes, plain is inactive... *)
  let old_raw = !raw_chars in
  let old_case = !case in
  case := Neutral ;
  raw_chars := true ;
  let restore () =
    raw_chars := old_raw ;
    case := old_case ;
    plain_back plain_sub '_' ; plain_back plain_sup '^' ;
    plain_back plain_dollar '$' ; plain_back plain_amper '&' ;
    plain_back plain_quote '\'' ; plain_back plain_backquote  '`' ;
    plain_back plain_minus '-' ; plain_back plain_dquote '"' ; (* '"' *) in    
  try
    let r = f start_normal end_normal Dest.nostyle main arg in
    restore () ;
    r
  with e -> restore () ; raise e

let get_prim_onarg arg = do_get_prim_onarg do_get_this arg
let get_prim_onarg_list arg = do_get_prim_onarg do_get_this_list arg


let get_prim s = get_prim_onarg (string_to_arg s)

let get_prim_arg lexbuf =
  let arg = save_arg lexbuf in
  get_prim_onarg arg

and get_prim_opt def lexbuf =
  let arg = save_opt def lexbuf in
  get_prim_onarg_list arg


let get_csname lexbuf =
  let r = 
    protect_save_string
      (fun lexbuf -> Save.csname get_prim Subst.subst_this lexbuf)
      lexbuf in
(*  eprintf "GET CSNAME: '%s'\n" r ; *)
  r


let def_fun name f =
  def_code name
    (fun lexbuf ->
      let arg = subst_arg lexbuf in
      scan_this main (f arg))
;;

(* Paragraphs *)
let do_unskip () =
 let _ = Dest.forget_par () in
 Dest.unskip ()
;;

def_code "\\unskip"
    (fun lexbuf ->
      do_unskip () ;
      check_alltt_skip lexbuf)
;;

def_code "\\hva@par"
  (fun lexbuf ->
    if !display || !in_math then begin
      warning "\\par in display or math mode"
    end else begin match par_val !in_table with
      | None ->
          Dest.put_char ' '
      | pval ->
          top_par pval
    end ;
    check_alltt_skip lexbuf)

;;


(********************************)
(* Tokens to be executed at eof *)
(********************************)

(* Defined as a token register in latexcommon.hva *)
let ateof = "\\@ateof"


let after_input lexbuf old_toks =
  begin try
    expand_command_no_skip ateof lexbuf
  with e ->
    Misc.warning
      (sprintf "Exception '%s' raised by input handler"
	 (Printexc.to_string e))
  end ;
  ignore (Latexmacros.replace ateof old_toks)

(* NB: masks Lexstate.input_file *)
let input_file loc_verb main filename lexbuf  =
  let old_toks = Latexmacros.replace ateof (Some (zero_pat, Toks [])) in
  begin try
    Lexstate.input_file loc_verb main filename ;
  with e ->
    after_input lexbuf old_toks ;
    raise e
  end ;
  after_input lexbuf old_toks
  
	      
(* Styles and packages *)
let saw_doc = "\\hva@doc"
    
let do_documentclass command lexbuf =
  if Latexmacros.exists saw_doc then
    raise (Misc.ScanError (sprintf "Multiple occurrences of %s" command)) ;
  Latexmacros.global_def saw_doc zero_pat (Subst []) ;
  Save.start_echo () ;
  let {arg=opt_arg} = save_opt "" lexbuf in
  let {arg=arg} =  save_arg lexbuf in
  let real_args = Save.get_echo () in
  begin try if not !styleloaded then
    input_file (if !verbose > 0 then 1 else 0) main (arg^".hva") lexbuf
  with
    Myfiles.Except | Myfiles.Error _ ->
      raise (Misc.ScanError ("No base style"))
  end ;
  if command = "\\documentstyle" then begin
    List.iter
      (fun pack -> scan_this main ("\\usepackage{"^pack^"}"))
      (Save.cite_arg
         (MyLexing.from_list ("{"::opt_arg@["}"])))
  end  else if  command = "\\documentclass"  then begin
    let opts = String.concat "" opt_arg in
    scan_this main
      (sprintf "\\sbox{\\@document@opts}{%s}" opts)
  end ;
(* Save gobal options *)
  Image.start () ;
  Image.put "\\newif\\ifimagen\\imagentrue\n" ;
  Image.put command ;
  Image.put real_args ;
  Image.put_char '\n' ;
  Dest.set_out (mk_out_file ()) ;
  Dest.stop ()
;;

def_name_code  "\\documentstyle" do_documentclass ;
def_name_code  "\\documentclass" do_documentclass
;;


let do_input lxm lexbuf =
  Save.start_echo () ;
  let arg = get_prim_arg lexbuf in
  let echo_arg = Save.get_echo () in
  if lxm <> "\\include" || check_include arg then begin
      try input_file !verbose main arg lexbuf
      with
      | Myfiles.Except ->
          Image.put lxm ;
          Image.put echo_arg ;
          Image.put "\n"
      | Myfiles.Error _ -> ()
  end
;;

def_code "\\input" (do_input "\\input") ;
def_code "\\include" (do_input "\\include") ;
;;

(* Command definitions *)

let do_newcommand lxm lexbuf =
  Save.start_echo () ;
  let name = get_csname lexbuf in
  let nargs = save_opts ["0" ; ""] lexbuf in
  let body = subst_body lexbuf in
  let echo () =
    if echo_toimage () && lxm <> "\\@forcecommand" then begin      
      Image.put lxm ;
      Image.put (Save.get_echo ()) ;
      Image.put_char '\n'
    end in
  let nargs,(def,defval) = match nargs with
    [a1 ; a2] ->
      Get.get_int (from_ok a1),
      (match a2 with
      | {arg=No s ; subst=env} -> false,mkarg [s] env
      | {arg=Yes s ; subst=env} -> true,mkarg s env)
  | _ -> assert false in
  let pat =
    latex_pat
      (if def then [do_subst_this_list defval] else []) nargs in
  match lxm with
  | "\\@forcecommand" -> Latexmacros.def name pat (Subst body)
  | "\\newcommand"|"\\newcommand*"    ->
      echo () ;
      if Latexmacros.exists name then
        warning ("Ignoring (re-)definition of '"^name^"' by \\newcommand")
      else begin
        Latexmacros.def name pat (Subst body)
      end
  | "\\renewcommand"|"\\renewcommand*" ->
      if not (Latexmacros.exists name) then begin
        warning ("Defining '"^name^"' by \\renewcommand")
      end else
        echo () ;
      Latexmacros.def name pat (Subst body)
  | _                ->
      echo () ;
      if not (Latexmacros.exists name) then
        Latexmacros.def name pat (Subst body)
;;

def_name_code "\\renewcommand" do_newcommand  ;
def_name_code "\\renewcommand*" do_newcommand  ;
def_name_code "\\newcommand" do_newcommand ;
def_name_code "\\newcommand*" do_newcommand ;
def_name_code "\\providecommand" do_newcommand ;
def_name_code "\\providecommand*" do_newcommand ;
def_name_code "\\@forcecommand" do_newcommand
;;

def_name_code "\\newcolumntype"
  (fun lxm lexbuf ->
    Save.start_echo () ;
    let old_raw = !raw_chars in
    raw_chars := true ;
    let name = get_prim_arg lexbuf in
    raw_chars := old_raw ;
    let nargs = save_opt "0" lexbuf in
    let body = subst_body lexbuf in
    let rest = Save.get_echo () in
    if echo_toimage () then
      Image.put (lxm^rest^"\n") ;
    let col_cmd = Misc.column_to_command name in
    if Latexmacros.exists col_cmd then
      warning
        ("Not (re)-defining column type '"^name^"' with \\newcolumntype")
    else
      Latexmacros.def
        col_cmd
        (latex_pat [] (Get.get_int nargs))
        (Subst body))
;;

let do_newenvironment lxm lexbuf =
  Save.start_echo () ;
  let name = get_prim_arg lexbuf in
  let nargs,optdef = match save_opts ["0" ; ""] lexbuf with
  |  [x ; y ] -> x,y
  | _ -> assert false in
  let body1 = subst_body lexbuf in
  let body2 = subst_body lexbuf in
  if echo_toimage () then
    Image.put (lxm^Save.get_echo ()^"\n") ;

  let do_defs () =
    Latexmacros.def
      (start_env name)
      (latex_pat
         (match optdef with
         | {arg=No _}    -> []
         | {arg=Yes s ; subst=env} -> [do_subst_this_list (mkarg s env)])
         (match nargs with 
         | {arg=No _} -> 0
         | {arg=Yes s ; subst=env} -> Get.get_int (mkarg s env)))
      (Subst body1) ;
    Latexmacros.def (end_env name)  zero_pat (Subst body2) in
         
  if lxm = "\\newenvironment" || lxm = "\\newenvironment*" then
    if
      Latexmacros.exists (start_env name) ||
      Latexmacros.exists (end_env name)
    then
      warning
        ("Not (re)-defining environment '"^name^"' with "^lxm)
    else
      do_defs ()
  else begin
    if
      not (Latexmacros.exists (start_env name) &&
           Latexmacros.exists (end_env name))
    then
      warning
        ("Defining environment '"^name^"' with "^lxm) ;
    do_defs ()
  end
;;

def_name_code "\\newenvironment" do_newenvironment ;
def_name_code "\\newenvironment*" do_newenvironment ;
def_name_code  "\\renewenvironment" do_newenvironment ;
def_name_code  "\\renewenvironment*" do_newenvironment
;;

let do_newcounter name within =
  try
    Counter.def_counter name within ;
    Latexmacros.global_def
      ("\\the"^name) zero_pat (Subst (["\\arabic{"^name^"}"]))
  with
  | Failed -> ()

let do_newtheorem lxm lexbuf =
  Save.start_echo () ;
  let name = get_prim_arg lexbuf in
  let numbered_like = match save_opts [""] lexbuf with
  |  [x] -> x
  | _ -> assert false in
  let caption = subst_arg lexbuf in
  let within =  match save_opts [""] lexbuf with
  | [x] -> x
  | _   -> assert false in
  if echo_global_toimage () then
    Image.put (lxm^Save.get_echo ()^"\n") ;
  let cname = match numbered_like,within with
    {arg=No _},{arg=No _} ->
      do_newcounter  name "" ; name
  | _,{arg=Yes _} ->
      let within = get_prim_onarg_list (from_ok within) in
      do_newcounter name within ; name
  | {arg=Yes _},_ ->
      get_prim_onarg_list (from_ok numbered_like) in
  do_expand_command main no_skip ("\\set@th")
    (MyLexing.from_string ("{"^name^"}")) ;
  Latexmacros.global_def
    (start_env name) zero_pat     
    (Subst ["\\begin{th@env}{"^name^"}{"^cname^"}{"^caption^"}"]) ;
  Latexmacros.global_def
    (end_env name) zero_pat
    (Subst ["\\end{th@env}"])
;;

def_name_code "\\newtheorem" do_newtheorem ;
def_name_code "\\renewtheorem" do_newtheorem
;;

(* Command definitions, TeX style *)

let do_def global lxm lexbuf =
  Save.start_echo () ;
  let name = get_csname lexbuf in
  Save.skip_blanks_init lexbuf ;
  let args_pat,body =
    if top_level () then
      let args_pat = Save.defargs lexbuf in
      let {arg=body} = save_body lexbuf in
      args_pat,body
    else
      let args_pat =
        Save.defargs
          (MyLexing.from_string
             (subst_this (Save.get_defargs lexbuf))) in
      let body = subst_body lexbuf in
      args_pat,body in
  let real_args = Save.get_echo () in
  if echo_toimage () || (global && echo_global_toimage ()) then begin    
    Image.put  (lxm^real_args) ;
    Image.put_char '\n'
  end ;
  (if global then  Latexmacros.global_def else Latexmacros.def)
    name ([],args_pat) (Subst body)
;;

def_name_code "\\def" (do_def false) ;
def_name_code "\\gdef" (do_def true)
;;

let caml_print s = CamlCode (fun _ -> Dest.put s)
;;

def_code "\\prim@def"
  (fun lexbuf ->
    let name = get_csname lexbuf in
    Save.skip_blanks_init lexbuf ;
    let body = get_prim_arg lexbuf in
    def name zero_pat (caml_print body))
;;

def_code "\\undef"
  (fun lexbuf ->
    let name = get_csname lexbuf in
    Latexmacros.global_undef name)
;;


let do_let global lxm lexbuf =
  Save.start_echo () ;
  let name = get_csname lexbuf in
  Save.skip_equal lexbuf ;
  let alt = get_csname lexbuf in
  let real_args = Save.get_echo () in
  try
    let nargs,body = Latexmacros.find_fail alt in
    (if global then global_def else def)
      name nargs body ;
    if echo_toimage () || (global && echo_global_toimage ()) then begin
      Image.put lxm ;
      Image.put real_args ;
      Image.put "\n"
    end
  with
  | Failed ->
      warning ("Not binding "^name^" with "^lxm^", command "^alt^" does not exist")
;;

def_name_code "\\let" (do_let false) ;
;;

let do_global lxm lexbuf =
  let next = subst_arg lexbuf in
  begin match next with
  | "\\def" -> do_def true (lxm^next) lexbuf
  | "\\let" -> do_let true (lxm^next) lexbuf
  | _       -> warning "Ignored \\global"
  end
;;



def_name_code "\\global" do_global
;;

(* TeXisms *)
def_code "\\noexpand"
  (fun lexbuf ->
     let arg = subst_arg lexbuf in
     Dest.put arg)
;;

def_code "\\execafter"
  (fun lexbuf ->
     let arg = save_arg lexbuf in
     let next_arg = save_arg lexbuf in
     let cur_subst = get_subst () in
     scan_this_may_cont main lexbuf cur_subst next_arg ;
     scan_this_may_cont main lexbuf cur_subst arg)
;;


def_code "\\csname"
  (fun lexbuf ->
    ignore (skip_blanks lexbuf) ;
    let name = "\\"^get_prim (Save.incsname lexbuf) in
    check_alltt_skip lexbuf ;
    expand_command name lexbuf)
;;

def_code "\\string"
   (fun lexbuf ->
     let arg = subst_arg lexbuf in
     Dest.put arg)
;;

let get_num_arg lexbuf =
  Save.num_arg lexbuf (fun s -> Get.get_int_string (string_to_arg s))
;;


let top_plain c =
  if not (is_plain c) then begin
    set_plain c ;
    fun_register (fun () -> unset_plain c)
  end

and top_unplain c =
  if is_plain c then begin
    unset_plain c ;
    fun_register (fun () -> set_plain c)
  end
;;

def_code "\\catcode"
   (fun lexbuf ->
     let arg = save_arg_with_delim "=" lexbuf in
     let char = Char.chr (Get.get_int_string arg) in
     let code = get_num_arg lexbuf in
     begin match char,code with
     | ('\\',0) | ('{',1) | ('}',2) | ('$',3) | ('&' ,4) |
       ('#',6) | ('^',7) | ('_',8) | ('~',13) |
       ('%',14)|('"',12) -> top_plain char
     | ('{',(11|12)) | ('}',(11|12)) | ('$',(11|12)) | ('&' ,(11|12)) |
       ('#',(11|12)) | ('^',(11|12)) | ('_',(11|12)) | ('~',(11|12)) |
       ('%',(11|12)) | ('\\',(11|12))|
       ('"',13) -> top_unplain char
     | _ ->
         warning "This \\catcode operation is not permitted"
     end ;
     main lexbuf)
;;

def_code "\\chardef"
  (fun lexbuf ->
    let csname = get_csname lexbuf in
    Save.skip_equal lexbuf ;
    let i = get_num_arg lexbuf in
    Latexmacros.def csname zero_pat (Subst [string_of_int i]))
;;

(* Complicated use of output blocks *)

let displayleft lexbuf = 
  let dprev = !display in
  MyStack.push stack_display dprev ;
  display := true ;
  if not dprev then top_open_display () ;      
  let delim = subst_arg lexbuf in
  let {sub=sub ; sup=sup} = save_sup_sub lexbuf in
  Dest.left delim
    (fun vsize ->
      scan_this main
	("\\process@delim{"^delim^"}{"^string_of_int vsize^"}"))
    (fun vsize ->
      Dest.int_sup_sub false vsize
        (scan_this_arg main) (fun () -> ())  sup sub true)
;;

let displayright lexbuf =
  let delim = subst_arg lexbuf in
  let vsize =
    Dest.right delim
    (fun vsize ->
      scan_this main
	("\\process@delim{"^delim^"}{"^string_of_int vsize^"}")) in  
  let {sup=sup ; sub=sub} = save_sup_sub lexbuf in
  let do_what = (fun () -> ()) in
  if vsize > 1 then
    Dest.int_sup_sub false vsize
      (scan_this_arg main) do_what sup sub !display
  else
    Dest.standard_sup_sub (scan_this_arg main) do_what sup sub !display ;
  let dprev = MyStack.pop stack_display in
  if not dprev then top_close_display () ;
  display := dprev
;;

def_code "\\left"
  (fun lexbuf ->
    if !display then displayleft lexbuf
    else expand_command "\\textleft" lexbuf)
;;

def_code "\\right"
  (fun lexbuf ->
    if !display then displayright lexbuf
    else expand_command "\\textright" lexbuf)
;;


def_code "\\over"
   (fun lexbuf ->
     if !display then  Dest.over lexbuf
     else Dest.put_char '/' ;
     ignore (skip_blanks lexbuf))
;;

def_code "\\MakeUppercase"
  (fun lexbuf ->
    let arg = save_arg lexbuf in
    let old_case = !case in
    case := Upper ;
    scan_this_arg main arg ;
    case := old_case) ;
def_code "\\MakeLowercase"
  (fun lexbuf ->
    let arg = save_arg lexbuf in
    let old_case = !case in
    case := Lower ;
    scan_this_arg main arg ;
    case := old_case) ;

def_fun "\\uppercase" Subst.uppercase ;
def_fun "\\lowercase" Subst.lowercase ;
;;

(* list items *)
(*
def_code "\\@linum" (fun _ -> Dest.nitem ()) ;
def_code "\\@li" (fun _ -> Dest.item ()) ;
def_code "\\@dt"
  (fun lexbuf ->
    let arg = subst_arg lexbuf in
    Dest.ditem (scan_this main) arg ;
    check_alltt_skip lexbuf)
;;
*)

def_code "\\@itemize@li" 
  (fun lexbuf -> Dest.item (get_prim_arg lexbuf)) ;
def_code "\\@enumerate@linum" 
  (fun lexbuf -> Dest.nitem (get_prim_arg lexbuf)) ;
def_code "\\@dtdd"
  (fun lexbuf ->
    let arg = subst_arg lexbuf in
    let dtclass = get_prim_arg lexbuf in
    let ddclass = get_prim_arg lexbuf in
    Dest.ditem (scan_this main) arg dtclass ddclass ;
    check_alltt_skip lexbuf)
;;

    
(* Html primitives *)
def_code "\\@open"
  (fun lexbuf ->
    let tag = get_prim_arg lexbuf in
    let arg = get_prim_arg lexbuf in
(*    eprintf "\\@open{%s}{%s}\n" tag arg ; *)
    top_open_block tag arg)
;;

def_code "\\@insert"
  (fun lexbuf ->
    let tag = get_prim_arg lexbuf in
    let arg = get_prim_arg lexbuf in
    Dest.insert_block tag arg )
;;

def_code "\\@close"
  (fun lexbuf ->
    let tag = get_prim_arg  lexbuf in
    top_close_block tag) ;
def_code "\\@force"
  (fun lexbuf ->
    let tag = get_prim_arg  lexbuf in
    top_force_block tag) ;
def_code "\\@flow"
  (fun lexbuf ->
    let tag = get_prim_arg  lexbuf in
    top_close_flow tag) ;
;;

(* Paragraphs, used for closing/re-opening P elts explictely *)
let out_par do_it =
  let have_closed = Dest.close_par () in
  do_it () ; 
  if have_closed then Dest.open_par ()
;;

def_code "\\@close@par"
  (fun lexbuf ->
    ignore (Dest.close_par ()) ;
    check_alltt_skip lexbuf) ;
def_code "\\@open@par"
  (fun lexbuf ->
    let attributes = get_prim_opt "" lexbuf in
    Dest.open_par ~attr:attributes () ;
    check_alltt_skip lexbuf) ;
(* Some material (eg hacha directives) must appear outside P *)
def_code "\\@out@par"
  (fun lexbuf ->
    let arg = save_arg lexbuf in
    out_par (fun () ->  scan_this_arg main arg)) ;
()
;;


def_code "\\@print"
  (fun lexbuf ->
    let {arg=arg} = save_arg lexbuf in
    Dest.put arg)
;;

let put_unicode_default uc =
  try
    let txt = OutUnicode.get_default uc in
    scan_this main txt
  with Not_found ->
    Misc.warning
      (sprintf
         "Cannot output that numerical entity: %s" (OutUnicode.show uc)) ;
    Dest.put_char '?'
;;

let put_unicode uc =
  try Dest.put_unicode uc
  with Misc.CannotPut -> put_unicode_default uc
;;

def_code "\\@print@u"
  (fun lexbuf ->
    let {arg=arg} = save_arg lexbuf in
    let uc = OutUnicode.parse arg in
    put_unicode uc)
;;

def_code "\\@print@u@default"
  (fun lexbuf ->
    let {arg=arg} = save_arg lexbuf in
    let uc = OutUnicode.parse arg in
    put_unicode_default uc)
;;

def_code "\\@def@u@default"
  (fun lexbuf ->
    let uc = OutUnicode.parse (subst_arg lexbuf) in
    let default = subst_arg lexbuf in
    OutUnicode.def_default uc default)
;;


def_code "\\@printnostyle"
  (fun lexbuf ->
    let {arg=arg} =  save_arg lexbuf in
    top_open_group () ;
    Dest.nostyle () ;
    Dest.put arg ;
    top_close_group ())
;;

def_code "\\@getprintnostyle"
  (fun lexbuf ->
    top_open_group () ;
    Dest.nostyle () ;
    let arg = get_prim_arg lexbuf in
    Dest.put arg ;
    top_close_group ())
;;

def_code "\\@getprint"
  (fun lexbuf ->
    let arg = get_prim_arg lexbuf in
(*    eprintf "GET PRINT: '%s'\n" arg ;*)
    let buff = MyLexing.from_string arg in
    Dest.put (Save.tagout buff)) ;
;;

def_code "\\@subst"
  (fun lexbuf ->
    let arg = subst_arg lexbuf in
    Dest.put arg) ;

def_code "\\@subst@expn"
  (fun lexbuf ->
    let arg = subst_expn_arg lexbuf in
    Dest.put arg)
;;

(* write a string in aux file *)
def_code "\\@auxdowrite"
  (fun lexbuf ->
     let what = save_arg lexbuf in
     let s = get_this_arg main what in
     Auxx.swrite s)
;;
(* Idem, with no evaluation *)
def_code "\\@auxdowritesubst"
  (fun lexbuf ->
     let what = subst_arg lexbuf in
     Auxx.swrite what)
;;

(* format toc file *)
def_code "\\@addtocsec"
  (fun lexbuf ->
     let suf = get_prim_arg lexbuf in
     let anchor = get_prim_arg lexbuf in
     let level = get_num_arg lexbuf in
     let {arg=title} = save_arg lexbuf in
     Auxx.addtoc suf level
       (sprintf "\\@locref{%s%s}{\\begin{@norefs}%s\\end{@norefs}}"
          suf anchor title))
;;

(* Second version with anchor disconnected from toc suffix *)
def_code "\\@@addtocsec"
  (fun lexbuf ->
     let suf = get_prim_arg lexbuf in
     let anchor = get_prim_arg lexbuf in
     let level = get_num_arg lexbuf in
     let {arg=title} = save_arg lexbuf in
     Auxx.addtoc suf level
       (sprintf "\\@locref{%s}{\\begin{@norefs}%s\\end{@norefs}}"
          anchor title))
;;
def_code "\\@addcontentsline"
  (fun lexbuf ->
    let saved = check_all () in
    try
     let suf = get_prim_arg lexbuf in
     let level =  get_num_arg lexbuf in
     let {arg=title} = save_arg lexbuf in
     Auxx.addtoc suf level title
    with e ->
      hot_all saved ;
      Misc.warning
        (sprintf "Failure of \\@addcontentsline: %s"
           (Printexc.to_string e)))
      
;;

def_code "\\@notags"
  (fun lexbuf ->
    let arg = save_arg lexbuf in
    let arg = get_this_arg main arg in
    let r =
      let buff = MyLexing.from_string arg in
      Save.tagout buff in
    Dest.put r)
;;
def_code "\\@anti"
  (fun lexbuf ->
    let arg = save_arg lexbuf in
    let envs =
      get_style (fun _lex -> Dest.clearstyle () ; main _lex) arg in
    if !verbose > 2 then begin
      prerr_string ("Anti result: ") ;
      List.iter
        (fun s ->
          prerr_string (Element.pretty_text s^", ")) envs ;
      prerr_endline ""
    end ;
    Dest.erase_mods envs)
;;

let styles_stack = MyStack.create "styles"
;;

def_code "\\push@styles"
  (fun _lexbuf ->
    let envs = get_style main {arg = "" ; subst=top_subst} in
    MyStack.push styles_stack envs) ;

def_code "\\pop@styles"
   (fun _lexbuf ->
     let envs = MyStack.pop styles_stack in
     Dest.clearstyle () ;
     List.iter Dest.open_mod (List.rev envs))
;;

def_code "\\@style"  
  (fun lexbuf ->
    let arg = get_prim_arg lexbuf in
    Dest.open_mod (Style arg) )
;;

def_code "\\@styleattr"
 (fun lexbuf ->
    let tag = get_prim_arg lexbuf in
    let attr = get_prim_arg lexbuf in
    Dest.open_mod (StyleAttr (tag, attr))) ;
def_code "\\@span"
 (fun lexbuf ->
    let attr = get_prim_arg lexbuf in
    Dest.open_mod (StyleAttr ("span", attr)))
;;

def_code "\\@fontcolor"  
  (fun lexbuf ->
    let arg = get_prim_arg lexbuf in
    Dest.open_mod (Color arg))
;;
(*
def_code "\\@styleset"
  (fun lexbuf -> 
    let arg = get_prim_arg lexbuf in
    Dest.open_mod (...))
;;
*)
def_code "\\@fontsize"  
  (fun lexbuf ->
    let arg = save_body lexbuf in
    Dest.open_mod (Font (Get.get_int arg)) )
;;

def_code "\\@nostyle"
  (fun lexbuf -> Dest.nostyle () ; check_alltt_skip lexbuf)
;;
def_code "\\@clearstyle"
  (fun lexbuf -> Dest.clearstyle ()  ; check_alltt_skip lexbuf)
;;
def_code "\\@incsize"
  (fun lexbuf ->
    let arg = save_body lexbuf in
    inc_size (Get.get_int arg) )
;;
def_code "\\htmlcolor"
  (fun lexbuf ->
    let arg = get_prim_arg lexbuf in
    Dest.open_mod (Color ("\"#"^arg^"\"")) )
;;

def_code "\\usecounter"
  (fun lexbuf ->
    let arg = get_prim_arg lexbuf in
    Counter.set_counter arg 0 ;
    Dest.set_dcount arg)
;;
def_code "\\@fromlib"
  (fun lexbuf ->
          let arg = get_prim_arg lexbuf in
          start_lexstate ();
          Mysys.put_from_file (Filename.concat Mylib.libdir arg) Dest.put;
          restore_lexstate ())
;;
def_code "\\@imageflush"
  (fun lexbuf ->
    Image.page () ;
    check_alltt_skip lexbuf)
;;

def_code "\\textalltt"
  (fun lexbuf ->
       let opt = get_prim_opt "code" lexbuf in
       let arg = save_arg lexbuf in
       let old = !alltt in
       scan_this main "\\mbox{" ;
       alltt := Lexstate.Inside ;
       Dest.open_group opt ;
       scan_this_arg main arg ;
       Dest.close_group () ;
       scan_this main "}" ;
       alltt := old )
;;
def_code "\\@itemdisplay"
  (fun _lexbuf -> Dest.force_item_display ())
;;
def_code "\\@br"
  (fun _lexbuf -> Dest.skip_line ())
;;


(* TeX conditionals *)
let testif cell lexbuf =
  if !cell then check_alltt_skip lexbuf
  else skip_false lexbuf

let setif cell b lexbuf =
  let old = !cell in
  fun_register (fun () -> cell := old) ;
  cell := b ;
  check_alltt_skip lexbuf 
;;

let extract_if name =
  let l = String.length name in
  if l <= 3 || String.sub name 0 3 <> "\\if" then
    raise (Misc.ScanError ("Bad newif: "^name)) ;
  String.sub name 3 (l-3)
;;

let def_and_register name f =
  def name zero_pat (CamlCode f)
;;

let tbool  = function
  | true ->  "+"
  | false -> "-"

let tverb name cell lexbuf =
  if !verbose > 1 then
  fprintf stderr
    "Testing %s -> %s\n" name (tbool !cell) ;
  testif cell lexbuf
;;

let newif_ref name cell =  
  def_and_register ("\\if"^name) (tverb name cell) ;
  def_and_register ("\\"^name^"true") (setif cell true) ;
  def_and_register ("\\"^name^"false") (setif cell false) ;
  register_cell name cell ;
  fun_register (fun () -> unregister_cell name)
;;

let newif lexbuf =
  let arg = get_csname lexbuf in
  begin try
    let name = extract_if arg in
    let cell = ref false in
    newif_ref name cell ;
  with Latexmacros.Failed -> ()
  end
;;

exception FailedFirst
;;

let rec norm_body k b = match b with
  | [] -> 0,b
  | x::xs ->
      if k >= String.length x then norm_body 0 xs
      else k,b

let same_body =
  let rec do_rec k1 k2 b1 b2 = match norm_body k1 b1, norm_body k2 b2 with
  | (_,[]),(_,[]) -> true
  | ((_,[]),(_,_::_))
  | ((_,_::_),(_,[])) -> false
  | (k1,(x1::_ as b1)),(k2,(x2::_ as b2)) ->
      let c1 =  x1.[k1] and c2 = x2.[k2] in
(*      eprintf "k1=%i, c1='%c', k2=%i, c2='%c'\n" k1 c1 k2 c2 ; *)
      c1 = c2 && do_rec (k1+1) (k2+1) b1 b2 in
  do_rec 0 0

let same_expn (p1,a1) (p2,a2) =
  (p1 = p2) &&
  (match a1,a2 with
  | Subst b1,Subst b2 ->
(*
      eprintf "Check: [%a] [%a]\n"
        Lexstate.pretty_body b1 Lexstate.pretty_body b2 ;
*)
      same_body b1 b2
  | _,_ -> a1 = a2)
;;

def_code "\\ifx"
  (fun lexbuf ->
    let arg1 = get_csname lexbuf in
    let arg2 = get_csname lexbuf  in
    let r =
      try
        let m1 =
          try Latexmacros.find_fail arg1 with
          |  Failed -> raise FailedFirst in
        let m2 = Latexmacros.find_fail arg2 in
        same_expn m1 m2
      with
      | FailedFirst ->
          begin
            try let _ = Latexmacros.find_fail arg2 in false
            with Failed -> true
          end
      | Failed -> false in
    if !verbose > 2 then
      prerr_endline ("\\ifx -> "^(if r then "true" else "false")) ;
    if r then
      check_alltt_skip lexbuf
    else
      skip_false lexbuf)
;;
def_code "\\ifu"
  (fun lexbuf ->
    let arg1 = get_csname lexbuf in
    try
      let _ = Latexmacros.find_fail arg1 in      
      skip_false lexbuf
    with
    | Failed -> check_alltt_skip lexbuf)
;;    

def_code "\\ife"
  (fun lexbuf ->
    let arg = get_csname lexbuf in
    let r = get_prim arg in
    if r <> "" then
      skip_false lexbuf
    else
      check_alltt_skip lexbuf)
;;

def_code "\\newif" newif 
;;

def_code "\\else" (fun lexbuf -> skip_false lexbuf)
;;

def_code "\\fi" (fun lexbuf -> check_alltt_skip lexbuf)
;;


let sawdocument = ref false
;;

let entities =
  ref (match !symbol_mode with Entity -> true | _ -> false)
;;


newif_ref "symb" (ref (match !symbol_mode with Symbol -> true | _ -> false)) ;
newif_ref "entities" entities ;
newif_ref "symbtext"
    (ref (match !symbol_mode with SText -> true | _ -> false)) ;
newif_ref "raw" raw_chars ;
newif_ref "silent" silent;
newif_ref "math" in_math ;
newif_ref "whitepre" whitepre ;
newif_ref "mmode" in_math ;
newif_ref "display" display ;
newif_ref "verbd" displayverb ;
(* NO NEED AFTER BABEL SUPPORT *)
(*newif_ref "french" french ;*)
newif_ref "html" html;
newif_ref "text" text;
newif_ref "info" text;
newif_ref "mathml" Parse_opts.mathml;
newif_ref "optarg" optarg;
newif_ref "styleloaded" styleloaded;
newif_ref "activebrace" activebrace;
newif_ref "pedantic" pedantic ;
newif_ref "fixpoint" fixpoint ;
newif_ref "moreentities" moreentities;
newif_ref "alltt@loaded" alltt_loaded ;
newif_ref "filter" (ref filter) ;
newif_ref "@sawdocument" sawdocument ;
newif_ref "@warnunder" warn_under ;
newif_ref "@dumpindex" Misc.dump_index ;
newif_ref "@silent" Misc.silent ;
def_code "\\iftrue" (testif (ref true)) ;
def_code "\\iffalse" (testif (ref false)) ;
;;

def_code "\\if@toplevel"
  (fun lexbuf ->
    if echo_global_toimage () then check_alltt_skip lexbuf
    else skip_false lexbuf)
;;



(* Bibliographies *)
let bib_ref s1 s2 post =
  Auxx.swrite ("\\citation{"^s1^"}\n") ;
  scan_this main ("\\@bibref{\\bibtaghook{"^s1^"}}{"^s2^"}{"^post^"}")
;;

let cite_arg key =
  let key = get_prim ("\\bibtaghook{"^key^"}") in
  match Auxx.bget true key with
  | None   -> ""
  | Some s -> s
;;

def_code "\\cite"
  (fun lexbuf ->
    let opt1 = subst_opt "" lexbuf in
    let has_opt1 = !optarg in
    let opt2 = subst_opt "" lexbuf in
    let has_opt2 = !optarg in
    check_alltt_skip lexbuf ; 
    let args = save_cite_arg lexbuf in
    let args = Subst.subst_list args in
    Dest.open_group "" ;
    scan_this main "\\@open@cite@one" ;
    if has_opt1 && has_opt2 then begin
      if not (is_empty_list opt1) then begin
        scan_this_list main
          ("\\@cite@pre{"::opt1@["}"])
      end
    end ;
    scan_this main "\\@open@cite@two" ;
    let post =
      let opt =
        if has_opt1 && has_opt2 then Some opt2
        else if has_opt1 then Some opt1
        else None in
      match opt with
      | Some s -> String.concat "" s
      | None   -> "" in
    let rec do_rec = function
        [] -> ()
      | [x] -> bib_ref x (cite_arg x) post
      | x::rest ->
          bib_ref x (cite_arg x) "" ;
          scan_this main "\\@sep@cite\\@sep@cite@space" ;
          do_rec rest in
    do_rec args ;
    scan_this main "\\@close@cite" ;
    Dest.close_group ())
;;

(* Includes *)
def_code "\\includeonly"
  (fun lexbuf ->
    let arg = Save.cite_arg lexbuf in
    add_includes arg )
;;

(* Foot notes *)

def_code "\\@stepanchor"
  (fun lexbuf ->
    let mark = Get.get_int (save_body lexbuf) in
    Foot.step_anchor mark) ;
def_code "\\@anchorval"
  (fun lexbuf ->
    let mark = Get.get_int (save_body lexbuf) in
    Dest.put (string_of_int (Foot.get_anchor mark)))
;;

def_code "\\@footnotetext"
  (fun lexbuf ->
    start_lexstate () ; 
    let mark = Get.get_int (save_body lexbuf) in
    let text = save_arg lexbuf in
    let text =
      do_get_this
        start_normal end_normal Dest.clearstyle
        main text in
    Foot.register
      mark
      (get_this_string main ("\\@fnmarknote{"^string_of_int mark^"}"))
      text ;
    restore_lexstate ())
;;

let foot_noteflush sticky lexbuf =
  let sec_here = get_prim_arg lexbuf
  and sec_notes = get_prim "\\@footnotelevel" in
  start_lexstate () ;
  Foot.flush sticky (scan_this main)
    Dest.put sec_notes sec_here ;
  restore_lexstate ()
;;

def_code "\\@footnoteflush" (foot_noteflush false) ;
def_code "\\@footnoteflush@sticky" (foot_noteflush true)
;;

def_code "\\@footnotesub" (fun _lexbuf -> Foot.sub_notes ()) ;
def_code "\\@endfootnotesub" (fun _ -> Foot.end_notes ())
;;

(* Opening and closing environments *)


def_code "\\begin"
  (fun lexbuf ->
    let env = get_prim_arg lexbuf in
    new_env env ;
    top_open_block "" "" ;
    let macro = start_env env in
    let old_envi = save stack_entry in
    push stack_entry env ;
    begin try
      do_expand_command main no_skip macro lexbuf
    with
    | e ->
        restore stack_entry old_envi ;
        raise e
    end ;
    restore stack_entry old_envi)
;;


def_code "\\end"
  (fun lexbuf ->
    let env = get_prim_arg lexbuf in
    do_expand_command main no_skip (end_env env) lexbuf ;
    top_close_block "" ;
    close_env env)
;;

(* To close/reopen envs from their associated commands *)

def_code "\\@end"
  (fun lexbuf ->
    let env = get_prim_arg lexbuf in
    top_close_block "" ;
    close_env env)
;;


def_code "\\@begin"
  (fun lexbuf ->
     let env = get_prim_arg lexbuf in
     new_env env ;
     top_open_block "" "")
;;


let append_to_opt o s = match o with
| "" -> s
| _ -> o ^ " " ^s
;;


(*****************************)
(* To be called by \document *)
(*****************************)

def_code "\\@begin@document"
    (fun lexbuf ->
    let s = get_prim "\\heveaimagedir" in
    if s <> "" then begin
      Misc.set_image_opt (append_to_opt (Misc.get_image_opt ()) ("-todir "^s))
    end ;
    check_alltt_skip lexbuf)
;;

def_code "\\@addimagenopt"
  (fun lexbuf ->
    let opt = get_prim_arg lexbuf in
    Misc.set_image_opt (append_to_opt (Misc.get_image_opt ()) opt) ;
    check_alltt_skip lexbuf)
;;

def_code "\\@getimagenopt"
(fun lexbuf ->
  Dest.put (Misc.get_image_opt ()) ;
  check_alltt_skip lexbuf)
;;

def_code "\\@imagenopt"
  (fun lexbuf ->
    Image.put ("%Options: "^Misc.get_image_opt ()) ;
    check_alltt_skip lexbuf)
    
;;

def_code "\\@raise@enddocument"
  (fun _ ->
    if not !sawdocument then
      fatal ("\\end{document} with no \\begin{document}")
    else if not (MyStack.empty stack_env) then
      error_env "document" !cur_env
    else begin
      raise Misc.EndDocument
    end)
;;

let little_more lexbuf =
  to_newline lexbuf ;
  Out.to_string more_buff
;;

def_code "\\endinput" (fun lexbuf ->
  let reste = little_more lexbuf in
  scan_this main reste ;
  raise Misc.EndInput)    
;;

(* Boxes *)

def_code "\\mbox" (fun lexbuf -> mbox_arg lexbuf)
;;



def_code "\\newsavebox"
  (fun lexbuf ->
    let name = get_csname lexbuf in
    try
      let _ = find_fail name in
      warning ("Not (re-)defining '"^name^"' with \\newsavebox")
    with
    | Failed ->
        global_def name zero_pat (CamlCode (fun _ -> ())))
;;

def_code "\\providesavebox"
  (fun lexbuf ->
    let name = get_csname lexbuf in
    try
      let _ = find_fail name in ()
    with
    | Failed ->
        global_def name zero_pat (CamlCode (fun _ -> ())))
;;


let get_this_arg_mbox arg =
  start_mbox () ;
  let r = get_this_arg main arg in
  top_close_group () ;
  r
;;


let do_sbox global name body =
  if not (Latexmacros.exists name) then
    warning ("\\sbox on undefined bin '"^name^"'") ;
  let to_print =  get_this_arg_mbox body in
  (if global then global_def else def) name zero_pat (caml_print to_print)
;;

def_code "\\savebox" 
  (fun lexbuf ->
    let name = get_csname lexbuf in
    warning "savebox";
    skip_opt lexbuf ;
    skip_opt lexbuf ;
    let body = save_arg lexbuf in
    do_sbox false name body)
;;

def_code "\\sbox"
  (fun lexbuf ->
    let name = get_csname lexbuf in
    let body = save_arg lexbuf in
    do_sbox false name body) ;

def_code "\\gsbox"
  (fun lexbuf ->
    let name = get_csname lexbuf in
    let body = save_arg lexbuf in
    do_sbox true name body) ;
;;

(* Notice that using a bin name as a command also works,
   but without erasing actives styles *)
def_code "\\usebox"
  (fun lexbuf ->
    let name = get_csname lexbuf in
    top_open_group () ;
    Dest.nostyle () ;
    expand_command name lexbuf ;
    top_close_group () ;
    ())
;;


def_code "\\lrbox"
  (fun lexbuf ->
    close_env "lrbox" ;
    push stack_display !display ;
    display := false ;
    let name = get_csname lexbuf in
    Dest.open_aftergroup
      (fun s ->
        def name zero_pat (caml_print s) ;
        "") ;
    start_mbox ())
;;

def_code "\\endlrbox"
  (fun _ ->
    top_close_group () ;   (* close mbox *)
    Dest.close_group () ;  (* close after group *)
    display := pop stack_display ;
    new_env "lrbox")
;;

(* External acess to close math mode, preserving text/display *)

let start_text () =
  push stack_table !in_table ; in_table := NoTable ;
  push stack_in_math !in_math ; in_math := false

and end_text () =
  in_math := pop stack_in_math ;
  in_table := pop stack_table
;;

def_code "\\@start@text" (fun _ -> start_text ()) ;
def_code "\\@end@text" (fun _ ->  end_text ())
;;



(* chars *)
def_code "\\char"
  (fun lexbuf ->
    let arg = get_num_arg lexbuf in
    if not !silent && (arg < 32 || (arg > 127 && arg < 161)) then begin
      Location.print_pos () ;
      prerr_endline ("Warning: \\char, check output");
    end ;
    translate_put_unicode (Char.chr arg) (fun () -> -1) ;
    if not (effective !alltt) then check_alltt_skip lexbuf)
;;

def_code "\\symbol"
  (fun lexbuf ->
    let arg = get_prim_arg lexbuf in
    scan_this main ("\\char"^arg))
;;

(* labels *)

(* Counters *)
let alpha_of_int i = String.make 1 (Char.chr (i-1+Char.code 'a'))
and upalpha_of_int i = String.make 1 (Char.chr (i-1+Char.code 'A'))
;;

let rec roman_of_int = function
  0 -> ""
| 1 -> "i"
| 2 -> "ii"
| 3 -> "iii"
| 4 -> "iv"
| 9 -> "ix"
| i ->
   if i < 9 then "v"^roman_of_int (i-5)
   else
     let d = i / 10 and u = i mod 10 in
     String.make d 'x'^roman_of_int u
;;

let uproman_of_int i = String.uppercase (roman_of_int i)
;;

let fnsymbol_of_int = function
  0 -> " "
| 1 -> "*"
| 2 -> "#"
| 3 -> "$"
| 4 -> "\167"
| 5 -> "\182"
| 6 -> "||"
| 7 -> "**"
| 8 -> "##"
| 9 -> "$$"
| i -> alpha_of_int (i-9)
;;

let def_printcount name f =
  def_code name
    (fun lexbuf ->
      let cname = get_prim_arg lexbuf in
      let cval =
        match cname with
        | "inputlineno" -> Location.get_lineno ()            
        | _ -> Counter.value_counter cname in
      let pp = f cval in
      Dest.put pp)
;;

def_printcount "\\arabic" string_of_int ;
def_printcount "\\alph"  alpha_of_int ;
def_printcount "\\Alph"  upalpha_of_int ;
def_printcount "\\roman" roman_of_int;
def_printcount "\\Roman" uproman_of_int;
def_printcount "\\fnsymbol" fnsymbol_of_int
;;

let pad p l s =
  for _i = l-String.length s downto 1 do
    translate_put_unicode_string p
  done
;;

def_code "\\@pad"
  (fun lexbuf ->
    let p = get_prim_arg lexbuf in
    let l = Get.get_int (save_body lexbuf) in
    let arg = get_prim_arg lexbuf in
    pad p l arg ;
    translate_put_unicode_string arg)
;;

def_code "\\newcounter"
  (fun lexbuf ->
    Save.start_echo () ;
    let name = get_prim_arg lexbuf in
    let within = get_prim_opt "" lexbuf in
    let real_args = Save.get_echo () in
    if echo_global_toimage () then begin
      Image.put "\\newcounter" ;
      Image.put real_args ;
      Image.put_char '\n'
    end ;
    do_newcounter name within)
;;

def_code "\\addtocounter"
  (fun lexbuf ->
    Save.start_echo () ;
    let name = get_prim_arg lexbuf in
    let arg = save_body lexbuf in
    let real_args = Save.get_echo () in
    if echo_global_toimage () then begin
      Image.put "\\addtocounter" ;
      Image.put real_args ;
      Image.put_char '\n'
    end ;
    Counter.add_counter name (Get.get_int arg))
;;

def_code "\\setcounter"
  (fun lexbuf ->
    Save.start_echo () ;
    let name = get_prim_arg lexbuf in
    let arg = save_body lexbuf in
    let real_args = Save.get_echo () in
    if echo_global_toimage () then begin
      Image.put "\\setcounter" ;
      Image.put real_args ;
      Image.put_char '\n'
    end ;
    Counter.set_counter name (Get.get_int arg) )
;;

def_code "\\stepcounter"
  (fun lexbuf ->
    Save.start_echo () ;    
    let name = get_prim_arg lexbuf in
    let real_args = Save.get_echo () in
    if echo_global_toimage () then begin
      Image.put "\\stepcounter" ;
      Image.put real_args ;
      Image.put_char '\n'
    end ;
    Counter.step_counter name)
;;

(* spacing *)

(*
let stack_closed = MyStack.create "stack_closed"
;;

def_code "\\@saveclosed"
  (fun lexbuf ->
    push stack_closed (Dest.get_last_closed ()) ;
    check_alltt_skip lexbuf)
;;

def_code "\\@restoreclosed"
  (fun lexbuf ->
    Dest.set_last_closed (pop stack_closed) ;
    check_alltt_skip lexbuf)
;;
*)    
exception Cannot
;;

def_code "\\@getlength"
  (fun lexbuf ->
    let arg = get_prim_arg lexbuf in
    let pxls = 
      match Get.get_length arg with
      | Length.Pixel n -> n
      | Length.Char n -> Length.char_to_pixel n
      | _             -> 0 in
(*    eprintf "GET LENGTH: %i\n" pxls ; *)
    Dest.put (string_of_int pxls))
;;

let insert_horizontal_space persistent (warn : string -> unit) (insert : bool -> Length.t -> unit) lexbuf =
  let arg = get_prim (subst_arg lexbuf) in
    try
      begin
        match Length.main (MyLexing.from_string arg) with
        | Length.Percent _ | Length.NotALength _ | Length.Default -> raise_notrace Cannot
        | length -> insert persistent length
      end
    with Cannot -> warn arg
;;

let do_space (warn : string -> unit) (doit : unit -> unit) lexbuf =
  let arg = subst_arg lexbuf in
    try
      let n = match Length.main (MyLexing.from_string arg) with
        | Length.Char n -> n
        | Length.Pixel n -> Length.pixel_to_char n
        | _ -> raise Cannot
      in
        for _i = 1 to n do
          doit ()
        done
    with Cannot -> warn arg
;;

let warn_space name arg = warning (name ^ " with arg '" ^ arg ^ "'")
;;

let warn_hspace = warn_space "\\hspace"
and warn_vspace = warn_space "\\vspace"
;;

def_code "\\hspace"
    (fun lexbuf -> insert_horizontal_space false warn_hspace Dest.put_hspace lexbuf);
def_code "\\hspace*"
    (fun lexbuf -> insert_horizontal_space true warn_hspace Dest.put_hspace lexbuf);
def_code "\\vspace"
    (fun lexbuf -> do_space warn_vspace Dest.skip_line lexbuf);
def_code "\\@vdotsfill"
  (fun lexbuf ->
    do_space
      (fun arg  -> warning ("vertical length: "^arg) ; scan_this main "\\vdots")
      (let fst = ref true in
      (fun () ->
        if not !fst then Dest.skip_line ()
        else fst := false ;
        scan_this main "\\vdots"))
        lexbuf)
;;

let single_space = "\\@print{ }" in
  OutUnicode.def_default OutUnicode.emsp single_space;
  OutUnicode.def_default OutUnicode.ensp single_space;
  OutUnicode.def_default OutUnicode.emsp13 single_space;
  OutUnicode.def_default OutUnicode.emsp14 single_space;
  OutUnicode.def_default OutUnicode.six_per_em_space single_space;
  OutUnicode.def_default OutUnicode.hairsp single_space;
  OutUnicode.def_default OutUnicode.six_per_em_nbsp single_space;
  OutUnicode.def_default OutUnicode.medium_space single_space;
;;

OutUnicode.def_default OutUnicode.visible_space "\\@print{_}";
;;

OutUnicode.def_default OutUnicode.zero_width_space "";
OutUnicode.def_default OutUnicode.zero_width_joiner "";
OutUnicode.def_default OutUnicode.word_joiner "";
;;


(* Explicit groups *)
def_code "\\begingroup"
  (fun lexbuf  ->
    new_env "command-group" ; top_open_block "" "" ;
    check_alltt_skip lexbuf)
;;

def_code "\\endgroup"
  (fun lexbuf  ->
    top_close_block ""  ; close_env !cur_env ;
    check_alltt_skip lexbuf)
;;

(* alltt *)

register_init "alltt"
    (fun () ->
      def_code "\\alltt"
        (fun _ ->
          if !verbose > 1 then prerr_endline "begin alltt" ;
          alltt := Lexstate.Inside ;
          fun_register (fun () -> alltt := Not) ;
          Dest.close_block "" ; Dest.open_block "pre" "") ;

      def_code "\\endalltt"
        (fun _ ->
          if !verbose > 1 then prerr_endline "end alltt" ;
          Dest.close_block "pre" ; Dest.open_block "" ""))
;;

(* Multicolumn *)

def_code "\\multicolumn"
    (fun lexbuf ->
      if not (is_table !in_table) then
        raise (ScanError "\\multicolumn should occur in some array") ;
      let n = Get.get_int (save_body lexbuf) in      
      let format =  Tabular.main (save_arg lexbuf) in
      do_multi n  format main)
;;

def_code "\\hline"
  (fun lexbuf ->
    if not (is_table !in_table) then
      raise (ScanError "\\hline should occur in some array") ;
    do_hline main ;
    skip_blanks_pop lexbuf ;
    let _ = Dest.forget_par () in
    ())
;;

(* inside tabbing *)
let do_tabul lexbuf =
  if is_tabbing !in_table then begin
    do_unskip () ;
    Dest.close_cell ""; Dest.open_cell default_format 1 0 false
  end ;
  skip_blanks_pop lexbuf
;;

let tabbing_kill lexbuf =
  if is_tabbing !in_table then begin
    do_unskip () ;
    Dest.close_cell "";
    Dest.erase_row () ;
    Dest.new_row () ;
    Dest.open_cell default_format 1 0 false
  end ;
  skip_blanks_pop lexbuf
;;

let def_no_fail name f = Latexmacros.def name zero_pat (CamlCode f)
;;

let def_tabbing_commands () =
  def_no_fail "\\=" do_tabul ;
  def_no_fail "\\>" do_tabul ;
  def_no_fail "\\kill" tabbing_kill
;;

(* Tabular and arrays *)


let check_width = function
  | Length.Char x ->
      "width:" ^ string_of_int (Length.char_to_pixel x) ^ "px"
  | Length.Pixel x ->
      "width:" ^ string_of_int x ^ "px"
  | Length.Percent x ->
      "width:" ^ string_of_int x^"%"
  | _ -> ""
;;


let get_table_attributes border len =
  let attrs = get_prim
      (if border then
        "\\@table@attributes@border"
      else
        "\\@table@attributes") in
  Lexattr.add_style (check_width len) attrs
  

let open_tabbing _lexbuf =
  let lexbuf = Lexstate.previous_lexbuf in
  let lexfun _lb =
    Dest.open_table false "style=\"border:0;border-spacing:0\" class=\"cellpadding0\"" ;
    Dest.new_row ();
    Dest.open_cell default_format 1 0 false in
  push stack_table !in_table ;
  in_table := Tabbing ;
  new_env "tabbing" ;
  def_tabbing_commands () ;
  def "\\a" zero_pat
    (CamlCode
       (fun lexbuf ->
         let acc = subst_arg lexbuf in
         let arg = subst_arg lexbuf in
         scan_this main ("\\"^acc^arg))) ;
  lexfun lexbuf
;;

def_code "\\tabbing" open_tabbing
;;

let close_tabbing _ =
  Dest.do_close_cell ();
  Dest.close_row ();
  Dest.close_table ();
  in_table := pop stack_table ;
  close_env "tabbing" ;
;;

def_code "\\endtabbing" close_tabbing
;;

let open_array env lexbuf =
  save_array_state ();
  Tabular.border := false ;
  let len =  match env with
    | "tabular*"|"Tabular*" ->
        let arg = save_arg lexbuf in
        begin match Get.get_length (get_prim_onarg arg) with
        | Length.NotALength _ ->
            warning ("'tabular*' with length argument: "^
                     do_subst_this arg) ;
            Length.Default
        | width -> width
        end
    | _ -> Length.Default in
  let attributes = match env with
  | "Tabular*" | "Array" | "Tabular" -> get_prim_opt "" lexbuf
  | _ -> skip_opt lexbuf ; "" in
  skip_opt lexbuf ;
  let format = save_arg lexbuf in
  let format = Tabular.main format in
  cur_format := format ;
  push stack_in_math !in_math ;
  in_table := Table
       {math = (match env with  "array"|"Array" -> true | _ -> false)  ;
         border = !Tabular.border} ;
  if !display then Dest.force_item_display () ;
  in_math := false ;
  push stack_display !display ;
  display := false ;
  begin match attributes with
  | "" ->
      if !Tabular.border then
        Dest.open_table true
          (get_table_attributes true len)
      else
        Dest.open_table false
          (get_table_attributes false len)

  | _  ->
      Dest.open_table !Tabular.border
        (Lexattr.add_style (check_width len) attributes)
 end ;
 open_row() ;
  open_first_col main ;
  skip_blanks_pop lexbuf ;
;;

def_code "\\@array" (open_array "array") ;
def_code "\\@tabular" (open_array "tabular") ;
def_code "\\@tabular*" (open_array "tabular*")
;;
def_code "\\@Array" (open_array "Array") ;
def_code "\\@Tabular" (open_array "Tabular") ;
def_code "\\@Tabular*" (open_array "Tabular*")
;;


let close_array _ =
  do_unskip () ;
  close_last_col main "" ;
  close_last_row () ;
  Dest.close_table () ;
  restore_array_state () ;
  in_math := pop stack_in_math ;
  display := pop stack_display;
  if !display then Dest.force_item_display () ;
;;

def_code "\\end@array" close_array  ;
def_code "\\end@tabular" close_array ;
def_code "\\end@tabular*" close_array ;
def_code "\\end@Array" close_array  ;
def_code "\\end@Tabular" close_array ;
def_code "\\end@Tabular*" close_array ;
;;
  

let do_amper lexbuf =
  if effective !alltt || not (is_plain '&') then begin
    let lxm = lexeme lexbuf in
    translate_put_unicode_string lxm
  end else if is_table !in_table  then begin
    close_col main "&nbsp;"; 
    open_col main
  end ;
  if not (effective !alltt) && is_plain '&' then skip_blanks_pop lexbuf

and do_bsbs lexbuf =
  do_unskip () ;
  skip_opt lexbuf ;
  if is_table !in_table  then begin
    close_col main "&nbsp;" ; close_row () ;
    open_row () ; open_first_col main
  end else if is_tabbing !in_table then begin
    Dest.close_cell "";
    Dest.close_row () ;
    Dest.new_row () ;
    Dest.open_cell default_format 1 0 false
  end else begin
    if !display then
      (*(Dest.put_hspace ();Dest.put_hspace ();Dest.put_hspace ();Dest.put_hspace ())*)
      warning "\\\\ in display mode, ignored"
    else
      Dest.skip_line ()
  end ;
  skip_blanks_pop lexbuf ;
  let _ = Dest.forget_par () in ()
;;

OutUnicode.def_default OutUnicode.minus "\\@print{-}" ;
OutUnicode.def_default OutUnicode.endash "\\@print{--}" ;
OutUnicode.def_default OutUnicode.emdash "\\@print{---}" ;
()
;;

let get_tt_mode () =
  match 
    get_style (fun _lex -> Dest.clearstyle () ; main _lex)
      (string_to_arg "\\tt")
  with
  | [m] -> m
  | m::_ ->
      warning "tt_mode is a complex style"  ;
      m
  | [] ->
      warning "tt_mode is an empty style" ;
      raise Exit

let tt_mode = ref (Style "tt")
;;

(* Will be called once command \tt is defined *)
def_code "\\@set@ttmode"
  (fun lexbuf ->
    begin try tt_mode := get_tt_mode () with Exit -> () end ;
    check_alltt_skip lexbuf)
;;

let has_tt () = Dest.has_mod !tt_mode

let do_minus lexbuf = 
  if  not (effective !alltt) && is_plain '-' && not (has_tt ()) then
    if Save.if_next_char '-' lexbuf then begin
      gobble_one_char lexbuf ;
      if  Save.if_next_char '-' lexbuf then begin
        gobble_one_char lexbuf ;
        put_unicode OutUnicode.emdash
      end else
        put_unicode OutUnicode.endash
    end else if !in_math && not !raw_chars then
      put_unicode OutUnicode.minus
    else
      Dest.put_char '-'
  else
    Dest.put_char '-'
;;

OutUnicode.def_default OutUnicode.ldquot "\\@print{\"}" ;
OutUnicode.def_default OutUnicode.rdquot "\\@print{\"}" ;
OutUnicode.def_default OutUnicode.lsquot "\\@print{`}" ;
OutUnicode.def_default OutUnicode.rsquot "\\@print{'}" ;
OutUnicode.def_default OutUnicode.prime "\\@print{'}" ;
OutUnicode.def_default OutUnicode.dprime "\\@print{''}" ;
OutUnicode.def_default OutUnicode.tprime "\\@print{'''}" ;
OutUnicode.def_default OutUnicode.rprime "\\@print{`}" ;
OutUnicode.def_default OutUnicode.rdprime "\\@print{``}" ;
OutUnicode.def_default OutUnicode.rtprime "\\@print{```}" ;
()
;;

let do_backquote lexbuf = 
  if  is_plain '`' then begin
    if !in_math then begin
      if Save.if_next_char '`' lexbuf then begin
        gobble_one_char lexbuf ;
        if Save.if_next_char '`' lexbuf then begin
          gobble_one_char lexbuf ;
          put_unicode OutUnicode.rtprime
        end else
          put_unicode OutUnicode.rdprime
      end else
         put_unicode OutUnicode.rprime
    end else if has_tt () then put_unicode OutUnicode.lsquot
    else if Save.if_next_char '`' lexbuf then begin
      gobble_one_char lexbuf ;
      put_unicode OutUnicode.ldquot
    end else
      put_unicode OutUnicode.lsquot
  end else
    Dest.put_char '`'

and do_quote lexbuf =
  if is_plain '\'' then begin
    if !in_math then begin
      if Save.if_next_char '\'' lexbuf then begin
        gobble_one_char lexbuf ;
        if Save.if_next_char '\'' lexbuf then begin
          gobble_one_char lexbuf ;
          put_unicode OutUnicode.tprime
        end else
          put_unicode OutUnicode.dprime
      end else
         put_unicode OutUnicode.prime
    end else if has_tt () then  put_unicode OutUnicode.rsquot
    else if Save.if_next_char '\'' lexbuf then begin
      gobble_one_char lexbuf ;
      put_unicode OutUnicode.rdquot
    end else
      put_unicode OutUnicode.rsquot
  end else
    Dest.put_char '\''
;;



def_code "\\@hevea@amper" do_amper ;
def_code "\\\\"           do_bsbs  ;
def_code "\\@HEVEA@amper" do_amper ;
def_code "\\@HEVEA@bsbs"  do_bsbs  ; 
def_code "\\@hevea@minus" do_minus ;
def_code "\\@hevea@backquote" do_backquote ;
def_code "\\@hevea@quote" do_quote ;
()
;;



(* Other scanners *)

def_code "\\latexonly"
  (fun lexbuf ->
    start_other_scan "latexonly" latexonly lexbuf)
;;

def_code "\\toimage"
  (fun lexbuf ->
    start_image_scan "" image lexbuf)
;;


(* Commands to control output to image file or target file *)
def_code "\\@stopimage"
    (fun lexbuf  ->
      Image.stop () ;
      check_alltt_skip lexbuf)
;;

def_code "\\@restartimage"
    (fun lexbuf  ->
      Image.restart () ;
      check_alltt_skip lexbuf)
;;

def_code "\\@stopoutput"
    (fun lexbuf  ->
      Dest.stop () ;
      check_alltt_skip lexbuf)
;;

def_code "\\@restartoutput"
    (fun lexbuf  ->
      Dest.restart () ;
      check_alltt_skip lexbuf)
;;

def_code "\\@prim@toimage"
  (fun lexbuf ->
    let arg = get_prim_arg lexbuf in
    Image.put arg)
;;

(* Info  format specific *)

def_code "\\@infomenu"
  (fun lexbuf ->
    let arg = get_prim_arg lexbuf in
    Dest.infomenu arg)
;;

def_code  "\\@infonode"
  (fun lexbuf ->
    let opt = get_prim_opt "" lexbuf in
    let num = get_prim_arg lexbuf in
    let nom = get_prim_arg lexbuf in
    Dest.infonode opt num nom)
;;

def_code "\\@infoextranode"
  (fun lexbuf ->
   let num = get_prim_arg lexbuf in
   let nom = get_prim_arg lexbuf in
   let text = get_prim_arg lexbuf in
   Dest.infoextranode num nom text)
;;

def_code "\\@infoname"
  (fun lexbuf ->
    let arg = get_prim_arg lexbuf in
    Dest.loc_name arg)
;;

let safe_len = function
  | Length.NotALength _ -> Length.Default
  | l    -> l
;;

let wrap_hr f =
  out_par
    (fun () ->
      top_open_group () ;
      Dest.nostyle () ;
      f () ;
      top_close_group ())
;;

def_code "\\@printHR"
  (fun lexbuf ->
    let arg = get_prim_arg lexbuf in
    let taille = safe_len (Get.get_length (get_prim_arg lexbuf)) in
    wrap_hr (fun () -> Dest.horizontal_line arg taille (Length.Pixel 2)))
;;

def_code"\\@hr"
   (fun lexbuf ->
     let attr = get_prim_opt "" lexbuf in
     let width = safe_len (Get.get_length (get_prim_arg lexbuf)) in
     let height = safe_len (Get.get_length (get_prim_arg lexbuf)) in
     wrap_hr (fun () ->  Dest.horizontal_line attr width height))
;;

Get.init
  get_prim_onarg
  get_fun_result
  new_env close_env
  get_csname
  main
;;

def_code "\\@primitives"
  (fun lexbuf ->
    let pkg = get_prim_arg lexbuf in
    exec_init pkg)
;;

(* try e1 with _ -> e2 *)


def_code "\\@try"
  (fun lexbuf ->
    let saved = check_all () in
    let e1 = save_arg lexbuf in
    let e2 = save_arg lexbuf in
    try
      top_open_block "temp" "" ;
      scan_this_arg main e1 ;
      top_close_block "temp"
    with e -> begin
      hot_all saved ;
      Misc.print_verb 0
        ("\\@try caught exception : "^Printexc.to_string e) ;
      scan_this_arg main e2
    end)
;;

def_code "\\@heveafail"
  (fun lexbuf ->
    let s = get_prim_arg lexbuf in
    raise (Misc.Purposly s))
;;

(*
(* A la TeX ouput (more or less...) *)

def_code "\\newwrite"
  (fun lexbuf ->
    let cmd = save_arg lexbuf in
    let file = ref stderr in
    def_code cmd
      (fun lexbuf ->
        let op = save_arg lexbuf in
        try
          match op with
          |  "\\write" ->
              let what = subst_arg subst lexbuf in
              output_string !file what ;
              output_char !file '\n'
          | "\\closeout" ->
              close_out !file
          | "\\openout" ->
              let name = get_this_nostyle main (save_filename lexbuf) in
              file := open_out name
          | _ ->
              warning ("Unkown file operation: "^op)
        with Sys_error s ->
          warning ("TeX file error : "^s)))
;;

let def_fileop me =
  def_code me
   (fun lexbuf ->
     let cmd = subst_arg lexbuf in
     scan_this_may_cont main lexbuf (cmd^me))
;;

def_fileop "\\write" ;
def_fileop "\\openout" ;
def_fileop "\\closeout"
;;
*)

def_code "\\@funregister"
  (fun lexbuf ->
    let later = subst_arg lexbuf in
    fun_register (fun () -> scan_this main later))
;;

end}

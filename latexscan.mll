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

(* $Id: latexscan.mll,v 1.154 1999-12-08 18:10:17 maranget Exp $ *)


{
module type S =
  sig
    val no_prelude : unit -> unit
    val main : Lexing.lexbuf -> unit

    (* additional resources needed for extension modules. *)
    val cur_env : string ref
    val new_env : string -> unit
    val close_env : string -> unit
    val env_level : int ref
    val macro_register : string -> unit
    val fun_register : (unit -> unit) -> unit
    val top_open_block : string -> string -> unit
    val top_close_block : string -> unit
    val check_alltt_skip : Lexing.lexbuf -> unit
    val def_fun : string -> (string -> string) -> unit
    val def_print : string -> string -> unit
    val get_this_main : string -> string
    val check_this_main : string -> bool
    val get_prim : string -> string
    val get_prim_arg : Lexing.lexbuf -> string
    val get_prim_opt : string -> Lexing.lexbuf -> string
  end

module Make
  (Dest : OutManager.S) (Image : ImageManager.S) =
struct
open Misc
open Parse_opts
open Lexing
open Myfiles
open Latexmacros
open Save
open Tabular
open Lexstate
open Stack
open Subst

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
and macros = ref []
and after = ref [] 
and stack_env = Stack.create "stack_env"
and env_level = ref 0
;;


let macro_register name =
  if !env_level > 0 then begin
    if !verbose > 3 then
      prerr_endline ("Registering macro: "^name);
    macros := name :: !macros
  end
;;

let fun_register f =
  if !env_level > 0 then
    after := f :: !after
;;

let macros_unregister () =
  List.iter
   (fun name ->
     if !verbose > 3 then
       prerr_endline ("Unregistering macro: "^name) ;
     Latexmacros.unregister name) !macros ;
  let rec do_rec = function
    | [] -> ()
    | f :: rest -> do_rec rest ; f () in
  do_rec !after
;;

let inc_size i =
  let n = Dest.get_fontsize () in
  let new_size =
    if n+i <= 1 then 1
    else if n+i >= 7 then 7
    else n+i in
  Dest.open_mod (Font new_size)
;;

let big_size () =  Dest.open_mod (Font 7)
;;

(* Horizontal display *)
let display_arg  verbose =
  if verbose > 0 then
    "BORDER=1 CELLSPACING=0 CELLPADDING=0"
  else
    "CELLSPACING=0 CELLPADDING=0"
;;

let top_open_display () =
  if !display then begin
    if !verbose > 1 then
       prerr_endline "open display" ;
    Dest.open_display (display_arg !verbose)
  end

and top_item_display () =
  if !display then begin
    Dest.item_display ()
  end
;;

let top_close_display () =
  if !display then begin
    Dest.close_display ()
  end


(* Latex environment stuff *)
let new_env env =
  push stack_env (!cur_env,!macros, !after) ;
  Location.push_pos () ;
  cur_env := env ;
  macros := [] ; after := [] ;
  if env <> "document" then incr env_level ;
  if !verbose > 1 then begin
    Location.print_pos () ;
    Printf.fprintf stderr "Begin : %s <%d>" env !env_level ;
    prerr_endline ""
  end ;
;;

let print_env_pos () =
  Location.print_top_pos () ;
  prerr_endline ("Latex environment ``"^ !cur_env^"'' is pending")
;;

let error_env close_e open_e =
  raise
    (Misc.Close
       ("Latex env error: ``"^close_e^"'' closes ``"^open_e^"''"))
;;

let close_env env  =
  if !verbose > 1 then begin
    Printf.fprintf stderr "End: %s <%d>" env !env_level ;
    prerr_endline  ""
  end ;
  if env = !cur_env then begin  
    if env <> "document" then decr env_level ;
    let e,m,a = pop stack_env in    
    cur_env := e ;
    macros_unregister () ;
    macros := m ;
    after := a ;
    Location.pop_pos ()
  end else
    error_env env !cur_env
;;

      
(* Top functions for blocks *)
let no_display = function
  "TABLE" | "TR" | "TD" | "DISPLAY" | "VDISPLAY" -> true
|  _ -> false
;;
type array_type = {math : bool ; border : bool}
type in_table = Table of array_type | NoTable | Tabbing
;;

let cur_format = ref [||]
and stack_format = Stack.create "stack_format"
and cur_col = ref 0
and stack_col = Stack.create "stack_col"
and in_table = ref NoTable
and stack_table = Stack.create "stack_table"
and first_col = ref false
and first_border = ref false
and stack_first = Stack.create "stack_first"
and stack_first_b = Stack.create "stack_first_b"
and in_multi = ref false
and stack_multi_flag = Stack.create "stack_multi_flag"
and stack_multi = Stack.create "stack_multi"
;;

let top_open_block block args =
  if !verbose > 2 then prerr_endline ("Top open: "^block);
  push stack_table !in_table ;
  in_table := NoTable ;
  begin match block with
    "PRE" ->
      push stack_display !display ;
      if !display then begin
        Dest.item_display () ;
        display := false
      end ;
      Dest.open_block "PRE" args
  | _ ->
      if !display then begin
        Dest.item_display () ; Dest.open_block block args ;
        Dest.open_display (display_arg !verbose)
      end else
        Dest.open_block block args
  end

and top_close_block_aux close_fun block =
  if !verbose > 2 then prerr_endline ("Top close: "^block) ;
  in_table := pop stack_table ;
  begin match block with
    "PRE" ->
      display := pop stack_display ;
      close_fun block ;
      if !display then Dest.item_display ()
  | _ ->
      if !display then begin
        Dest.close_display () ; close_fun block ; Dest.item_display ()
      end else
        close_fun block
  end
;;

let top_close_block block = top_close_block_aux Dest.close_block block
and top_erase_block block = top_close_block_aux Dest.erase_block block

let top_open_group () =
  top_open_block "" "" ; new_env ""

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

let do_get_this make_style  lexfun (s,subst) =
  let par_val = Dest.forget_par () in
  start_normal subst ;

  if !verbose > 1 then
    prerr_endline ("get_this : ``"^s^"''") ;  
  verbose := !verbose - 1;
  let lexer = Lexing.from_string s in
  let r = Dest.to_string (fun () ->
    top_open_group () ;
    make_style () ;
    lexfun lexer ;
    top_close_group ()) in

  verbose := !verbose + 1 ;
  if !verbose > 1 then begin
    prerr_endline ("get_this ``"^s^"'' -> ``"^r^"''")
  end ;
  end_normal () ;
  Dest.par par_val ;
  r


  
let get_this lexfun s = do_get_this (fun () -> ()) lexfun (s,get_subst ())
and get_this_nostyle lexfun s =
  do_get_this Dest.nostyle lexfun (s,get_subst ())
and get_this_clearstyle lexfun s =
  do_get_this Dest.clearstyle lexfun (s,get_subst ())


let get_this_arg = do_get_this (fun () -> ())
and get_this_nostyle_arg = do_get_this Dest.nostyle

let more_buff = Out.create_buff ()
;;

let default_format =
  Tabular.Align
    {hor="left" ; vert = "" ; wrap = false ;
      pre = "" ; post = "" ; width = Length.Default}

and center_format =
  Tabular.Align
    {hor="center" ; vert = "top" ; wrap = false ;
      pre = "" ; post = "" ; width = Length.Default} 
;;


let pretty_array_type = function
  | Table {math = m ; border = b} ->
      "Table math="^(if m then "+" else "-")^
      " border="^(if b then "+" else "-")
  | NoTable -> "NoTable"
  | Tabbing -> "Tabbing"

let is_table = function
  | Table _ -> true
  | _       -> false

and is_noborder_table = function
  | Table {border = b} -> not b
  | _                  -> false

and is_tabbing = function
  | Tabbing -> true
  | _ -> false

and math_table = function
  | Table {math = m} -> m
  | _ -> raise (Misc.Fatal "Array construct outside an array")
;;

let par_val t =
  if is_table t then Some 0 else Some 1

let prerr_array_state () =
  prerr_endline (pretty_array_type !in_table) ;
  prerr_string "  format:";
  pretty_formats !cur_format ;
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

exception EndInside
;;
exception NoMulti
;;

let attribut name = function
  | "" -> ""
  | s  -> " "^name^"="^s

and as_colspan = function
  |  1  -> ""
  |  n -> " COLSPAN="^string_of_int n

let is_inside = function
    Tabular.Inside _ -> true
  | _ -> false

let is_border = function
  | Tabular.Border _ -> true
  | _ -> false
(*
and as_inside = function
    Tabular.Inside s -> s
  | _        -> ""
    attribut "VALIGN" v^
    attribut "ALIGN" h^
    (if w then "" else " NOWRAP")^
    as_colspan span
| _       ->  raise (Misc.Fatal ("as_align"))
*)
and as_wrap = function
  | Tabular.Align {wrap = w} -> w
  | _ -> false

and as_pre = function
  | Tabular.Align {pre=s} -> s
  | _ -> raise (Misc.Fatal "as_pre")

and as_post = function
  | Tabular.Align {post=s} -> s
  | f -> raise (Misc.Fatal ("as_post "^pretty_format f))
;;

let get_col format i =
  let r = 
    if i >= Array.length format+1 then
      raise (Misc.ScanError ("This array/tabular column has no specification"))
    else if i = Array.length format then default_format
    else format.(i) in
  if !verbose > 2 then begin
   Printf.fprintf stderr "get_col : %d: " i ;
   prerr_endline (pretty_format r) ;
   prerr_string " <- " ;
   pretty_formats format ;
   prerr_newline ()
  end ;
  r
;;

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
        let s = get_this main s in
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

let find_align format =
  let t = ref 0 in
  while (is_inside (get_col format !t)) || (is_border (get_col format !t)) do
    t := !t+1
  done ;
  !t
;;

let next_no_border format n =
  let t = ref n in
  while is_border (get_col format !t) do
    t:= !t+1
  done;
  !t
;;
(*
let show_inside_multi main format i j =
  let rec show_rec i =
    if i >= j then ()
    else begin
      let s = get_this main (as_inside (get_col format i)) in
      Dest.put s ;
      show_rec (i+1)
    end in
  show_rec i
;;
*)
let do_open_col main format span insides =
  let save_table = !in_table in
  Dest.open_cell format span insides;
  if not (as_wrap format) && math_table !in_table then begin
    display  := true ;
    Dest.open_display (display_arg !verbose)
  end ;
  if math_table !in_table && not (as_wrap format) then begin
    scan_this main "$"
  end ;
  scan_this main (as_pre format) ;
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
    scan_this main "$" ;
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
    Printf.fprintf stderr "hline: %d %d" !cur_col (Array.length !cur_format) ;
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
    pretty_formats format ;
    prerr_endline ""
  end ;

  erase_col main ;

  let start_span = find_start !cur_col
  and k,b,insides = find_end n !cur_format !cur_col 0 0 in
  let end_span = k - b in

  in_multi := true;

  let i = show_inside main format 0 true in

  Dest.open_cell_group () ;
  do_open_col main (get_col format i) (end_span - start_span) insides;
  push stack_multi (!cur_format,k) ;
  cur_format := format ;
  cur_col := i ;
;;


let close_col_aux main content is_last =
  let old_format = get_col !cur_format !cur_col in
  scan_this main (as_post old_format) ;
  if math_table !in_table && not (as_wrap old_format) then
    scan_this main "$" ;
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

let get_style lexfun (s,env) =
  start_normal env ;
  let lexer = Lexing.from_string s in
  let r = Dest.to_style (fun () -> lexfun lexer) in
  end_normal () ;
  r

(* Image stuff *)

let iput_newpage () = Image.page ()
;;

let stack_entry = Stack.create "stack_entry"
and stack_out = Stack.create  "stack_out"
;;

let start_other_scan env lexfun lexbuf =
  if !verbose > 1 then begin
    prerr_endline ("Start other scan ("^env^")") ;
    Stack.pretty
      (fun (x,_,_) -> x) stack_env ;
    prerr_endline ("Current env is: ``"^ !cur_env^"''") ;
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
    Stack.pretty (fun (x,_,_) -> x) stack_env ;
    prerr_endline ("Current env is: ``"^ !cur_env^"''")
  end
;;


let stop_other_scan comment main lexbuf =
  if !verbose > 1 then begin
    prerr_endline "Stop image: env stack is" ;
    Stack.pretty  (fun (x,_,_) -> x) stack_env ;
    prerr_endline ("Current env is: ``"^ !cur_env^"''")
  end;
  let _ = pop stack_entry in
  if not comment then close_env !cur_env ;
  if not (Stack.empty stack_out) then begin
    complete_scan main lexbuf ;
    while not (Stack.empty stack_out) do
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
| "", Parse_opts.Info ->  Out.create_buff ()
| "", _ -> Out.create_chan stdout
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

let expand_command main skip_blanks name lexbuf =
  let cur_subst = get_subst () in
  let exec = function
    | Subst body ->
        if !verbose > 2 then
          prerr_endline ("user macro: "^body) ;            
        scan_this_may_cont main lexbuf cur_subst (body,get_subst ())
    | CamlCode f -> f lexbuf in

  let pat,body = find_macro name in
  let par_before = Dest.forget_par () in
  if
    (if !in_math then Latexmacros.invisible name
    else
      not !alltt &&
      is_subst_noarg body pat && last_letter name)
  then begin
    if !verbose > 2 then
      prerr_endline "skipping blanks";
    skip_blanks lexbuf
  end else begin
    if !verbose > 2 then begin
      prerr_endline "not skipping blanks"
    end
  end ;
  let par_after = Dest.forget_par () in
  Dest.par par_before ;
  let args = make_stack name pat lexbuf in
  let saw_par = !Save.seen_par in
  let is_limit = checklimits lexbuf ||  Latexmacros.limit name in
  if not !alltt && (is_limit || Latexmacros.big name) then begin
    let sup,sub = Save.get_sup_sub lexbuf in
    let do_what =
      (fun () -> scan_body exec body args) in
    if !display && is_limit then
      Dest.limit_sup_sub (scan_this main) do_what sup sub !display
    else if !display &&  Latexmacros.int name then
      Dest.int_sup_sub true 3 (scan_this main) do_what sup sub !display
    else
      Dest.standard_sup_sub (scan_this main) do_what sup sub !display
  end else begin
    if (!verbose > 1) then begin
      prerr_endline
        ("Expanding macro "^name^" {"^(string_of_int !macro_depth)^"}") ;
      macro_depth := !macro_depth + 1
    end ;
    scan_body exec body args ;
    if (!verbose > 1) then begin
      prerr_endline ("Cont after macro "^name^": ") ;
      macro_depth := !macro_depth - 1
    end ;
    Dest.par par_after ;
    if saw_par then top_par (par_val !in_table)
  end
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
} 

let command_name = '\\' (( ['@''A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])

rule  main = parse
(* comments *)
   '%'+
   {if !alltt || not (is_plain '%') then begin
     let lxm = lexeme lexbuf in
     Dest.put lxm ;
     main lexbuf
   end else
     comment lexbuf}

(* Paragraphs *)
  | '\n' (' '* '\n')*
      {
       let lxm = lexeme lexbuf in
       let nlnum = count_newlines lxm in
       if !Lexstate.withinLispComment
       then begin
         if !verbose > 2 then prerr_endline "NL caught after LispComment" ;
         raise (Misc.EndOfLispComment nlnum) (* QNC *)
       end else begin
         if !alltt then
           Dest.put lxm
         else if nlnum >= 2 then
           top_par (par_val !in_table)
         else
           Dest.put_separator () ;
         main lexbuf
       end}
(* subscripts and superscripts *)
  | ('_' | '^')
     {let lxm = lexeme lexbuf in
     if !alltt || not (is_plain lxm.[0]) then Dest.put lxm
     else if not !in_math then begin
       warning ("``"^lxm^"''occuring outside math mode") ;
       Dest.put lxm
     end else begin
       let sup,sub = match lxm with
         "^" ->
           let sup = Save.arg lexbuf in
           let sub = Save.get_sub lexbuf in
           sup,sub
       | _   ->
           let sub = Save.arg lexbuf in
           let sup = Save.get_sup lexbuf in
           sup,sub in
       Dest.standard_sup_sub (scan_this main) (fun () -> ()) sup sub !display
     end ;
     main lexbuf}
(* Math mode *)
| "$" | "$$"
     {let lxm = lexeme lexbuf in
     if !alltt || not (is_plain '$') then begin
       Dest.put lxm ; main lexbuf
     end else begin
       let dodo = lxm <> "$" in
       let math_env = if dodo then "*display" else "*math" in
       if !in_math then begin
         in_math := pop stack_in_math ;
         if dodo then begin
	   Dest.close_maths dodo;
         end else begin
           top_close_display () ;
	   Dest.close_maths dodo;
         end ;
         display := pop stack_display ;
         if !display then begin
           Dest.item_display ()
         end ;
         close_env math_env ;
         main lexbuf
     end else begin
       push stack_in_math !in_math ;
       in_math := true ;
       let lexfun lb =
         if !display then  Dest.item_display () ;
         push stack_display !display ;
         if dodo then begin
           display  := true ;
	   Dest.open_maths dodo;
         end else begin
	   Dest.open_maths dodo;
           top_open_display () ;
         end;
         skip_blanks lb ; main lb in
       new_env math_env ;
       lexfun lexbuf
     end end}

(* Definitions of  simple macros *)
(* inside tables and array *)
  | [' ''\n']* "&"
    {if !alltt || not (is_plain '&') then begin
      let lxm = lexeme lexbuf in
      for i = 0 to String.length lxm -1 do
        Dest.put (Dest.iso lxm.[i])
      done
    end else if is_table !in_table  then begin
      close_col main "&nbsp;"; 
      open_col main
    end ;
    if not !alltt && is_plain '&' then skip_blanks_pop lexbuf ;
    main lexbuf}
(* Substitution  *)
  | '#' ['1'-'9']
      {let lxm = lexeme lexbuf in
      begin if !alltt || not (is_plain '#') then
        Dest.put lxm
      else
        let i = Char.code lxm.[1] - Char.code '1' in
        scan_arg (scan_this main) i
      end ;
      main lexbuf}
(* Commands *)
  | command_name
      {let name = lexeme lexbuf in
      expand_command main skip_blanks name lexbuf ;
      main lexbuf}
(* Groups *)
| '{' 
    {if !activebrace && is_plain '{' then
      top_open_group ()
    else begin
      Dest.put_char '{'
    end ;
    main lexbuf}
| '}' 
    {if !activebrace && is_plain '}' then begin
      let cenv = !cur_env in
      top_close_group ()
    end else begin
      Dest.put_char '}'
    end ;
    main lexbuf}
| eof {()}
| ' '+
   {if !alltt then
     let lxm = lexeme lexbuf in Dest.put lxm
   else
     Dest.put_char ' ';
   main lexbuf}
(* Alphabetic characters *)
| ['a'-'z' 'A'-'Z']+
   {let lxm =  lexeme lexbuf in
   if !in_math then begin
      Dest.put_in_math lxm;
    end else
      Dest.put lxm ;
    main lexbuf}
(* Numbers *)
| ['0'-'9']+
    {let lxm = lexeme lexbuf in
    Dest.put lxm;
    main lexbuf}
(* Html specials *)
| '~'
  {if !alltt || not (is_plain '~') then Dest.put_char '~'
  else Dest.put_nbsp () ;
  main lexbuf }
(* Spanish stuff *)
| "?`"
    {if !alltt then Dest.put "?`"
    else Dest.put (Dest.iso '¿') ;
    main lexbuf}
| "!`"
  {if !alltt then Dest.put "!`"
  else Dest.put (Dest.iso '¡') ;
  main lexbuf}
(* One character *)
| _ 
   {let lxm = lexeme_char lexbuf 0 in
   Dest.put (Dest.iso lxm) ;
   main lexbuf}

and latex2html_latexonly = parse
| '%' + [ ' ' '\t' ] * "end{latexonly}" [ ^ '\n' ] * '\n'
    { () }
| _ 
    {latex2html_latexonly lexbuf}
| eof
    {failwith "EOF in latexonly"}

and latexonly = parse
   '%'+ ' '* ("END"|"end") ' '+ ("LATEX"|"latex")  [^'\n']* '\n'
     {stop_other_scan true main lexbuf}
|  '%'+ ' '* ("HEVEA"|"hevea") ' '*
     {latexonly lexbuf}
|  '%'
     {latex_comment lexbuf ; latexonly lexbuf}
|  "\\end"
     {let arg,_ = save_arg lexbuf in
     if arg = "latexonly" then begin
       top_close_block "" ;
       stop_other_scan false main lexbuf
     end else if arg = top stack_entry then begin
       let _ = pop stack_entry in
       push stack_out arg ;
       begin match find_macro (end_env arg) with
         _,(Subst body) ->
           scan_this_may_cont latexonly lexbuf (get_subst ())
             (body,get_subst ())
       |  _,_ ->
           raise (Misc.ScanError ("Bad closing macro in latexonly: ``"^arg^"''"))
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
    scan_arg (scan_this image) i ;
    image lexbuf}
|  "\\end"
     {let lxm = lexeme lexbuf in
     Save.start_echo () ;
     let arg,_ = save_arg lexbuf in
     let true_arg = Save.get_echo () in
     if arg = "toimage" then begin
       top_close_block "" ;
       stop_other_scan false main lexbuf
     end else if arg = top stack_entry then begin
       let _ = pop stack_entry in
       push stack_out arg ;
       begin match find_macro (end_env arg) with
         _,(Subst body) ->
           scan_this_may_cont  image lexbuf (get_subst ())
             (body,get_subst ())
       |  _,_ -> raise (Misc.ScanError ("Bad closing macro in image: ``"^arg^"''"))
       end
     end else begin
       Image.put lxm ; Image.put true_arg ;
       image lexbuf
     end}
|  command_name
    {let lxm = lexeme lexbuf in
    begin match lxm with
(* Definitions of  simple macros, bodies are not substituted *)
    | "\\def" | "\\gdef" ->
        Save.start_echo () ;
        skip_csname lexbuf ;
        skip_blanks lexbuf ;
        let _ = Save.defargs lexbuf in
        let _ = save_arg lexbuf in
        Image.put lxm ;
        Image.put (Save.get_echo ())
    | "\\renewcommand" | "\\newcommand" | "\\providecommand" ->
        Save.start_echo () ;
        skip_csname lexbuf ;
        let _ = save_opts ["0" ; ""] lexbuf in
        let _ = save_arg lexbuf in
        Image.put lxm ;
        Image.put (Save.get_echo ())
    | "\\newenvironment" | "\\renewenvironment" ->
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
    {push stack_table !in_table ; in_table := NoTable ;
    push stack_in_math !in_math ; in_math := false ;
    if !display then Dest.item_display () ;
    push stack_display !display ; display := false ;
    Dest.open_block "" "" ;
    new_env "*mbox"}

and skip_blanks_pop = parse
  ' '+ {skip_blanks_pop lexbuf}
| '\n' {more_skip_pop lexbuf}
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

and more_skip_pop = parse
  '\n'+ {top_par (par_val !in_table)}
| ""    {skip_blanks_pop lexbuf}
| eof
   {if not (empty stack_lexbuf) then begin
     let lexbuf = previous_lexbuf () in
     if !verbose > 2 then begin
       prerr_endline "Poping lexbuf in skip_blanks" ;
       pretty_lexbuf lexbuf
     end ;
     more_skip_pop lexbuf
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
| ""   {()}

and more_skip = parse
  '\n'+ {top_par (par_val !in_table)}
| ""    {skip_blanks lexbuf}

and skip_spaces_main = parse
  ' ' * {main lexbuf}
| eof   {main lexbuf}


and skip_false = parse
|  '%'
     {if is_plain '%' then skip_comment lexbuf ;
       skip_false lexbuf}
|  "\\if" ['a'-'z' 'A'-'Z']+
     {if_level := !if_level + 1 ;
     skip_false lexbuf}
| "\\else" ['a'-'z' 'A'-'Z']+
     {skip_false lexbuf}
| "\\else"
     {if !if_level = 0 then skip_blanks lexbuf
     else skip_false lexbuf}
| "\\fi" ['a'-'z' 'A'-'Z']+
     {skip_false lexbuf}
| "\\fi"
     {if !if_level = 0 then begin
        skip_blanks lexbuf
     end else begin
       if_level := !if_level -1 ;
       skip_false lexbuf
     end}
| _  {skip_false lexbuf}
| "" {raise (Error "End of entry while skipping TeX conditional macro")}

and comment = parse
  ' '* ("BEGIN"|"begin") ' '+ ("IMAGE"|"image")
    {skip_comment lexbuf ; start_image_scan "" image lexbuf ; main lexbuf}
(* Backward compatibility with latex2html *)
| [ ' ' '\t' ] * "begin{latexonly}"
    {latex2html_latexonly lexbuf;
     main lexbuf}
| ' '* ("HEVEA"|"hevea") ' '*
   {main lexbuf}
| ' '* ("BEGIN"|"begin") ' '+ ("LATEX"|"latex")
    {skip_comment lexbuf ;
    start_other_scan "latexonly" latexonly lexbuf ;
    skip_spaces_main lexbuf}
| ""
    {skip_comment lexbuf ; more_skip lexbuf ; main lexbuf}

and skip_comment = parse    
   [^ '\n']* '\n'
   {if !verbose > 1 then
     prerr_endline ("Comment:"^lexeme lexbuf) ;
   if !flushing then Dest.flush_out () }
| "" {raise (Misc.ScanError "Latex comment is not terminated")}


{
let check_alltt_skip lexbuf =
  if not (Stack.empty stack_alltt) && pop stack_alltt then begin
    push stack_alltt true
  end else begin
    push stack_alltt false ;
    skip_blanks lexbuf
  end
;;

let get_this_main arg = get_this main arg
let check_this_main s =
  if !verbose > 1 then
    prerr_endline ("check_this: ``"^s^"''");
  start_normal (get_subst ()) ;
  let save_par = Dest.forget_par () in
  Dest.open_block "TEMP" "";
  let r =
    try
      scan_this main s ;
      true
    with
    |  x -> false in
  Dest.erase_block "TEMP" ;
  Dest.par save_par ;
  end_normal () ;
  if !verbose > 1 then
    prerr_endline ("check_this: ``"^s^"'' = "^sbool r);
  r
  


let get_prim_onarg arg =
  let plain_sub = is_plain '_'
  and plain_sup = is_plain '^'
  and plain_dollar = is_plain '$' in
  unset_plain '_' ; unset_plain '^' ; unset_plain '$' ;
  let r = get_this_nostyle_arg main arg in
  plain_back plain_sub '_' ; plain_back plain_sup '^' ;
  plain_back plain_dollar '$' ;
  r

let get_prim s = get_prim_onarg (s,get_subst ())

let get_prim_arg lexbuf =
  let arg = save_arg lexbuf in
  get_prim_onarg arg

and get_prim_opt def lexbuf =
  let arg = save_opt def lexbuf in
  get_prim_onarg arg

let def_fun name f =
  def_code name
    (fun lexbuf ->
      let arg = subst_arg lexbuf in
      scan_this main (f arg))
;;

(* Styles and packages *)
let do_documentclass command lexbuf =
  Save.start_echo () ;
  let opt_arg,_ = save_opt "" lexbuf in
  let arg,_ =  save_arg lexbuf in
  let real_args = Save.get_echo () in
  begin try if not !styleloaded then
    input_file 0 main (arg^".hva")
  with
    Myfiles.Except | Myfiles.Error _ ->
      raise (Misc.ScanError ("No base style"))
  end ;
  if command = "\\documentstyle" then begin
    let rec read_packages = function
      | [] -> ()
      | pack :: rest ->
          scan_this main ("\\usepackage{"^pack^"}") ;
          read_packages rest in
    read_packages
      (Save.cite_arg (Lexing.from_string ("{"^opt_arg^"}")))
  end ;
  Image.start () ;
  Image.put command ;
  Image.put real_args ;
  Image.put_char '\n'  
;;

def_name_code  "\\documentstyle" do_documentclass ;
def_name_code  "\\documentclass" do_documentclass
;;


let do_input lxm lexbuf =
  Save.start_echo () ;
  let arg = get_prim_arg lexbuf in
  let echo_arg = Save.get_echo () in
  if lxm <> "\\include" || check_include arg then begin
    let filename =
      if lxm = "\\bibliography" then Parse_opts.base_in^".bbl"
      else arg in
    
    begin try input_file !verbose main filename
    with Myfiles.Except ->
      Image.put lxm ;
      Image.put echo_arg ;
      Image.put "\n" ;
    | Myfiles.Error _ -> ()
    end
  end
;;

def_code "\\input" (do_input "\\input") ;
def_code "\\include" (do_input "\\include") ;
def_code "\\bibliography" (do_input "\\bibliography")
;;

(* Command definitions *)

let do_newcommand lxm lexbuf =
  Save.start_echo () ;
  let name = subst_csname lexbuf in
  let nargs = save_opts ["0" ; ""] lexbuf in
  let body = subst_body lexbuf in
  if (!env_level = 0) && lxm <> "\\@forcecommand"  && top_level () then
    Image.put
      (lxm^Save.get_echo ()^"\n") ;
  let nargs,(def,defval) = match nargs with
    [a1 ; a2] ->
      Get.get_int (from_ok a1),
      (match a2 with
      | No s,env -> false,(s,env)
      | Yes s,env -> true,(s,env))
  | _ -> assert false in
  begin try
    (match lxm with
      "\\newcommand"   -> def_macro_pat
    | "\\renewcommand" -> redef_macro_pat
    | "\\@forcecommand" -> silent_def_pat        
    | _                -> provide_macro_pat) name
      (Latexmacros.make_pat
         (if def then [do_subst_this defval]
         else []) nargs)
      (Subst body) ;
    macro_register name
  with Latexmacros.Failed -> () end
;;

def_name_code "\\renewcommand" do_newcommand  ;
def_name_code "\\newcommand" do_newcommand ;
def_name_code "\\providecommand" do_newcommand ;
def_name_code "\\@forcecommand" do_newcommand ;
;;

def_name_code "\\newcolumntype"
  (fun lxm lexbuf ->
    Save.start_echo () ;
    let name = subst_csname lexbuf in
    let nargs = save_opt "0" lexbuf in
    let body = subst_body lexbuf in
    let rest = Save.get_echo () in
    if !env_level = 0 then
      Image.put (lxm^rest^"\n") ;
    def_coltype
      name
      (Latexmacros.make_pat [] (Get.get_int nargs))
      (Subst body) ;
    macro_register (Misc.column_to_command name))
;;

let do_newenvironment lxm lexbuf =
  Save.start_echo () ;
  let name = get_prim_arg lexbuf in
  let nargs,optdef = match save_opts ["0" ; ""] lexbuf with
  |  [x ; y ] -> x,y
  | _ -> assert false in
  let body1 = subst_body lexbuf in
  let body2 = subst_body lexbuf in
  if !env_level = 0 then
    Image.put (lxm^Save.get_echo ()^"\n") ;
  begin try
    (match lxm with
      "\\newenvironment" -> def_env_pat
    |  _ -> redef_env_pat) name
      (Latexmacros.make_pat
         (match optdef with
         | No _,_    -> []
         | Yes s,env -> [do_subst_this (s,env)])
         (match nargs with No _,_ -> 0 | Yes s,env -> Get.get_int (s,env)))
      (Subst body1) (Subst body2);
    macro_register (start_env name) ; 
    macro_register (end_env name)
  with Latexmacros.Failed -> () end
;;

def_name_code "\\newenvironment" do_newenvironment ;
def_name_code  "\\renewenvironment" do_newenvironment
;;

let do_newtheorem lxm lexbuf =
  Save.start_echo () ;
  let name = subst_csname lexbuf in
  let numbered_like = match save_opts [""] lexbuf with
  |  [x] -> x
  | _ -> assert false in
  let caption = subst_arg lexbuf in
  let within =  match save_opts [""] lexbuf with
  | [x] -> x
  | _   -> assert false in
  if !env_level = 0 then
    Image.put (lxm^Save.get_echo ()^"\n") ;
  begin try
    let cname = match numbered_like,within with
      (No _,_),(No _,_) ->
        Counter.def_counter name "" ;
        def_macro ("\\the"^name) 0 (Subst ("\\arabic{"^name^"}")) ;
        name
    | _,(Yes within,env) ->
        let within = do_subst_this (within,env) in
        Counter.def_counter name within ;
        def_macro ("\\the"^name) 0
          (Subst ("\\the"^within^".\\arabic{"^name^"}")) ;
        name
    | (Yes numbered_like,env),_ ->
        do_subst_this (numbered_like,env) in
    
    def_env_pat name (Latexmacros.make_pat [""] 0)
      (Subst ("\\cr{\\bf\\stepcounter{"^cname^"}"^caption^"~"^
              "\\the"^cname^"}\\quad\\ifoptarg{\\purple[#1]\\quad}\\fi\\it"))
      (Subst "\\cr")
  with Latexmacros.Failed -> () end
;;

def_name_code "\\newtheorem" do_newtheorem ;
def_name_code "\\renewtheorem" do_newtheorem
;;

(* Command definitions, TeX style *)

let do_def realdef global lxm lexbuf =
  let name = subst_csname lexbuf in
  skip_blanks lexbuf ;
  let name,args_pat,body =
    if top_level () then
      let args_pat = Save.defargs lexbuf in
      let body,_ = save_arg lexbuf in
      name,args_pat,body
    else
      let args_pat =
        Save.defargs
          (Lexing.from_string
             (subst_this (Save.get_defargs lexbuf))) in
      let body = subst_arg lexbuf in
      name,args_pat,body in
  if not realdef && (!env_level = 0 || global) then
    Image.put
      (lxm^name^
       (List.fold_right (fun s r -> s^r) args_pat ("{"^body^"}\n"))) ;
  begin try
    (if realdef then silent_def_pat else def_macro_pat)
      name ([],args_pat) (Subst body) ;
    if not global then macro_register name
  with Latexmacros.Failed -> () end
;;

def_name_code "\\def" (do_def false false) ;
def_name_code "\\texdef" (do_def true false) ;
def_name_code "\\gdef" (do_def false true)
;;

let do_let reallet global lxm lexbuf =
  let name = subst_csname lexbuf in
  Save.skip_equal lexbuf ;
  let alt = subst_csname lexbuf in
  let nargs,body = find_macro alt in
  begin try
    (if reallet then silent_def_pat else def_macro_pat)
      name nargs body ;
    if not global then macro_register name
  with Latexmacros.Failed -> ()
  end ;
  if not reallet && (!env_level = 0 || global) then begin
    Image.put lxm ;
    Image.put name ;
    Image.put "=" ;
    Image.put alt ;
    Image.put "\n"
  end
;;

def_name_code "\\let" (do_let false false) ;
def_name_code "\\texlet" (do_let true false)
;;

let do_global lxm lexbuf =
  let next = subst_arg lexbuf in
  begin match next with
  | "\\def" -> do_def false true (lxm^next) lexbuf
  | "\\texdef" -> do_def true true (lxm^next) lexbuf
  | "\\let" -> do_let false true (lxm^next) lexbuf
  | "\\texlet" -> do_let true true (lxm^next) lexbuf
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

let do_unskip () =
 let _ = Dest.forget_par () in
 Dest.unskip ()
;;

def_code "\\unskip"
    (fun lexbuf ->
      do_unskip () ;
      check_alltt_skip lexbuf)
;;

def_code "\\csname"
  (fun lexbuf ->
    skip_blanks lexbuf ;
    let name = "\\"^subst_this (Save.incsname lexbuf) in
    check_alltt_skip lexbuf ;
    expand_command main skip_blanks name lexbuf)
;;

def_code "\\string"
   (fun lexbuf ->
     let arg = subst_arg lexbuf in
     Dest.put arg)
;;

let get_num_arg lexbuf =
  Save.num_arg lexbuf (fun s -> Get.get_int (s,get_subst ()))
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
     let char = Char.chr
         (Get.get_int (Save.with_delim "=" lexbuf,get_subst ()))in
     let code = get_num_arg lexbuf in
     begin match char,code with
     | ('\\',0) | ('{',1) | ('}',2) | ('$',3) | ('&' ,4) |
       ('#',6) | ('^',7) | ('_',8) | ('~',13) |
       ('%',14) -> top_plain char
     | ('{',(11|12)) | ('}',(11|12)) | ('$',(11|12)) | ('&' ,(11|12)) |
       ('#',(11|12)) | ('^',(11|12)) | ('_',(11|12)) | ('~',(11|12)) |
       ('%',(11|12)) | ('\\',(11|12)) -> top_unplain char
     | _ ->
         warning "This \\catcode operation is not permitted"
     end ;
     main lexbuf)
;;

def_code "\\chardef"
  (fun lexbuf ->
    let csname = Subst.subst_csname lexbuf in
    Save.skip_equal lexbuf ;
    let i = get_num_arg lexbuf in
    macro_register csname ;
    Latexmacros.silent_def csname 0 (Subst (string_of_int i)))
;;

(* Complicated use of output blocks *)
def_code "\\left"
  (fun lexbuf ->
    if !display then begin
      let delim = subst_arg lexbuf in
      Dest.left delim;
    end)
;;

def_code "\\right"
  (fun lexbuf ->
    if !display then begin
      let delim = subst_arg lexbuf in
      let vsize = Dest.right delim in
      let sup,sub = Save.get_sup_sub lexbuf in
      let do_what = (fun () -> ()) in
      Dest.int_sup_sub false vsize (scan_this main) do_what sup sub !display
    end ;
    check_alltt_skip lexbuf)
;;

def_code "\\over"
   (fun lexbuf ->
     Dest.over !display lexbuf;
     skip_blanks lexbuf)
;;

let check_not = function
  | "\\in" -> "\\notin"
  | "="    -> "\\neq"
  | "\\subset" -> "\\notsubset"
  | s -> "\\neg\\:"^s
;;

def_fun "\\not" check_not
;;

def_fun "\\uppercase" String.uppercase
;;

(* list items *)
def_code "\\@li" (fun _ -> Dest.item ()) ;
def_code "\\@linum" (fun _ -> Dest.nitem ()) ;
def_code "\\@dt"
  (fun lexbuf ->
    let arg = subst_arg lexbuf in
    Dest.ditem (scan_this main) arg)
;;

    
(* Html primitives *)
def_code "\\@open"
  (fun lexbuf ->
    let tag = get_prim_arg lexbuf in
    let arg = get_prim_arg lexbuf in
    if no_display tag then begin
      if tag="DISPLAY" then begin
        push stack_display !display;
        display := true ;
        top_open_display ()
      end else begin
        if !verbose > 2 then warning ("direct opening of "^tag);
        top_open_block tag arg
      end
    end else
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
    if no_display tag then begin
      if tag="DISPLAY" then begin
        top_close_display ();
        display := pop stack_display
      end else begin
        if !verbose > 2 then warning ("direct closing of "^tag);
        top_close_block tag
      end
    end else
      top_close_block tag)
;;

def_code "\\@print"
  (fun lexbuf ->
          let arg,_ = save_arg lexbuf in
          Dest.put arg) ;
def_code "\\@subst"
  (fun lexbuf ->
    let arg = subst_arg lexbuf in
    Dest.put arg)
;;

def_code "\\@notags"
  (fun lexbuf ->
          let arg = save_arg lexbuf in
          let arg = get_this_arg main arg in
          let r =
            let buff = Lexing.from_string arg in
            Save.tagout buff in
          Dest.put r)
;;
def_code "\\@anti"
  (fun lexbuf ->
          let arg = save_arg lexbuf in
          let envs = get_style main arg in
          if !verbose > 2 then begin
            prerr_string "Anti result: " ;
            List.iter
              (fun s ->
                prerr_string (Latexmacros.pretty_env s^", ")) envs ;
            prerr_endline ""
          end ;
          Dest.erase_mods envs)
;;
def_code "\\@style"  
  (fun lexbuf ->
          let arg = get_prim_arg lexbuf in
          Dest.open_mod (Style arg) )
;;
def_code "\\@fontcolor"  
  (fun lexbuf ->
          let arg = get_prim_arg lexbuf in
          Dest.open_mod (Color arg) )
;;
def_code "\\@fontsize"  
  (fun lexbuf ->
          let arg = save_arg lexbuf in
          Dest.open_mod (Font (Get.get_int arg)) )
;;
def_code "\\@nostyle" (fun lexbuf -> Dest.nostyle () )
;;
def_code "\\@clearstyle" (fun lexbuf -> Dest.clearstyle () )
;;
def_code "\\@incsize"
  (fun lexbuf ->
          let arg = save_arg lexbuf in
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
          Dest.set_dcount arg )
;;
def_code "\\@fromlib"
  (fun lexbuf ->
          let arg = get_prim_arg lexbuf in
          start_lexstate ();
          Mylib.put_from_lib arg Dest.put;
          restore_lexstate ())
;;
def_code "\\@imageflush"
  (fun lexbuf ->
    iput_newpage () ;
    check_alltt_skip lexbuf)
;;
def_code "\\textalltt"
  (fun lexbuf ->
       let opt = get_prim_opt "CODE" lexbuf in
       let arg = save_arg lexbuf in
       let old = !alltt in
       scan_this main "\\mbox{" ;
       alltt := true ;
       Dest.open_group opt ;
       scan_this_arg main arg ;
       Dest.close_group () ;
       scan_this main "}" ;
       alltt := old )
;;
def_code "\\@itemdisplay"
  (fun lexbuf -> Dest.force_item_display ())
;;
def_code "\\@br"
  (fun lexbuf -> Dest.skip_line ())
;;


(* TeX conditionals *)
let testif cell lexbuf =
  if !cell then check_alltt_skip lexbuf
  else skip_false lexbuf

let setif cell b lexbuf =
  cell := b ;
  check_alltt_skip lexbuf 
;;

let extract_if name =
  let l = String.length name in
  if l <= 3 || String.sub name 0 3 <> "\\if" then
    raise (Error ("Bad newif: "^name)) ;
  String.sub name 3 (l-3)
;;

let def_and_register name f =
  def_code name f ; macro_register name
;;


let newif lexbuf =
  let arg = subst_arg lexbuf in
  try
    let name = extract_if arg in
    let cell = ref false in
    def_and_register ("\\if"^name) (testif cell) ;
    def_and_register ("\\"^name^"true") (setif cell true) ;
    def_and_register ("\\"^name^"false") (setif cell false) ;
    if !env_level > 0 then register_cell name cell
  with Latexmacros.Failed -> ()
;;

def_code "\\ifx"
  (fun lexbuf ->
    let arg1 = subst_csname lexbuf in
    let arg2 = subst_csname lexbuf  in
    if silent_find_macro arg1 = silent_find_macro arg2 then
      check_alltt_skip lexbuf
    else skip_false lexbuf)
;;
    
    
def_code "\\newif" newif 
;;

def_code "\\else" (fun lexbuf -> skip_false lexbuf)
;;

def_code "\\fi" (fun lexbuf -> check_alltt_skip lexbuf)
;;


let newif_ref name cell =
  def_code ("\\if"^name) (testif cell) ;
  def_code ("\\"^name^"true") (setif cell true) ;
  def_code ("\\"^name^"false") (setif cell false) ;
  register_cell name cell              
;;

newif_ref "symb" symbols ;
newif_ref "iso" iso ;
newif_ref "alltt" alltt ;
newif_ref "silent" silent;
newif_ref "math" in_math ;
newif_ref "mmode" in_math ;
newif_ref "display" display ;
newif_ref "french" french ;
newif_ref "html" html;
newif_ref "text" text;
newif_ref "info" text;
newif_ref "mathml" Parse_opts.mathml;
newif_ref "entities" Parse_opts.entities;
newif_ref "optarg" optarg;
newif_ref "styleloaded" styleloaded;
newif_ref "activebrace" activebrace;
newif_ref "pedantic" pedantic ;
newif_ref "fixpoint" fixpoint ;
def_code ("\\iftrue") (testif (ref true)) ;
def_code ("\\iffalse") (testif (ref false))
;;


(* Bibliographies *)
let bib_ref s1 s2 =
  scan_this main ("\\@bibref{"^s1^"}{"^s2^"}")
;;

def_code "\\cite"
  (fun lexbuf ->
    let opt = save_opt "" lexbuf in
    check_alltt_skip lexbuf ; 
    let args = List.map subst_this (Save.cite_arg lexbuf) in
    Dest.put_char '[' ;
    Dest.open_group "CITE" ;
    let rec do_rec = function
        [] -> ()
      | [x] -> bib_ref x (Auxx.bget x)
      | x::rest ->
          bib_ref x (Auxx.bget x) ;
          Dest.put ", " ;
          do_rec rest in
    do_rec args ;
    if fst opt <> "" then begin
      Dest.put ", " ;
      scan_this_arg main opt ;
    end ;
    Dest.close_group () ;
    Dest.put_char ']' )
;;

def_fun "\\@bibread" Auxx.bget
;;

def_code "\\@bibwrite"
  (fun lexbuf ->
    let pretty = match Subst.subst_arg lexbuf with
    | "\\theheveabib" as s  -> get_prim s
    | s -> s in
    let key = get_prim_arg lexbuf in
    Auxx.bwrite key pretty)
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
    let mark = Get.get_int (save_arg lexbuf) in
    Foot.step_anchor mark) ;
def_code "\\@anchorval"
  (fun lexbuf ->
    let mark = Get.get_int (save_arg lexbuf) in
    Dest.put (string_of_int (Foot.get_anchor mark)))
;;

def_code "\\@footnotetext"
  (fun lexbuf ->
    start_lexstate () ; 
    let mark = Get.get_int (save_arg lexbuf) in
    let text,env = save_arg lexbuf in
    let text = get_this_arg main ("\\@clearstyle "^text,env) in
    Foot.register
      mark
      (get_this main ("\\@fnmarknote{"^string_of_int mark^"}"))
      text ;
    restore_lexstate ())
;;

def_code "\\@footnoteflush"
  (fun lexbuf ->
    let sec_here = get_prim_arg lexbuf
    and sec_notes = get_this_nostyle main "\\@footnotelevel" in
    start_lexstate () ;
    Foot.flush (scan_this main) sec_notes sec_here ;
    restore_lexstate ())
;;

(* Opening and closing environments *)


def_code "\\begin"
  (fun lexbuf ->
    let cur_subst = get_subst () in
    let env = get_prim_arg lexbuf in
    if env = "document" && not filter then begin
      Image.put "\\pagestyle{empty}\n\\begin{document}\n";
      let _ = Dest.forget_par () in () ;
      Dest.set_out (mk_out_file ())
    end ;
    new_env env ;
    let macro = "\\csname "^env^"\\endcsname"  in
    if env <> "document" then
      top_open_block "" "" ;
    let old_envi = save stack_entry in
    push stack_entry env ;
    scan_this_may_cont main lexbuf cur_subst (macro,get_subst ()) ;
    restore stack_entry old_envi)
;;

def_code "\\end"
  (fun lexbuf ->
    let env = get_prim_arg lexbuf in
    scan_this main ("\\csname end"^env^"\\endcsname") ;
    if env <> "document" then top_close_block "" ;
    close_env env ;
    if env = "document" then raise Misc.EndDocument)
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

let def_print name s = def_code name (fun _ -> Dest.put s)
and redef_print name s = redef_code name (fun _ -> Dest.put s)
;;


def_code "\\newsavebox"
  (fun lexbuf ->
    let name = subst_csname lexbuf in
    begin try def_print name ""
    with Latexmacros.Failed -> () end )
;;

def_code "\\savebox" 
  (fun lexbuf ->
    let name = subst_csname lexbuf in
    warning "savebox";
    skip_opt lexbuf ;
    skip_opt lexbuf ;
    let body = save_arg lexbuf in
    redef_print name (get_this_arg main body) ;
    macro_register name)
;;

def_code "\\sbox"
  (fun lexbuf ->
    let name = subst_csname lexbuf in
    let body = save_arg lexbuf in
    redef_print name (get_this_arg main body) ;
    macro_register name)
;;

def_code "\\usebox"
  (fun lexbuf ->
    let arg = save_arg lexbuf in
    scan_this_arg main arg)
;;

def_code "\\lrbox"
  (fun _ ->
    close_env "lrbox" ;
    let lexbuf = previous_lexbuf () in
    let name = subst_csname lexbuf in
    Dest.open_aftergroup
      (fun s ->
        redef_print name s ;
        macro_register name ;
        "") ;
    scan_this main ("\\mbox{"))
;;

def_code "\\endlrbox"
  (fun _ ->
    scan_this main "}" ; Dest.force_block "" "" ; new_env "lrbox")
;;


(* chars *)
def_code "\\char"
  (fun lexbuf ->
    let arg = get_num_arg lexbuf in
    if not !silent && (arg < 32 || (arg > 127 && arg < 161)) then begin
      Location.print_pos () ;
      prerr_endline ("Warning: \\char, check output");
    end ;
    Dest.put (Dest.iso (Char.chr arg)) ;
    if not !alltt then check_alltt_skip lexbuf)
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
| 3 -> "%"
| 4 -> "\167"
| 5 -> "\182"
| 6 -> "||"
| 7 -> "**"
| 8 -> "##"
| 9 -> "%%"
| i -> alpha_of_int (i-9)
;;

let def_printcount name f =
  def_code name
    (fun lexbuf ->
      let cname = get_this_nostyle_arg main (save_arg lexbuf) in
      let cval = Counter.value_counter cname in
      Dest.put (f cval))
;;

def_printcount "\\arabic" string_of_int ;
def_printcount "\\@arabic" (fun i -> Printf.sprintf "%0.3d" i) ;
def_printcount "\\alph"  alpha_of_int ;
def_printcount "\\Alph"  upalpha_of_int ;
def_printcount "\\roman" roman_of_int;
def_printcount "\\Roman" uproman_of_int;
def_printcount "\\fnsymbol" fnsymbol_of_int
;;


def_code "\\newcounter"
  (fun lexbuf ->
    let name = get_this_nostyle_arg main (save_arg lexbuf) in
    let within = save_opt "" lexbuf in
    let within = get_this_nostyle_arg main within in
    try
      Counter.def_counter name within ;
      def_macro ("\\the"^name) 0 (Subst ("\\arabic{"^name^"}"))
    with
    | Latexmacros.Failed -> ())
;;

def_code "\\addtocounter"
  (fun lexbuf ->
    let name = get_this_nostyle_arg main (save_arg lexbuf) in
    let arg = save_arg lexbuf in
    Counter.add_counter name (Get.get_int arg))
;;

def_code "\\setcounter"
  (fun lexbuf ->
          let name = get_this_nostyle_arg main (save_arg lexbuf) in
          let arg = save_arg lexbuf in
          Counter.set_counter name (Get.get_int arg) )
;;

def_code "\\stepcounter"
  (fun lexbuf ->
          let name = get_this_nostyle_arg main (save_arg lexbuf) in
          Counter.step_counter name )
;;

def_print "\\@currentlabel" "" ;
def_code "\\refstepcounter"
  (fun lexbuf ->
          let name = get_this_nostyle_arg main (save_arg lexbuf) in
          Counter.step_counter name ;
          redef_print "\\@currentlabel"
            (get_this_clearstyle main ("\\the"^name)) ;
          macro_register "\\@currentlabel")
;;

def_code "\\numberwithin"
  (fun lexbuf ->
          let name = get_this_nostyle_arg main (save_arg lexbuf) in
          let within = get_this_nostyle_arg  main (save_arg lexbuf) in
          Counter.number_within name within )
;;

(* terminal output *)
def_code "\\typeout"
  (fun lexbuf ->
    let what = subst_arg lexbuf in
    prerr_endline what )
;;

def_code "\\warning"
  (fun lexbuf ->
    let what = subst_arg lexbuf in
    warning what )
;;

(* spacing *)

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
    
exception Cannot
;;

let do_space vert lexbuf  = 
  let arg = subst_arg lexbuf in
  begin try
    let n = match Length.main (Lexing.from_string arg) with
    | Length.Char n -> n
    | Length.Pixel n -> Length.pixel_to_char n
    | _                 -> raise Cannot in
    if vert then
      for i=1 to n do
        Dest.skip_line ()
      done
    else
      for i=1 to n do
        Dest.put_nbsp (); (* "&nbsp;"*)
      done
  with Cannot ->
    warning ((if vert then "\\vspace" else "\\hspace")^
             " with arg ``"^arg^"''")
  end
;;

def_code "\\hspace"  (fun lexbuf -> do_space false lexbuf) ;
def_code "\\vspace"  (fun lexbuf -> do_space true lexbuf)
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

def_code "\\alltt"
  (fun _ ->
    alltt := true ; Dest.close_block "" ; Dest.open_block "PRE" "") ;
def_code "\\endalltt"
  (fun _ ->
    alltt := true ; Dest.close_block "PRE" ; Dest.open_block "" "")
;;

(* Multicolumn *)

def_code "\\multicolumn"
    (fun lexbuf ->
      if not (is_table !in_table) then
        raise (ScanError "\\multicolumn should occur in some array") ;
      let n = Get.get_int (save_arg lexbuf) in      
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
    Dest.close_cell ""; Dest.open_cell default_format 1 0
  end ;
  skip_blanks_pop lexbuf
;;

def_code "\\>" do_tabul ;
def_code "\\=" do_tabul
;;

def_code "\\kill"
  (fun lexbuf ->
    if is_tabbing !in_table then begin
      do_unskip () ;
      Dest.close_cell "";
      Dest.erase_row () ;
      Dest.new_row () ;
      Dest.open_cell default_format 1 0
    end ;
    skip_blanks_pop lexbuf)
;;


(* Tabular and arrays *)


let open_tabbing lexbuf =
  top_close_block "" ;
  let lexbuf = Lexstate.previous_lexbuf in
  let lexfun lb =
    Dest.open_table false "CELLSPACING=0 CELLPADDING=0" ;
    Dest.new_row ();
    Dest.open_cell default_format 1 0 in
  push stack_table !in_table ;
  in_table := Tabbing ;
  new_env "tabbing" ;
  silent_def "\\a" 0
    (CamlCode
       (fun lexbuf ->
         let acc = subst_arg lexbuf in
         let arg = subst_arg lexbuf in
         scan_this main ("\\"^acc^arg))) ;
  macro_register "\\a" ;
  lexfun lexbuf
;;

def_code "\\tabbing" open_tabbing
;;

let check_width = function
  | Length.Char x ->
      " WIDTH="^string_of_int (Length.char_to_pixel x)
  | Length.Pixel x ->
      " WIDTH="^string_of_int x
  | Length.Percent x ->
      " WIDTH=\""^string_of_int x^"%\""
  | _ -> ""
;;

let open_array env lexbuf =

  save_array_state ();
  Tabular.border := false ;
  let len =  match env with
    | "tabular*" ->
        let arg = save_arg lexbuf in
        begin match Get.get_length arg with
        | Length.No s ->
            warning ("``tabular*'' with length argument: "^
                     do_subst_this arg) ;
            Length.Default
        | width -> width
        end
    | _ -> Length.Default in
      
  skip_opt lexbuf ;
  let format = save_arg lexbuf in
  let format = Tabular.main format in
  cur_format := format ;
  push stack_in_math !in_math ;
  in_table := Table
       {math = (env = "array")  ;
         border = !Tabular.border} ;
  if !display then Dest.item_display () ;
  in_math := false ;
  push stack_display !display ;
  display := false ;
  if !Tabular.border then
    Dest.open_table true ("CELLSPACING=0 CELLPADDING=1"^check_width len)
  else
    Dest.open_table false ("CELLSPACING=2 CELLPADDING=0"^check_width len);
  open_row() ;
  open_first_col main ;
  skip_blanks_pop lexbuf
;;

def_code "\\array" (open_array "array") ;
def_code "\\tabular" (open_array "tabular") ;
def_code "\\tabular*" (open_array "tabular*")
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

let close_array env _ =
  do_unskip () ;
  close_last_col main "" ;
  close_last_row () ;
  Dest.close_table () ;
  restore_array_state () ;
  in_math := pop stack_in_math ;
  display := pop stack_display;
  if !display then Dest.item_display () ;
;;

def_code "\\endarray" (close_array "array") ;
def_code "\\endtabular" (close_array "tabular") ;
def_code "\\endtabular*" (close_array "tabular*")
;;


def_code "\\\\"
 (fun lexbuf -> 
   do_unskip () ;
   skip_opt lexbuf ;
   if is_table !in_table  then begin
     close_col main "&nbsp;" ; close_row () ;
     open_row () ; open_first_col main
   end else if is_tabbing !in_table then begin
     Dest.close_cell "";
     Dest.close_row () ;
     Dest.new_row () ;
     Dest.open_cell default_format 1 0
   end else begin
     if !display then
       warning "\\\\ in display mode, ignored"
     else
       Dest.skip_line ()
   end ;
   skip_blanks_pop lexbuf ;
   let _ = Dest.forget_par () in ())
;;


(* Other scanners *)

def_code "\\latexonly"
  (fun lexbuf ->
    let lexbuf = previous_lexbuf () in
    start_other_scan "latexonly" latexonly lexbuf)
;;

def_code "\\toimage"
  (fun lexbuf ->
    let lexbuf = previous_lexbuf () in
    start_image_scan "" image lexbuf)
;;

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


(* Info  format specific *)

def_code "\\@infomenu"
  (fun lexbuf ->
    let arg = get_this_arg main (save_arg lexbuf) in
    Dest.infomenu arg)
;;

def_code  "\\@infonode"
  (fun lexbuf ->
    let opt = subst_opt "" lexbuf in
    let num = get_this_arg main (save_arg lexbuf) in
    let nom = get_this_arg main (save_arg lexbuf) in
    Dest.infonode opt num nom)
;;

def_code "\\@infoextranode"
  (fun lexbuf ->
   let num = get_this_arg main (save_arg lexbuf) in
   let nom = get_this_arg main (save_arg lexbuf) in
   let text = get_this_arg main (save_arg lexbuf) in
   Dest.infoextranode num nom text)
;;

def_code "\\@infoname"
  (fun lexbuf ->
    let arg = get_prim_arg lexbuf in
    Dest.loc_name arg)
;;

let safe_len = function
  | Length.No _ -> Length.Default
  | l    -> l
;;

def_code "\\@printHR"
    (fun lexbuf ->
      let arg = get_prim_arg lexbuf in
      let taille = safe_len (Get.get_length (save_arg lexbuf)) in
      Dest.horizontal_line arg taille (Length.Pixel 2))
;;

def_code"\\@hr"
   (fun lexbuf ->
     let attr = subst_opt "" lexbuf in
     let width = safe_len (Get.get_length (save_arg lexbuf)) in
     let height = safe_len (Get.get_length (save_arg lexbuf)) in
     Dest.horizontal_line attr width height)
;;

(* Accents *)
let aigu = function
  "a" -> "á" | "e" -> "é" | "i" | "\\i" | "\\i " -> "í"
| "o" -> "ó" | "u" -> "ú"
| "A" -> "Á" | "E" -> "É" | "I" | "\\I" | "\\I " -> "Í"
| "O" -> "Ó" | "U" -> "Ú"
| "y" -> "ý" | "Y" -> "Ý"
| "" | " " -> "'"
| s   -> s

and grave = function
  "a" -> "à" | "e" -> "è"  | "i" -> "ì"
| "o" -> "ò" | "u" -> "ù"  | "\\i" | "\\i " -> "í"
| "A" -> "À" | "E" -> "È"  | "I" -> "Ì"
| "O" -> "Ò" | "U" -> "Ù"  | "\\I" | "\\I " -> "Ì"
| "" | " " -> "`"
| s -> s
and circonflexe = function
  "a" -> "â" | "e" -> "ê"  | "i" -> "î"
| "o" -> "ô" | "u" -> "û"  | "\\i" | "\\i " -> "î"
| "A" -> "Â" | "E" -> "Ê"  | "I" -> "Î"
| "O" -> "Ô" | "U" -> "Û"  | "\\I" | "\\I " -> "Î"
| "" | " " -> "\\@print{^}"
| s -> s

and trema = function
  "a" -> "ä" | "e" -> "ë"  | "i" -> "ï"
| "o" -> "ö" | "u" -> "ü"  | "\\i" | "\\i " -> "ï"
| "A" -> "Ä" | "E" -> "Ë"  | "I" -> "Ï"
| "O" -> "Ö" | "U" -> "Ü"  | "\\I" | "\\I " -> "Ï"
| "" | " " -> "¨"
| s -> s

and cedille = function
  "c" -> "ç"
| "C" -> "Ç"
| s   -> s

and tilde = function
  "a" -> "ã" | "A" -> "Ã"
| "o" -> "õ" | "O" -> "Õ"
| "n" -> "ñ" | "N" -> "Ñ"
| "" | " " -> "\\@print{~}"
| s   -> s
;;


      
def_fun "\\'"  aigu ;
def_fun "\\`"  grave ;
def_fun "\\^"  circonflexe ;
def_fun "\\\"" trema ;
def_fun "\\c"  cedille ;
def_fun "\\~"  tilde
;;

Get.init
      (fun nostyle s ->
        if nostyle then
          get_this_nostyle_arg main s
	else
          get_this_arg main s)
      macro_register new_env close_env
;;

def_code "\\@primitives"
  (fun lexbuf ->
    let pkg = get_prim_arg lexbuf in
    exec_init pkg)
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



end}

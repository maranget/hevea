let header =  "$Id: lexstate.ml,v 1.7 1999-04-15 15:05:48 maranget Exp $"

open Misc
open Lexing

exception Error of string
exception IfFalse


type 'a t = 'a list ref

let create () = ref []

and push s e = s := e :: !s

and pop s = match !s with
  [] -> raise (Misc.Fatal "Empty stack")
| e::rs -> s := rs ; e

and top s = match !s with
  [] -> raise (Misc.Fatal "Empty stack")
| e::_ -> e

and empty s = match !s with | [] -> true | _ -> false

and rev s = s := List.rev !s

type 'a r = 'a list

let save_stack s = !s
and restore_stack s old = s := old

(* stack for recoding lexbuf *)
let stack_lexbuf = ref []
;;

let pretty_lexbuf lb =
  let  pos = lb.lex_curr_pos and len = String.length lb.lex_buffer in
  prerr_endline "Buff contents:" ;
  prerr_endline ("<<"^String.sub lb.lex_buffer pos (len-pos)^">>");
  prerr_endline ("curr_pos="^string_of_int lb.lex_curr_pos);
  prerr_endline "End of buff"
;;

(* arguments inside macros*)
type env = string array ref
type closenv = string array t

let stack = ref [||]
and stack_stack = ref []
and stack_stack_stack = ref []
;;

let top_level () = empty stack_stack


let prerr_args_aux args =
  if Array.length args <> 0 then begin
    prerr_endline "Arguments: " ;
    for i = 0 to Array.length args - 1 do
      prerr_string "\t``" ;
      prerr_string args.(i) ;
      prerr_endline "''"
    done
  end

let prerr_args () = prerr_args_aux !stack


let prerr_stack_string s f stack =
  let rec do_rec = function
    [] -> prerr_endline ">>"
  | [s] -> prerr_string ("``"^f s^"''") ; prerr_endline ">>"
  | s::r ->
      prerr_string "``" ;
      prerr_string (f s) ;
      prerr_string "'' " ;
      do_rec r in
        
  prerr_string s ;
  prerr_string ": <<" ;
  do_rec !stack


let scan_arg lexfun i =
  if i >= Array.length !stack then begin
    if !verbose > 1 then begin
      prerr_string ("Subst arg #"^string_of_int (i+1)^" -> not found") ;
      prerr_args_aux !stack;
      prerr_endline (" ("^string_of_int (List.length !stack_stack)^")")
    end ;
    raise (Error "Macro argument not found")
  end;
  let arg = !stack.(i) in
  if !verbose > 1 then begin
    prerr_string ("Subst arg #"^string_of_int (i+1)^" -> ``"^arg^"''") ;
    prerr_endline (" ("^string_of_int (List.length !stack_stack)^")")
  end ;
  let old_args = !stack in
  stack := pop stack_stack ;
  if !verbose > 1 then
    prerr_args_aux !stack;
  let r = lexfun arg in
  push stack_stack !stack ;
  stack := old_args ;
  r

and scan_fun f lexbuf name = f lexbuf name

and scan_body exec body args =
  begin match body with
  | Latexmacros.CamlCode _ -> ()
  | _ ->
      push stack_stack !stack ;
      stack := args
  end ;
  try
(*
    prerr_string "scan_body :" ;
    Latexmacros.pretty_action body ;
    prerr_args () ;
*)
    let r = exec body in
    begin match body with
    | Latexmacros.CamlCode _ -> ()
    | _ -> stack := pop stack_stack
    end;
    r
  with IfFalse -> begin
    stack := pop stack_stack ;
    raise IfFalse
  end

let eat_space = ref true
and stack_eat = ref []

let tab_val = ref 8


(* Recoding and restoring lexbufs *)
let record_lexbuf lexbuf eat =
  push stack_eat !eat_space ;
  push stack_stack_stack (!stack,!stack_stack) ;
  push stack_lexbuf lexbuf ;
  eat_space := eat

and previous_lexbuf () =
  let lexbuf = pop stack_lexbuf
  and s,ss = pop stack_stack_stack in
  eat_space := pop stack_eat ;
  stack := s ; stack_stack := ss ;
  lexbuf
;;

(* Saving and restoring lexing status *)
let stack_stack_lexbuf = ref []
;;

let save_lexstate () =
  let old_eat = !stack_eat
  and old_stack = !stack_stack_stack in
  push stack_eat !eat_space ;
  push stack_stack_stack (!stack,!stack_stack) ;
  push stack_stack_lexbuf (!stack_lexbuf,!stack_eat,!stack_stack_stack) ;
  stack_eat := old_eat ; stack_stack_stack := old_stack

and restore_lexstate () =
  let l,s,args = pop stack_stack_lexbuf in
  stack_lexbuf := l ;
  stack_eat := s;
  eat_space := pop stack_eat ;
  stack_stack_stack := args ;
  let s,ss = pop stack_stack_stack in
  stack := s ;
  stack_stack := ss
  
;;

(* Blank lexing status *)
let start_lexstate () =
  save_lexstate () ;
  stack_lexbuf := [] ;
  eat_space := false
;;

let out_file = match Parse_opts.name_out with
| "" -> Out.create_chan stdout
| x  -> Out.create_chan (open_out x)
;;

let prelude = ref true
;;

let flushing = ref false
;;

let stack_in_math = ref []
and stack_display = ref []


let start_normal display in_math =
  start_lexstate () ;
  push stack_display !display ;
  push stack_in_math !in_math ;
  display := false ;
  in_math := false

and  end_normal display in_math =
  in_math := pop stack_in_math ;
  display := pop stack_display ;
  restore_lexstate () ;
;;

let save_arg lexbuf =

  let rec save_rec lexbuf =
    try Save.arg lexbuf
    with Save.Eof -> begin
        if empty stack_lexbuf then
          raise (Error "Eof while looking for argument");
        let lexbuf = previous_lexbuf () in
        if !verbose > 2 then begin
          prerr_endline "popping stack_lexbuf in save_arg";
          pretty_lexbuf lexbuf ;
          prerr_args ()
        end;
        save_rec lexbuf end in

  Save.seen_par := false ;
  save_lexstate () ;
  let arg = save_rec lexbuf in
  restore_lexstate () ;
  if !verbose > 2 then
    prerr_endline ("Arg parsed: ``"^arg^"''") ;
  arg
;;

type ok = No of string | Yes of string
;;

let from_ok = function
  Yes s -> (Latexmacros.optarg := true ; s)
| No s  -> (Latexmacros.optarg := false ; s)
;;

let pretty_ok = function
  Yes s -> "+"^s^"+"
| No s  -> "-"^s^"-"
;;


let parse_quote_arg_opt def lexbuf =

  let rec save_rec lexbuf = 
    try Yes (Save.opt lexbuf) with
      Save.NoOpt -> No def
    | Save.Eof -> begin
        if empty stack_lexbuf  then No def
        else let lexbuf = previous_lexbuf () in
        if !verbose > 2 then begin
          prerr_endline "poping stack_lexbuf in parse_quote_arg_opt";
          pretty_lexbuf lexbuf
        end;
        save_rec lexbuf end in
  
  save_lexstate () ;
  let r = save_rec lexbuf in
  restore_lexstate () ;
  if !verbose > 2 then begin
     Printf.fprintf stderr "Parse opt : %s" (pretty_ok r) ;
     prerr_endline ""
  end ;
  Save.seen_par := false ;
  r
;;

let rec parse_args_norm pat lexbuf = match pat with
  [] -> []
| s :: pat ->
    let arg = save_arg lexbuf in
    let r = parse_args_norm pat lexbuf in
     arg :: r
;;


let parse_arg_opt def lexbuf =
  let arg = parse_quote_arg_opt def lexbuf in
(*
   (match arg with Yes s -> Yes (subst_arg s)
           | No s -> No (subst_arg s))
*)
  arg
;;

let rec parse_args_opt pat lexbuf = match pat with
  [] -> []
| def::rest ->
   let arg = parse_arg_opt def lexbuf in
   let r   = parse_args_opt rest lexbuf in
   arg :: r
;;


let skip_opt lexbuf =
  let _ =  parse_quote_arg_opt "" lexbuf  in
  ()

and check_opt lexbuf =
  match parse_quote_arg_opt "" lexbuf  with
    Yes _ -> true
  | No  _ -> false

and save_opt def lexbuf =
  match parse_arg_opt def  lexbuf with
    Yes s -> s
  | No s  -> s
;;


let parse_args (popt,pat) lexbuf =
  let opts =  parse_args_opt popt lexbuf in
  let args =  parse_args_norm pat lexbuf in
  (opts,args)
;;

let make_stack name pat lexbuf =
  let (opts,args) = parse_args pat lexbuf in
  let args = Array.of_list (List.map from_ok opts@args) in
  if !verbose > 2 then begin
    Printf.fprintf stderr "make_stack for macro: %s "  name ;
    Latexmacros.pretty_pat pat ;
    prerr_endline "";
    for i = 0 to Array.length args-1 do
      Printf.fprintf stderr "\t#%d = %s\n" (i+1) args.(i)
    done
  end ;
  args
;;

let scan_this lexfun s =
  start_lexstate ();
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this : [%s]" s ;
    prerr_endline ""  
  end ;
  let lexer = Lexing.from_string s in
  let r = lexfun lexer in
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this : over" ;
    prerr_endline ""
  end ;
  restore_lexstate ();
  r
;;

let scan_this_may_cont eat lexfun lexbuf s =
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_may_cont : [%s]" s ;
    prerr_endline "" ;
    if !verbose > 2 then begin
      prerr_endline "Pushing lexbuf" ;
      pretty_lexbuf lexbuf
    end
  end ;
  save_lexstate ();
  record_lexbuf lexbuf eat;

  let lexer = Lexing.from_string s in
  let r = lexfun lexer in

  restore_lexstate ();
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_may_cont : over" ;
    prerr_endline ""
  end ;
  r

let input_file loc_verb main filename =
  try
    let filename,input = Myfiles.open_tex filename in
    if !verbose > 0 then
      prerr_endline ("Input file: "^filename) ;
    let buf = Lexing.from_channel input in
    Location.set filename buf ;
    let old_verb = !verbose in
    verbose := loc_verb ;
    main buf ;
    close_in input ;
    verbose := old_verb ;
    Location.restore ()  
  with Myfiles.Except -> begin
    if !verbose > 0 then
      prerr_endline ("Not opening file: "^filename) ;
    raise  Myfiles.Except
  end
 | Myfiles.Error m as x -> begin
     Parse_opts.warning m ;
     raise x
 end



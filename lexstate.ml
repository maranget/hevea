let header =  "$Id: lexstate.ml,v 1.27 1999-09-07 18:47:58 maranget Exp $"

open Misc
open Lexing
open Stack

(* Commands nature *)
type action =
  | Subst of string
  | CamlCode of (Lexing.lexbuf -> unit)


let pretty_action acs =
   match acs with
   | Subst s    -> Printf.fprintf stderr "{%s}\n" s
   | CamlCode _ -> prerr_endline "*code*"

type pat = string list * string list


let pretty_pat (_,args) =
  List.iter (fun s -> prerr_string s ; prerr_char ',') args


exception Error of string

(* Status flags *)
let display = ref false
and in_math = ref false
and french =
  ref
    (match !Parse_opts.language with
    | Parse_opts.Francais -> true | _ -> false)
and optarg = ref false
and styleloaded = ref false
and activebrace = ref true
and html = 
  ref
    (match !Parse_opts.destination with
    | Parse_opts.Html -> true
    | Parse_opts.Info | Parse_opts.Text -> false)
and text = 
  ref
    (match !Parse_opts.destination with
    | Parse_opts.Html -> false
    | Parse_opts.Info | Parse_opts.Text -> true)
(* Additional variables for videoc *)
and withinLispComment = ref false
and afterLispCommentNewlines = ref 0
;;

(*
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
*)
(* stack for recoding lexbuf *)
let stack_lexbuf = Stack.create "stack_lexbuf"
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
and stack_stack = Stack.create "stack_stack"
;;

let stack_stack_stack =
  Stack.create "stack_stack_stack"
;;


(* catcodes *)

let plain_of_char = function
  | '{' -> 0
  | '}' -> 1
  | '$' -> 2
  | '&' -> 3
  | '#' -> 4
  | '^' -> 5
  | '_' -> 6
  | '~' -> 7
  | '\\' -> 8
  | '%'  -> 9
  | _   -> raise (Fatal "Catcode table is bad")

and plain = Array.create 10 true

let is_plain c = plain.(plain_of_char c)
and set_plain c = plain.(plain_of_char c) <- true
and unset_plain c = plain.(plain_of_char c) <- false

let  alltt = ref false
and stack_alltt = Stack.create "stack_alltt"
and stack_closed = Stack.create "stack_closed"
;;

let top_level () = Stack.empty stack_stack


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




let scan_arg lexfun i =
  if i >= Array.length !stack then begin
    if !verbose > 1 then begin
      prerr_string ("Subst arg #"^string_of_int (i+1)^" -> not found") ;
      prerr_args_aux !stack;
      prerr_endline (" ("^string_of_int (Stack.length stack_stack)^")")
    end ;
    raise (Error "Macro argument not found")
  end;
  let arg = !stack.(i) in
  if !verbose > 1 then begin
    prerr_string ("Subst arg #"^string_of_int (i+1)^" -> ``"^arg^"''") ;
    prerr_endline (" ("^string_of_int (Stack.length stack_stack)^")")
  end ;
  let old_args = !stack
  and old_alltt = !alltt in
  stack := Stack.pop  stack_stack ;
  alltt := pop  stack_alltt ;
  if !verbose > 2 then
    prerr_args_aux !stack;
  let r = lexfun arg in
  Stack.push stack_stack !stack ;
  push stack_alltt !alltt ;
  stack := old_args ;
  alltt := old_alltt ;
  r

and scan_body exec body args =
  begin match body with
  | CamlCode _ -> ()
  | _ ->
      Stack.push stack_stack !stack ;
      stack := args
  end ;
(*
    prerr_string "scan_body :" ;
    pretty_action body ;
    prerr_args () ;
*)
  push stack_alltt !alltt ;
  let r =
    if !alltt then begin
      alltt := false ;
      let r = exec body in
      alltt := not !alltt ;
      r
    end else
      exec body in
  let _ = pop stack_alltt in
  begin match body with
  | CamlCode _ -> ()
  | _ -> stack := Stack.pop stack_stack
  end;
  r
    
let tab_val = ref 8


(* Recoding and restoring lexbufs *)
let record_lexbuf lexbuf =
  Stack.push stack_stack_stack (!stack, Stack.save stack_stack) ;
  Stack.push stack_lexbuf lexbuf

and previous_lexbuf () =
  let lexbuf = Stack.pop stack_lexbuf
  and s,ss = Stack.pop stack_stack_stack in
  stack := s ; Stack.restore stack_stack ss ;
  lexbuf
;;

(* Saving and restoring lexing status *)
let stack_stack_lexbuf = Stack.create "stack_stack_lexbuf"
;;

let save_lexstate () =
  let old_stack = Stack.save stack_stack_stack in
  Stack.push stack_stack_stack (!stack,Stack.save stack_stack) ;
  push stack_stack_lexbuf
    (Stack.save stack_lexbuf, Stack.save stack_stack_stack) ;
  Stack.restore stack_stack_stack old_stack

and restore_lexstate () =
  let l,args = pop stack_stack_lexbuf in
  Stack.restore stack_lexbuf  l ;
  Stack.restore stack_stack_stack args ;
  let s,ss = Stack.pop stack_stack_stack in
  stack := s ;
  Stack.restore stack_stack ss
  
;;

(* Blank lexing status *)
let start_lexstate () =
  save_lexstate () ;
  Stack.restore stack_lexbuf (Stack.empty_saved)
;;

let out_file = match Parse_opts.name_out,!Parse_opts.destination with
| "", Parse_opts.Info ->  Out.create_buff ()
| "", _ -> Out.create_chan stdout
| x , Parse_opts.Info -> Out.create_chan (open_out (x^".tmp"))
| x , _  -> Out.create_chan (open_out x)
;;

let prelude = ref true
;;

let flushing = ref false
;;

let stack_in_math = Stack.create "stack_in_math"
and stack_display = Stack.create "stack_display"


let start_normal display in_math =
  start_lexstate () ;
  push stack_display !display ;
  push stack_in_math !in_math ;
  push stack_alltt !alltt ;
  display := false ;
  in_math := false ;
  alltt := false

and  end_normal display in_math =
  alltt   := pop stack_alltt ;
  in_math := pop stack_in_math ;
  display := pop stack_display ;
  restore_lexstate () ;
;;

let full_save_arg lexfun lexbuf =
  let rec save_rec lexbuf =
    try lexfun lexbuf
    with Save.Eof -> begin
        if Stack.empty stack_lexbuf then
          raise (Error "Eof while looking for argument");
        let lexbuf = previous_lexbuf () in
        if !verbose > 2 then begin
          prerr_endline "popping stack_lexbuf in full_save_arg";
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

let save_arg lexbuf = full_save_arg Save.arg lexbuf
and save_arg_with_delim delim lexbuf =
  full_save_arg (Save.with_delim delim) lexbuf
and save_filename lexbuf = full_save_arg Save.filename lexbuf
;;


type ok = No of string | Yes of string
;;

let from_ok = function
  Yes s -> (optarg := true ; s)
| No s  -> (optarg := false ; s)
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
        if Stack.empty stack_lexbuf  then No def
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

let norm_arg s =
  String.length s = 2 && s.[0] = '#' &&
  ('0' <= s.[1] && s.[1] <= '9')

let rec parse_args_norm pat lexbuf = match pat with
|   [] -> []
| s :: (ss :: _ as pat) when norm_arg s && norm_arg ss ->
    let arg = save_arg lexbuf in
    let r = parse_args_norm pat lexbuf in
     arg :: r
| s :: ss :: pat when norm_arg s && not (norm_arg ss) ->
    let arg = save_arg_with_delim ss lexbuf in
    arg :: parse_args_norm pat lexbuf
| s :: pat when not (norm_arg s) ->
    Save.skip_delim s lexbuf ;
    parse_args_norm pat lexbuf
| s :: pat ->
    let arg = save_arg lexbuf in
    let r = parse_args_norm pat lexbuf in
    arg :: r
;;


let parse_arg_opt def lexbuf =
  let arg = parse_quote_arg_opt def lexbuf in
  arg
;;

let rec parse_args_opt pat lexbuf = match pat with
  [] -> []
| def::rest ->
   let arg = parse_arg_opt def lexbuf in
   let r   = parse_args_opt rest lexbuf in
   arg :: r
;;

let skip_csname lexbuf =
  let _ = Save.csname lexbuf (fun x -> x) in ()


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
  begin match pat with
  | s :: ss :: _ when norm_arg s && not (norm_arg ss) ->
      Save.skip_blanks_init lexbuf
  | _ -> ()
  end ;
  let args =  parse_args_norm pat lexbuf in
  (opts,args)
;;

let make_stack name pat lexbuf =
  try
    let (opts,args) = parse_args pat lexbuf in
    let args = Array.of_list (List.map from_ok opts@args) in
    if !verbose > 1 then begin
      Printf.fprintf stderr "make_stack for macro: %s "  name ;
      pretty_pat pat ;
      prerr_endline "";
      for i = 0 to Array.length args-1 do
        Printf.fprintf stderr "\t#%d = %s\n" (i+1) args.(i)
      done
    end ;
    args
  with Save.Delim delim ->
    raise
      (Error
         ("Use of "^name^
          " does not match its definition (delimiter: "^delim^")"))
    
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

let scan_this_may_cont lexfun lexbuf s =
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_may_cont : [%s]" s ;
    prerr_endline "" ;
    if !verbose > 2 then begin
      prerr_endline "Pushing lexbuf" ;
      pretty_lexbuf lexbuf
    end
  end ;
  save_lexstate ();
  record_lexbuf lexbuf ;

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
    if !verbose > 1 then prerr_endline ("scanning: "^filename) ;
    begin try main buf with Misc.EndInput -> () end ;
    if !verbose > 1 then prerr_endline ("scanning over: "^filename) ;    
    close_in input ;
    verbose := old_verb ;
    Location.restore ()  
  with Myfiles.Except -> begin
    if !verbose > 0 then
      prerr_endline ("Not opening file: "^filename) ;
    raise  Myfiles.Except
  end
 | Myfiles.Error m as x -> begin
     Misc.warning m ;
     raise x
 end



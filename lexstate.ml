let header =  "$Id: lexstate.ml,v 1.32 1999-10-06 17:18:56 maranget Exp $"

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

(* Environments *)
type subst = Top | Env of (string * subst) array

let subst = ref Top
let get_subst () = !subst
let stack_subst =  Stack.create "stack_subst"

let pretty_subst = function
  | Top -> prerr_endline "Top level"
  | Env args ->      
      if Array.length args <> 0 then begin
        prerr_endline "Env: " ;
        for i = 0 to Array.length args - 1 do
          prerr_string "\t``" ;
          prerr_string (fst args.(i)) ;
          prerr_endline "''"
        done
      end

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
  let size = if !verbose > 3 then len-pos else min (len-pos) 80 in
  if size <> len-pos then begin
    prerr_string "<<" ;
    prerr_string (String.sub lb.lex_buffer pos (size/2)) ;
    prerr_string "... (omitted) ..." ;
    prerr_string (String.sub lb.lex_buffer (len-size/2-1) (size/2)) ;
    prerr_endline ">>"
  end else
    prerr_endline ("<<"^String.sub lb.lex_buffer pos size^">>");
  prerr_endline ("curr_pos="^string_of_int lb.lex_curr_pos);
  prerr_endline "End of buff"
;;

(* arguments inside macros*)
type env = string array ref
type closenv = string array t




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
  | c   ->
      raise
        (Fatal ("Internal catcode table error: '"^String.make 1 c^"'"))

and plain = Array.create 10 true

let is_plain c = plain.(plain_of_char c)
and set_plain c = plain.(plain_of_char c) <- true
and unset_plain c = plain.(plain_of_char c) <- false
and plain_back b c = plain.(plain_of_char c) <- b

let  alltt = ref false
and stack_alltt = Stack.create "stack_alltt"
and stack_closed = Stack.create "stack_closed"
;;

let top_level () = match !subst with Top -> true | _ -> false



let prerr_args () = pretty_subst !subst


let scan_arg lexfun i =
  let args = match !subst with
  | Top -> [||]
  | Env args -> args in
  if i >= Array.length args then begin
    if !verbose > 1 then begin
      prerr_string ("Subst arg #"^string_of_int (i+1)^" -> not found") ;
      pretty_subst !subst
    end ;
    raise (Error "Macro argument not found")
  end;
  let arg,arg_subst = args.(i) in
  if !verbose > 1 then begin
    prerr_string ("Subst arg #"^string_of_int (i+1)^" -> ``"^arg^"''")
  end ;
  let old_subst = !subst
  and old_alltt = !alltt in
  subst := arg_subst ;
  alltt := pop  stack_alltt ;
  if !verbose > 2 then
    pretty_subst !subst ;
  let r = lexfun arg in
  push stack_alltt !alltt ;
  subst := old_subst ;
  alltt := old_alltt ;
  r

and scan_body exec body args =

  let old_subst = match body with
  | CamlCode _ -> !subst
  | _          ->
      let old_subst = !subst in
      subst := args ;
      old_subst in

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
  | _ -> subst := old_subst
  end;
  r

let tab_val = ref 8


(* Recoding and restoring lexbufs *)
let record_lexbuf lexbuf subst =
  Stack.push stack_subst subst ;
  Stack.push stack_lexbuf lexbuf

and previous_lexbuf () =
  let lexbuf = Stack.pop stack_lexbuf in
  subst := Stack.pop stack_subst ;
  lexbuf
;;

(* Saving and restoring lexing status *)
let stack_lexstate = Stack.create "stack_lexstate"
;;

let save_lexstate () =
  let old_stack = Stack.save stack_subst in
  Stack.push stack_subst !subst ;
  push stack_lexstate (Stack.save stack_lexbuf, Stack.save stack_subst) ;
  Stack.restore stack_subst old_stack

and restore_lexstate () =
  let lexbufs,substs = pop stack_lexstate in
  Stack.restore stack_lexbuf lexbufs ;
  Stack.restore stack_subst substs ;
  subst := Stack.pop stack_subst
;;

(* Blank lexing status *)
let start_lexstate () =
  save_lexstate () ;
  Stack.restore stack_lexbuf (Stack.empty_saved) ;
  Stack.restore stack_subst (Stack.empty_saved)
;;

let prelude = ref true
;;

let flushing = ref false
;;

let stack_in_math = Stack.create "stack_in_math"
and stack_display = Stack.create "stack_display"


let start_normal this_subst =
  start_lexstate () ;
  push stack_display !display ;
  push stack_in_math !in_math ;
  push stack_alltt !alltt ;
  display := false ;
  in_math := false ;
  alltt := false ;
  subst := this_subst

and end_normal () =
  alltt   := pop stack_alltt ;
  in_math := pop stack_in_math ;
  display := pop stack_display ;
  restore_lexstate () ;
;;

let full_save_arg eoferror parg lexfun lexbuf =
  let rec save_rec lexbuf =
    try
      let arg = lexfun lexbuf in
      arg,(!subst)
    with Save.Eof -> begin
        if Stack.empty stack_lexbuf then
           eoferror () 
        else begin
          let lexbuf = previous_lexbuf () in
          if !verbose > 2 then begin
            prerr_endline "popping stack_lexbuf in full_save_arg";
            pretty_lexbuf lexbuf ;
            prerr_args ()
          end;
          save_rec lexbuf
        end
    end in

  Save.seen_par := false ;
  save_lexstate () ;
  let arg,subst_arg = save_rec lexbuf in
  restore_lexstate () ;
  if !verbose > 2 then
    prerr_endline ("Arg parsed: ``"^parg arg^"''") ;
  arg,subst_arg
;;


type ok = No of string | Yes of string
;;

let pstring s = s
and pok = function
  | Yes s -> s
  | No s  -> s


let eof_arg () = raise (Error "Eof while looking for argument")

let save_arg lexbuf = full_save_arg eof_arg pstring Save.arg lexbuf
and save_arg_with_delim delim lexbuf =
  full_save_arg eof_arg pstring (Save.with_delim delim) lexbuf
and save_filename lexbuf = full_save_arg eof_arg pstring Save.filename lexbuf
and save_verbatim lexbuf =
  full_save_arg eof_arg pstring Save.arg_verbatim lexbuf
let eof_opt def () = No def,Top

let save_arg_opt def lexbuf =
  match
    full_save_arg
      (eof_opt def)
      pok
      (fun lexbuf ->
        try Yes (Save.opt lexbuf) with          
        | Save.NoOpt -> No def)
      lexbuf with
  | No def,_ -> No def,Top
  | arg      -> arg    
    
      
  
;;


let from_ok = function
  Yes s,env -> (optarg := true ; s,env)
| No s,env  -> (optarg := false ; s,env)
;;

let pretty_ok = function
  Yes s -> "+"^s^"+"
| No s  -> "-"^s^"-"
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


let skip_csname lexbuf =
  let _ = Save.csname lexbuf (fun x -> x) in ()


let skip_opt lexbuf =
  let _ =  save_arg_opt "" lexbuf  in
  ()

and save_opt def lexbuf = from_ok (save_arg_opt def  lexbuf)
;;

let rec save_opts pat lexbuf = match pat with
  [] -> []
| def::rest ->
   let arg = save_arg_opt def lexbuf in
   let r = save_opts rest lexbuf in
   arg :: r
;;


let parse_args (popt,pat) lexbuf =
  let opts =  save_opts popt lexbuf in
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
        Printf.fprintf stderr "\t#%d = %s\n" (i+1) (fst args.(i)) ;
        pretty_subst (snd args.(i))
      done
    end ;
    Env args
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
and scan_this_arg lexfun (s,this_subst) =
  start_lexstate () ;
  subst := this_subst ;
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_arg : [%s]" s ;
    prerr_endline ""  
  end ;
  let lexer = Lexing.from_string s in
  let r = lexfun lexer in
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_arg : over" ;
    prerr_endline ""
  end ;
  restore_lexstate ();
  r
;;

let scan_this_may_cont lexfun lexbuf cur_subst (s,env) =
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_may_cont : [%s]" s ;
    prerr_endline "" ;
    if !verbose > 1 then begin
      prerr_endline "Pushing lexbuf and env" ;
      pretty_lexbuf lexbuf ;
      pretty_subst !subst
    end
  end ;
  save_lexstate ();
  record_lexbuf lexbuf cur_subst ;
  subst := env ;

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
     Misc.warning m
 end



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

open Printf
open Misc
open Lexing
open MyStack



(* Commands nature *)
type action =
  | Subst of string list
  | Toks of string list
  | CamlCode of (Lexing.lexbuf -> unit)

let body_to_string = String.concat ""

let pretty_body chan = List.iter(fprintf chan "%s")

let pretty_action acs =
   match acs with
   | Subst s    ->
       eprintf "{%a}" (fun chan -> List.iter (fprintf chan "\"%s\" ")) s
   | Toks l ->
       List.iter
         (fun s -> Printf.fprintf stderr "{%s}, " s)
         l
   | CamlCode _ -> prerr_string "*code*"

let rec is_empty_list = function
  | [] -> true
  | x::xs -> String.length x = 0 && is_empty_list xs

type pat = string list * string list


let pretty_pat (_,args) =
  List.iter (fun s -> prerr_string s ; prerr_char ',') args

let is_subst body = match body with
| CamlCode _ -> false
| _ -> true

let latex_pat opts n =
  let n_opts = List.length opts in
  let rec do_rec r i =
    if i <=  n_opts  then r
    else do_rec (("#"^string_of_int i)::r) (i-1) in
  opts,do_rec [] n

let zero_pat = latex_pat [] 0
and one_pat  = latex_pat [] 1

(* Environments *)
type subst = Top | Env of string list arg array
and 'a arg = {arg : 'a ; subst : subst }

let mkarg arg subst = {arg=arg ; subst=subst }



type alltt = Not | Inside | Macro

let effective = function
  | Inside -> true
  | _      -> false

let subst = ref Top
and alltt = ref Not

let stack_subst = MyStack.create "stack_subst"
and stack_alltt = MyStack.create_init "stack_alltt" Not

let get_subst () = !subst
let top_subst = Top



let pretty_subst = function
  | Top -> prerr_endline "Top level"
  | Env args ->      
      if Array.length args <> 0 then begin
        prerr_endline "Env: " ;
        for i = 0 to Array.length args - 1 do
          prerr_string "\t'" ;
          eprintf "%a" pretty_body args.(i).arg  ;
          prerr_endline "'"
        done
      end

let rec pretty_subst_rec indent = function
  | Top -> prerr_string indent ; prerr_endline "Top level"
  | Env args ->      
      if Array.length args <> 0 then begin
        prerr_string indent ;
        prerr_endline "Env: " ;
        for i = 0 to Array.length args - 1 do
          prerr_string indent ;
          prerr_string ("  #"^string_of_int (i+1)^" ``");
          pretty_body stderr args.(i).arg ;
          prerr_endline "''" ;
          pretty_subst_rec ("  "^indent) args.(i).subst
        done
      end

let full_pretty_subst s = pretty_subst_rec "  " s

exception Error of string
exception SubstTop
type jax = JaxOut | JaxInline | JaxDisplay
  
(* Status flags *)
let display = ref false
and raw_chars = ref false
and in_math = ref false
and jaxauto = ref false
and injaxauto = ref JaxOut
and whitepre = ref false
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
and alltt_loaded = ref false

(* Additional variables for videoc *)
and withinLispComment = ref false
and afterLispCommentNewlines = ref 0
(* Additional flags for transformations *)
;;
type case = Upper | Lower | Neutral
let case = ref Neutral
;;


let string_to_arg arg = {arg=arg ; subst= !subst }

(* Stacks for flags *)
let stack_in_math = MyStack.create "stack_in_math"
and stack_display = MyStack.create "stack_display"

(* Stacks for entry stream  *)
let stack_lexbuf = MyStack.create "stack_lexbuf"
;;

let pretty_lexbuf lb =
  let  pos = lb.lex_curr_pos and len = Bytes.length lb.lex_buffer in
  prerr_endline "Buff contents:" ;
  let size = if !verbose > 3 then len-pos else min (len-pos) 80 in
  if size <> len-pos then begin
    prerr_string "<<" ;
    prerr_string (Bytes.sub_string lb.lex_buffer pos (size/2)) ;
    prerr_string "... (omitted) ..." ;
    prerr_string (Bytes.sub_string lb.lex_buffer (len-size/2-1) (size/2)) ;
    prerr_endline ">>"
  end else
    prerr_endline ("<<"^Bytes.sub_string lb.lex_buffer pos size^">>");
  prerr_endline ("curr_pos="^string_of_int lb.lex_curr_pos);
  prerr_endline "End of buff"
;;

  
(* arguments inside macros*)
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
  | '\'' -> 10
  | '`' -> 11
  | '-' -> 12
  | '"' -> 13 (* '"' *)
  | c   ->
      raise
        (Fatal ("Internal catcode table error: '"^String.make 1 c^"'"))

and plain = Array.make 14 true

let is_plain c = plain.(plain_of_char c)
and set_plain c =
(*  if c = '_' then eprintf "set_plain %c\n" c ; *)
  plain.(plain_of_char c) <- true
and unset_plain c =
(*  if c = '_' then eprintf "unset_plain %c\n" c ; *)
  plain.(plain_of_char c) <- false
and plain_back b c =
(*  if c = '_' then eprintf "plain_back %c <- %b\n" c b ; *)
  plain.(plain_of_char c) <- b


let top_level () = match !subst with Top -> true | _ -> false
and is_top = function
  | Top -> true
  | _   -> false


let prerr_args () = pretty_subst !subst


let scan_arg lexfun i  = match !subst with
  | Top ->  raise SubstTop
  | Env args ->
      if i >= Array.length args then begin
        if !verbose > 1 then begin
          prerr_string ("Subst arg #"^string_of_int (i+1)^" -> not found") ;
          pretty_subst !subst
        end ;
        raise (Error "Macro argument not found")
      end;
      let arg = args.(i) in
      if !verbose > 1 then begin
        eprintf
          "Subst arg #%i -> %a\n" i pretty_body arg.arg
      end ;
      let r = lexfun arg in
      r

and scan_body do_exec body args = match body with
| CamlCode _|Toks _ -> do_exec body
| Subst _ -> 
    let old_subst = !subst in
    subst := args ;
    let r = do_exec body in
    subst := old_subst ;
    r

(* Recoding and restoring lexbufs *)

let record_lexbuf lexbuf subst =
  MyStack.push stack_subst subst ;
  MyStack.push stack_lexbuf lexbuf ;

and previous_lexbuf () =
  let lexbuf = MyStack.pop stack_lexbuf in
  subst := MyStack.pop stack_subst ;
  lexbuf
;;

(* Saving and restoring lexing status *)

let stack_lexstate = MyStack.create "stack_lexstate"

let top_lexstate () = MyStack.empty stack_lexstate

let save_lexstate () =
  let old_stack = MyStack.save stack_subst in
  MyStack.push stack_subst !subst ;
  push stack_lexstate
    (MyStack.save stack_lexbuf,
     MyStack.save stack_subst) ;
  MyStack.restore stack_subst old_stack

and restore_lexstate () =
  let lexbufs,substs = pop stack_lexstate in
  MyStack.restore stack_lexbuf lexbufs ;
  MyStack.restore stack_subst substs ;
  subst := MyStack.pop stack_subst

(* Flags save and restore *)
let save_flags () = 
  push stack_display !display ;
  push stack_in_math !in_math

and restore_flags () =
  in_math := pop stack_in_math ;
  display := pop stack_display

(* Total checkpoint of lexstate *)
type saved_lexstate = 
(Lexing.lexbuf MyStack.saved * subst MyStack.saved) MyStack.saved *
bool MyStack.saved * bool MyStack.saved

let check_lexstate () =
  save_lexstate () ;
  save_flags () ;
  let r =
    MyStack.save stack_lexstate,
    MyStack.save stack_display,
    MyStack.save stack_in_math in
  restore_lexstate () ;
  restore_flags () ;
  r

and hot_lexstate (l,d,m) =
  MyStack.restore stack_lexstate l ;
  MyStack.restore stack_display d ;
  MyStack.restore stack_in_math m ;
  restore_lexstate ()  ;
  restore_flags ()
;;

(* Blank lexing status *)
let start_lexstate () =
  save_lexstate () ;
  MyStack.restore stack_lexbuf (MyStack.empty_saved) ;
  MyStack.restore stack_subst (MyStack.empty_saved)

let start_lexstate_subst this_subst =
  start_lexstate () ;
  subst := this_subst
;;

let flushing = ref false
;;


let start_normal this_subst =
  start_lexstate () ;
  save_flags () ;
  display := false ;
  in_math := false ;
  subst := this_subst

and end_normal () =
  restore_flags () ;
  restore_lexstate ()
;;

let full_peek_char lexbuf =
  let rec full_peek lexbuf =
    try
      Save.peek_next_char lexbuf
    with Not_found ->
      if MyStack.empty stack_lexbuf then
        raise Not_found
      else
        full_peek (previous_lexbuf ()) in
  full_peek lexbuf
        
let full_save_arg eoferror mkarg parg lexfun lexbuf =
  let rec save_rec lexbuf =
    try
      let arg = lexfun lexbuf in
      mkarg arg !subst
    with Save.Eof -> begin
        if MyStack.empty stack_lexbuf then
           eoferror () 
        else begin
          let lexbuf = previous_lexbuf () in
          if !verbose > 1 then begin
            prerr_endline "popping stack_lexbuf in full_save_arg";
            pretty_lexbuf lexbuf ;
            prerr_args ()
          end;
          save_rec lexbuf
        end
    end in

  let start_pos = Location.get_pos () in
  try 
    Save.seen_par := false ;
    save_lexstate () ;
    let r = save_rec lexbuf in
    restore_lexstate () ;
    if !verbose > 2 then
      prerr_endline ("Arg parsed: '"^parg r^"'") ;
    r
  with
  | (Save.Error _ | Error _) as e ->
      restore_lexstate () ;
      Save.seen_par := false ;
      Location.print_this_pos start_pos ;
      prerr_endline "Parsing of argument failed" ;
      raise e
  | e ->
      restore_lexstate () ;
      raise e
;;

let full_save_arg_limits eoferror parg lexfun lexbuf =
  let rec save_rec opt lexbuf =
    try
      lexfun opt lexbuf
    with Save.LimitEof r -> begin
        if MyStack.empty stack_lexbuf then
          match r with
          | None -> eoferror () 
          | _ -> r
        else begin
          let lexbuf = previous_lexbuf () in
          if !verbose > 1 then begin
            prerr_endline "popping stack_lexbuf in full_save_arg_limits";
            pretty_lexbuf lexbuf ;
            prerr_args ()
          end;
          save_rec r lexbuf
        end
    end in

  let start_pos = Location.get_pos () in
  try 
    Save.seen_par := false ;
    save_lexstate () ;
    let r = save_rec None lexbuf in
    restore_lexstate () ;
    if !verbose > 2 then
      prerr_endline ("Arg parsed: '"^parg r^"'") ;
    r
  with
  | (Save.Error _ | Error _) as e ->
      restore_lexstate () ;
      Save.seen_par := false ;
      Location.print_this_pos start_pos ;
      prerr_endline "Parsing of argument failed" ;
      raise e
  | e ->
      restore_lexstate () ;
      raise e
;;


type ok = No of string | Yes of string list
;;

let parg {arg=arg} = arg
and pargs {arg=args} = String.concat ", " args
and parg_list {arg=xs} = body_to_string xs

and pok = function
  | {arg=Yes s} -> String.concat "" s
  | {arg=No s} -> "* default arg: ["^s^"] *"


let eof_arg () =
  Save.empty_buffs () ;
  raise (Error "Eof while looking for argument")

let save_arg lexbuf =
  full_save_arg eof_arg mkarg parg Save.arg lexbuf
and save_body lexbuf =
    full_save_arg eof_arg mkarg parg_list Save.arg_list lexbuf
and save_arg_with_delim delim lexbuf =
  full_save_arg eof_arg mkarg parg (Save.with_delim delim) lexbuf
and save_filename lexbuf =
  full_save_arg eof_arg mkarg parg Save.filename lexbuf
and save_verbatim lexbuf =
  full_save_arg eof_arg mkarg parg Save.arg_verbatim lexbuf
and save_xy_arg lexbuf =
  full_save_arg eof_arg mkarg parg Save.xy_arg lexbuf
and save_cite_arg lexbuf =
  full_save_arg eof_arg mkarg pargs Save.cite_arg lexbuf

type sup_sub = {
  limits : Misc.limits option ;
  sup : string arg ;
  sub : string arg ;
} 

let plimits = function
  | Some Limits ->    "\\limits"
  | Some NoLimits ->  "\\nolimits"
  | Some IntLimits -> "\\intlimits"
  | None          -> "*no limit info*"

exception Over
let eof_over () = raise Over

let save_limits lexbuf =
  let rec do_rec res =
    try
      let r =
        full_save_arg_limits
          eof_over plimits Save.get_limits lexbuf in
      match r with
      | None -> res
      | Some _ -> do_rec r
    with
    | Over -> res in
  do_rec None

let mkoptionarg opt subst = match opt with
| None -> None
| Some s -> Some (mkarg s subst)

and poptionarg = function
| None -> "*None*"
| Some a -> a.arg

let save_sup lexbuf =
  try
   full_save_arg eof_over mkoptionarg poptionarg Save.get_sup lexbuf
  with
  | Over -> None

and save_sub lexbuf =
  try
    full_save_arg eof_over mkoptionarg poptionarg Save.get_sub lexbuf
  with
  | Over -> None

let unoption = function
  | None   -> {arg="" ; subst=top_subst }
  | Some a -> a

let save_sup_sub lexbuf =
  let limits = save_limits lexbuf in
  match save_sup lexbuf with
  | None ->
      let sub = save_sub lexbuf in
      let sup = save_sup lexbuf in
      {limits=limits  ; sup = unoption sup ; sub = unoption sub}
  | Some sup ->
      let sub = save_sub lexbuf in
      {limits=limits  ; sup = sup ; sub = unoption sub}

let protect_save_string lexfun lexbuf =
  full_save_arg eof_arg
    (fun s _ -> s)
    (fun s -> s)
    lexfun lexbuf

let eof_opt default () = {arg=No default ; subst=Top }

let save_arg_opt default lexbuf =
  let r = 
    full_save_arg
      (eof_opt default)
      mkarg
      pok
      (fun lexbuf ->
        try Yes (Save.opt_list lexbuf) with          
        | Save.NoOpt -> No default)
      lexbuf in
  match r.arg with
  | Yes _ -> r
  | No  _ -> mkarg (No default) !subst
      
  
;;


let from_ok okarg = match okarg.arg with
  | Yes s ->
      optarg := true ;
      mkarg s okarg.subst
  | No s  ->
      optarg := false ;
      mkarg [s] okarg.subst

let pretty_ok = function
  Yes s -> "+"^String.concat "" s^"+"
| No s  -> "-"^s^"-"
;;


let norm_arg s =
  String.length s = 2 && s.[0] = '#' &&
  ('0' <= s.[1] && s.[1] <= '9')

let list_arg a = { a with arg = [a.arg] }

let rec parse_args_norm pat lexbuf = match pat with
|   [] -> []
| s :: (ss :: _ as pat) when norm_arg s && norm_arg ss ->
    let arg = save_body lexbuf in
    let r = parse_args_norm pat lexbuf in
     arg :: r
| s :: ss :: pat when norm_arg s && not (norm_arg ss) ->
    let arg = save_arg_with_delim ss lexbuf in
    list_arg arg :: parse_args_norm pat lexbuf
| s :: pat when not (norm_arg s) ->
    Save.skip_delim s lexbuf ;
    parse_args_norm pat lexbuf
| _ :: pat ->
    let arg = save_body lexbuf in
    let r = parse_args_norm pat lexbuf in
    arg :: r
;;


let skip_csname lexbuf =
  let _ = Save.csname (fun x -> x) (fun x -> x) lexbuf in
  ()


let skip_opt lexbuf =
  let _ =  save_arg_opt "" lexbuf  in
  ()

and save_opt def lexbuf = from_ok (save_arg_opt def lexbuf)
;;

let rec save_opts pat lexbuf = match pat with
  [] -> []
| def::rest ->
   let arg = save_arg_opt def lexbuf in
   let r = save_opts rest lexbuf in
   arg :: r
;;


let parse_args (popt,pat) lexbuf =
  Save.seen_par := false ;
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
        Printf.fprintf stderr "\t#%d = %a\n" (i+1) pretty_body args.(i).arg ;
        pretty_subst (args.(i).subst)
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
  let lexbuf = MyLexing.from_string s in
  let r = lexfun lexbuf in
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this : over" ;
    prerr_endline ""
  end ;
  restore_lexstate ();
  r

and scan_this_list lexfun xs =
  start_lexstate ();
  if !verbose > 1 then begin
    eprintf "scan_this_list : [%a]" pretty_body xs ;
    prerr_endline ""  
  end ;
  let lexbuf = MyLexing.from_list xs in
  let r = lexfun lexbuf in
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_list : over" ;
    prerr_endline ""
  end ;
  restore_lexstate ();
  r

and scan_this_arg lexfun {arg=s ; subst=this_subst } =
  start_lexstate () ;
  subst := this_subst ;
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_arg : [%s]" s ;
    prerr_endline ""  
  end ;
  let lexbuf = MyLexing.from_string s in
  let r = lexfun lexbuf in
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_arg : over" ;
    prerr_endline ""
  end ;
  restore_lexstate ();
  r

and scan_this_arg_list lexfun {arg=xs ; subst=this_subst } =
  start_lexstate () ;
  subst := this_subst ;
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_arg_list : [%a]\n%!"
      pretty_body xs
  end ;
  let lexbuf = MyLexing.from_list xs in
  let r = lexfun lexbuf in
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_arg : over\n%!"
  end ;
  restore_lexstate ();
  r

;;

let scan_this_may_cont lexfun lexbuf cur_subst
    {arg=s ; subst=env } =
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
  let lexer = MyLexing.from_string s in
  let r = lexfun lexer in

  restore_lexstate ();
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_may_cont : over" ;
    prerr_endline ""
  end ;
  r

let scan_this_list_may_cont lexfun lexbuf cur_subst
    {arg=s ; subst=env } =
  if !verbose > 1 then begin
    eprintf "scan_this_list_may_cont : [%a]\n%!" pretty_body s ;
    if !verbose > 1 then begin
      prerr_endline "Pushing lexbuf and env" ;
      pretty_lexbuf lexbuf ;
      pretty_subst !subst
    end
  end ;
  save_lexstate ();
  record_lexbuf lexbuf cur_subst ;
  subst := env ;
  let lexer = MyLexing.from_list s in
  let r = lexfun lexer in

  restore_lexstate ();
  if !verbose > 1 then begin
    Printf.fprintf stderr "scan_this_list_may_cont : over" ;
    prerr_endline ""
  end ;
  r

let real_input_file loc_verb main filename input =
  if !verbose > 0 then
    prerr_endline ("Input file: "^filename) ;
  let buf = Lexing.from_channel input in
  Location.set filename buf ;
  let old_verb = !verbose in
  verbose := loc_verb ;
  if !verbose > 1 then prerr_endline ("scanning: "^filename) ;
  start_lexstate () ;
  let old_lexstate = MyStack.save stack_lexstate in
  subst := Top ;
  begin try  main buf with
  | Misc.EndInput ->
      MyStack.restore  stack_lexstate old_lexstate
  | e ->
      MyStack.restore  stack_lexstate old_lexstate ;
      restore_lexstate ();
      close_in input ;
      verbose := old_verb ;
(*   NO  Location.restore () ;  for proper error messages *)
      raise e
  end ;
  restore_lexstate ();
  if !verbose > 1 then prerr_endline ("scanning over: "^filename) ;    
  close_in input ;
  verbose := old_verb ;
  Location.restore ()  

let input_file loc_verb main filename =
  try
    let filename,input = Myfiles.open_tex filename in
    real_input_file loc_verb main filename input
  with Myfiles.Except -> begin
    if !verbose > 0 then
      prerr_endline ("Not opening file: "^filename) ;
    raise  Myfiles.Except
  end
 | Myfiles.Error m as x -> begin
     Misc.warning m ;
     raise x
 end


(* Hot start *)
type saved = (string * bool ref) list * bool list

let cell_list = ref []

let checkpoint () =
  !cell_list, List.map (fun (_,cell) -> !cell) !cell_list ;

and hot_start (cells, values)  =
  let rec start_rec cells values = match cells, values with
  | [],[] -> ()
  | (name,cell)::rcells, value :: rvalues ->
      if !verbose > 1 then begin
      prerr_endline
        ("Restoring "^name^" as "^if value then "true" else "false")
      end ;
      cell := value ;
      start_rec rcells rvalues
  | _,_ ->
      Misc.fatal ("Trouble in Lexstate.hot_start") in
  start_rec cells values ;
  cell_list := cells
  

let register_cell name cell =
  cell_list :=  (name,cell) :: !cell_list

and unregister_cell name =
  let rec un_rec = function
    | [] ->
        Misc.warning ("Cannot unregister cell: "^name) ;
        []
    | (xname,cell) :: rest ->
        if xname = name then rest
        else
          (xname,cell) :: un_rec rest in
  cell_list := un_rec !cell_list


open Misc

let header = ""

exception Error of string

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

(* arguments inside macros*)
type env = string array ref

let stack = ref [||]
and stack_stack = ref []
and stack_stack_stack = ref []
;;

let prerr_args_aux args =
  prerr_endline "Arguments: " ;
  for i = 0 to Array.length args - 1 do
    prerr_string "\t``" ;
    prerr_string args.(i) ;
    prerr_endline "''"
  done ;
  prerr_endline "End of arguments"

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
;;


let scan_arg lexfun i =
  if i >= Array.length !stack then
    raise (Error "Macro argument not found");
  let arg = !stack.(i) in
  if !verbose > 2 then
    prerr_endline ("Subst arg: ``"^arg^"''") ;
  let old_args = !stack in
  stack := pop stack_stack ;
  if !verbose > 2 then
    prerr_args_aux !stack;
  let r = lexfun arg in
  push stack_stack !stack ;
  stack := old_args ;
  r

and scan_body exec body args =
  push stack_stack !stack ;
  stack := args ;
  try
    let r = exec body in
    stack := pop stack_stack ;
    r
  with x -> begin
    stack := pop stack_stack ;
    raise x
  end

let eat_space = ref true
and stack_eat = ref []
;;

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

let out_file = ref (Out.create_null ())
;;

let prelude = ref true
;;

let flushing = ref false
;;

let stack_in_math = ref []
and stack_display = ref []

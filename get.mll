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

{
open Misc
open Parse_opts
open Lexing
open Latexmacros
open Lexstate
open Stack

(* Compute functions *)
let header = "$Id: get.mll,v 1.20 2000-05-30 12:28:40 maranget Exp $"

exception Error of string

let sbool = function
  | true -> "true"
  | false -> "false"

let get_this = ref (fun s -> assert false)
and get_fun = ref (fun f lexbuf -> assert false)
and open_env = ref (fun _ -> ())
and close_env = ref (fun _ -> ())
and get_csname = ref (fun _ -> assert false)
and main = ref (fun _ -> assert false)
;;

let bool_out = ref false
and int_out = ref false

let int_stack = Stack.create "int_stack"
and bool_stack = Stack.create "bool_stack"
and group_stack = Stack.create "group_stack"
and just_opened = ref false

type saved =
  bool * bool Stack.saved *
  bool * int Stack.saved * 
  (unit -> unit) Stack.saved * bool

let check () =
  !bool_out, Stack.save bool_stack,
  !int_out, Stack.save int_stack,
  Stack.save group_stack,
  !just_opened

and hot (b,bs,i,is,gs,j) =
  bool_out := b ; Stack.restore bool_stack bs ;
  int_out := i ; Stack.restore int_stack is ;
  Stack.restore group_stack gs ;
  just_opened := j

let push_int x =
  if !verbose > 2 then
    prerr_endline ("PUSH INT: "^string_of_int x) ;
  just_opened := false ;
  push int_stack x

let open_ngroups n =
  let rec open_ngroups_rec  = function
    | 0 ->()
    | n -> push group_stack (fun () -> ()) ; open_ngroups_rec (n-1) in
  if !verbose > 2 then
    prerr_endline ("OPEN NGROUPS: "^string_of_int n) ;
  if n > 0 then begin
    just_opened := true ;
    open_ngroups_rec n
  end

let close_ngroups n =
  let rec close_ngroups_rec  = function
    | 0 -> ()
    | n ->
        let f = pop group_stack in
        f() ; close_ngroups_rec (n-1) in
  if !verbose > 2 then
    prerr_endline ("CLOSE NGROUPS: "^string_of_int n);
  close_ngroups_rec n

let open_aftergroup f s =
  if !verbose > 2 then
    prerr_endline ("OPEN AFTER: "^s) ;
  just_opened := true ;
  push group_stack f

} 
let command_name =
 '\\' ((['@''A'-'Z' 'a'-'z']+ '*'?) | [^ '@' 'A'-'Z' 'a'-'z'])

rule result = parse
(* Skip comments and spaces *)
| '%' [^ '\n'] * '\n' {result lexbuf}
| [' ' '\n']+         {result lexbuf}
(* Integers *)
| ['0'-'9']+
    {let lxm = Lexing.lexeme lexbuf in
    push_int (int_of_string lxm) ;
    result lexbuf}
| '\'' ['0'-'7']+
    {let lxm = lexeme lexbuf in
    push_int
      (int_of_string ("0o"^String.sub lxm 1 (String.length lxm-1))) ;
    result lexbuf}
|  "\"" ['0'-'9' 'a'-'f' 'A'-'F']+
    {let lxm = lexeme lexbuf in
    push_int
      (int_of_string ("0x"^String.sub lxm 1 (String.length lxm-1))) ;
    result lexbuf}
| '`'
    {let token = !get_csname lexbuf in
    after_quote (Lexing.from_string token) ;
    result lexbuf}
|  "true"
    {push bool_stack true ;
    result lexbuf}
|  "false"
    {push bool_stack false ;
    result lexbuf}
(* Operands *)
| '+' | '-'
    {let lxm = lexeme_char lexbuf 0 in
    let unary = !just_opened in
    if unary then begin
      let f = pop group_stack in
      open_aftergroup
        (fun () ->
          if !verbose > 2 then begin
            prerr_endline ("UNARY: "^String.make 1 lxm) ;
            Stack.pretty string_of_int int_stack
          end ;
          let x1 = pop int_stack in
          let r = match lxm with
          | '+' -> x1
          | '-' -> 0 - x1
          | _   -> assert false in
          push_int r ; f()) "UNARY"
    end else begin
      close_ngroups 2 ;
      open_aftergroup
        (fun () ->
          if !verbose > 2 then begin
            prerr_endline ("OPPADD: "^String.make 1 lxm) ;
            Stack.pretty string_of_int int_stack
          end ;
          let x2 = pop int_stack in
          let x1 = pop int_stack in
          let r = match lxm with
          | '+' -> x1 + x2
          | '-' -> x1 - x2
          | _   -> assert false in
          push_int r) "ADD";
      open_ngroups 1 ;
    end ;
    result lexbuf}
| '/' | '*'
    {let lxm = lexeme_char lexbuf 0 in
    close_ngroups 1 ;
    open_aftergroup
        (fun () ->
          if !verbose > 2 then begin
            prerr_endline ("MULTOP"^String.make 1 lxm) ;
            Stack.pretty string_of_int int_stack
          end ;
          let x2 = pop int_stack in
          let x1 = pop int_stack in
          let r = match lxm with
          | '*' -> x1 * x2
          | '/' -> x1 / x2
          | _   -> assert false in
          push_int r) "MULT";
    result lexbuf}
(* boolean openrands *)
| '<' | '>' | '=' 
    {let lxm = Lexing.lexeme_char lexbuf 0 in
    close_ngroups 3 ;
    open_aftergroup
      (fun () ->
        if !verbose > 2 then begin
          prerr_endline ("COMP: "^String.make 1 lxm) ;
          Stack.pretty string_of_int int_stack
        end ;
        let x2 = pop int_stack in
        let x1 = pop int_stack in              
        push bool_stack
          (match lxm with
          | '<' -> x1 < x2
          | '>' -> x1 > x2
          | '=' -> x1 = x2
          | _   -> assert false) ;
          if !verbose > 2 then
            Stack.pretty sbool bool_stack) "COMP" ;
    open_ngroups 2 ;
    result lexbuf}

(* Parenthesis for integer computing *)
| '('|'{'
    {open_ngroups 2 ;
    result lexbuf}
| ')'|'}'
    {close_ngroups 2 ;
    result lexbuf}
(* Commands *)
|  '#' ['1'-'9']
    {let lxm = lexeme lexbuf in
    let i = Char.code (lxm.[1]) - Char.code '1' in
    scan_arg (scan_this result) i ;
    result lexbuf} 
| command_name
    {let lxm = lexeme lexbuf in
    let pat,body = Latexmacros.find lxm in
    let args = make_stack lxm pat lexbuf in
    scan_body
      (function
        | Subst body ->
            scan_this result body
        | CamlCode f ->
            let rs = !get_fun f lexbuf in
            scan_this result rs)
          body args ;
    result lexbuf}
| _   {raise (Error ("Bad character in Get.result: ``"^lexeme lexbuf^"''"))}
| eof {()}

and after_quote = parse
|  '\\'  [^ 'A'-'Z' 'a'-'z'] eof
    {let lxm = lexeme lexbuf in
    push_int (Char.code lxm.[1]);
    result lexbuf}
|  _ eof
    {let lxm = lexeme lexbuf in
    push_int (Char.code lxm.[0]);
    result lexbuf} 
| ""
    {Misc.fatal "Cannot understand `-like numerical argument"}
{
let init latexget latexgetfun latexopenenv latexcloseenv latexcsname
    latexmain =
  get_this := latexget ;
  get_fun := latexgetfun ;
  open_env := latexopenenv ;
  close_env := latexcloseenv ;
  get_csname := latexcsname ;
  main := latexmain
;;

let def_loc  name f = 
  Latexmacros.def name zero_pat (CamlCode f) ;
;;

let def_commands l =
  List.map
    (fun (name,f) ->
      name,Latexmacros.replace name (Some (zero_pat,CamlCode f)))
    l

let def_commands_int () =
  def_commands
    ["\\value",
      (fun lexbuf ->
        let name = !get_this (save_arg lexbuf) in
        push_int (Counter.value_counter name)) ;
      "\\pushint",
        (fun lexbuf ->
          let s = !get_this (save_arg lexbuf) in
          scan_this result s)]

let def_commands_bool () =  
  let old_ints = def_commands_int () in
  let old_commands =
    def_commands
      ["\\(", (fun _ -> open_ngroups 7) ;
        "\\)",  (fun _ -> close_ngroups 7) ;
        "\\@fileexists",
        (fun lexbuf ->
          let name = !get_this (save_arg lexbuf) in
          push bool_stack
            (try
              let _ = Myfiles.open_tex name in
              true
            with Myfiles.Except | Myfiles.Error _ -> false)) ;
        "\\@commandexists",
        (fun lexbuf ->
          let name = !get_csname lexbuf in
          push bool_stack (Latexmacros.exists name)) ;
        "\\or",
        (fun _ ->
          close_ngroups 7 ;
          open_aftergroup
            (fun () ->
              if !verbose > 2 then begin
                prerr_endline "OR" ;
                Stack.pretty sbool bool_stack
              end ;
              let b1 = pop bool_stack in
              let b2 = pop bool_stack in
              push bool_stack (b1 || b2)) "OR";
          open_ngroups 6) ;
        "\\and",
        (fun _ ->
          close_ngroups 6 ;
          open_aftergroup
            (fun () ->
              if !verbose > 2 then begin
                prerr_endline "AND" ;
                Stack.pretty sbool bool_stack
              end ;
              let b1 = pop bool_stack in
              let b2 = pop bool_stack in
              push bool_stack (b1 && b2)) "AND";            
          open_ngroups 5) ;
        "\\not",
        (fun _ ->
          close_ngroups 4 ;
          open_aftergroup
            (fun () ->
              if !verbose > 2 then begin
                prerr_endline "NOT" ;
                Stack.pretty sbool bool_stack
              end ;
              let b1 = pop bool_stack in
              push bool_stack (not b1)) "NOT";
          open_ngroups 3) ;
        "\\boolean",
          (fun lexbuf ->
            let name = !get_this (save_arg lexbuf) in
            let b = try
              let r = !get_this
                  ("\\if"^name^" true\\else false\\fi",get_subst ()) in
              match r with
              | "true" -> true
              | "false" -> false
              | _ -> raise (Misc.Fatal ("boolean value: "^r))
            with
              Latexmacros.Failed -> true  in
            push bool_stack b) ;
        "\\isodd",
        (fun lexbuf ->
          close_ngroups 3 ;
          open_aftergroup
            (fun () ->
              if !verbose > 2 then begin
                prerr_endline ("ISODD") ;
                Stack.pretty string_of_int int_stack
              end ;
              let x = pop int_stack in
              push bool_stack (x mod 2 = 1) ;
              if !verbose > 2 then
                Stack.pretty sbool bool_stack) "ISODD" ;
          open_ngroups 2) ] in
  let old_equal =
    try Some (Latexmacros.find_fail "\\equal") with Failed -> None in
  
  def_loc "\\equal"
    (fun lexbuf ->
      let arg1 = save_arg lexbuf in
      let arg2 = save_arg lexbuf in
      scan_this !main "\\begin{@norefs}" ;
      let again = List.map (fun (name,x) -> name,Latexmacros.replace name x)
          ((("\\equal",old_equal)::old_ints)@old_commands) in
      push bool_stack (!get_this arg1 = !get_this arg2) ;
      let _ =
        List.map (fun (name,x) -> Latexmacros.replace name x) again in
      scan_this !main "\\end{@norefs}")



let first_try s =
  let l = String.length s in
  if l <= 0 then raise (Failure "first_try") ;
  let rec try_rec r i =
    if i >= l then r
    else match s.[i] with
    | '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' ->
        try_rec (10*r + Char.code s.[i] - Char.code '0') (i+1)
    | _ -> raise (Failure ("first_try")) in
  try_rec 0 0
;;

let get_int (expr, subst) =
  if !verbose > 1 then
    prerr_endline ("get_int : "^expr) ;
  let r =
    try first_try expr with Failure _ -> begin
      let old_int = !int_out in
      int_out := true ;
      start_normal subst ;
      !open_env "*int*" ;
      let _ = def_commands_int () in
      open_ngroups 2 ;
      begin try scan_this result expr with
      | x ->
          begin
            prerr_endline
              ("Error while scanning ``"^expr^"'' for integer result");
            raise x
          end
      end ;
      close_ngroups 2 ;
      !close_env "*int*" ;
      end_normal () ;
      if Stack.empty int_stack then
        raise (Error ("``"^expr^"'' has no value as an integer"));
      let r = pop int_stack in
      int_out := old_int ;
      r end in
  if !verbose > 1 then
    prerr_endline ("get_int: "^expr^" = "^string_of_int r) ;
  r
  

let get_bool (expr,subst) =
  if !verbose > 1 then
    prerr_endline ("get_bool : "^expr) ;
  let old_bool = !bool_out in
  bool_out := true ;
  start_normal subst ;
  !open_env "*bool*" ;
  def_commands_bool () ;
  open_ngroups 7 ;
  begin try scan_this result expr with
  | x ->
      begin
        prerr_endline
          ("Error while scanning ``"^expr^"'' for boolean result");
        raise x
      end
  end ;
  close_ngroups 7 ;
  !close_env "*bool*" ;
  end_normal () ;
  if Stack.empty bool_stack then
    raise (Error ("``"^expr^"'' has no value as a boolean"));
  let r = pop bool_stack in
  if !verbose > 1 then
    prerr_endline ("get_bool: "^expr^" = "^sbool r);
  bool_out := old_bool ;
  r

let get_length ((expr,_) as arg) =
  if !verbose > 1 then
    prerr_endline ("get_length : "^expr) ;
  let r = Length.main (Lexing.from_string (Subst.do_subst_this arg)) in
  if !verbose > 2 then begin
    prerr_string ("get_length : "^expr^" -> ") ;
    prerr_endline (Length.pretty r)
  end ;
  r
} 

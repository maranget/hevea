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

(* Compute functions *)
let header = "$Id: get.mll,v 1.3 1999-04-02 18:34:15 maranget Exp $"

exception Error of string

let sbool = function
  | true -> "true"
  | false -> "false"

let subst_this = ref (fun s -> s)
and get_this = ref (fun s -> s)
let init latexsubst latexget =
  subst_this := latexsubst ;
  get_this := latexget
       
let bool_out = ref false
and int_out = ref false
;;

let int_stack = Lexstate.create ()
and bool_stack = Lexstate.create ()
and group_stack = Lexstate.create ()
and just_opened = ref false

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
let command_name = '\\' (('@' ? ['A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])

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
| '`' '\\'  [^ 'A'-'Z' 'a'-'z']
    {let lxm = lexeme lexbuf in
    push_int (Char.code lxm.[2]);
    result lexbuf}
| '`' _
    {let lxm = lexeme lexbuf in
    push_int (Char.code lxm.[1]);
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
            prerr_stack_string "int" string_of_int int_stack
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
            prerr_stack_string "int" string_of_int int_stack
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
            prerr_stack_string "int" string_of_int int_stack
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
          prerr_stack_string "int" string_of_int int_stack
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
            prerr_stack_string "bool" sbool bool_stack) "COMP" ;
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
    match lxm with
    | "\\(" when !bool_out ->
        open_ngroups 7 ;
        result lexbuf
    | "\\)" when !bool_out ->
        close_ngroups 7 ;
        result lexbuf
    | "\\@fileexists" when !bool_out ->
        let name = !subst_this (save_arg lexbuf) in
        push bool_stack
          (try
            let _ = Myfiles.open_tex name in
            true
          with Myfiles.Except | Myfiles.Error _ -> false) ;
        result lexbuf
    | "\\equal" when !bool_out ->
        let arg1 = save_arg lexbuf in
        let arg2 = save_arg lexbuf in
        push bool_stack (!get_this arg1 = !get_this arg2) ;
        result lexbuf
    | "\\or" when !bool_out ->
        close_ngroups 7 ;
        open_aftergroup
          (fun () ->
            if !verbose > 2 then begin
              prerr_endline "OR" ;
              prerr_stack_string "bool" sbool bool_stack
            end ;
            let b1 = pop bool_stack in
            let b2 = pop bool_stack in
            push bool_stack (b1 || b2)) "OR";
        open_ngroups 6 ;
        result lexbuf
    | "\\and" when !bool_out ->
        close_ngroups 6 ;
        open_aftergroup
          (fun () ->
            if !verbose > 2 then begin
              prerr_endline "AND" ;
              prerr_stack_string "bool" sbool bool_stack
            end ;
            let b1 = pop bool_stack in
            let b2 = pop bool_stack in
            push bool_stack (b1 && b2)) "AND";            
        open_ngroups 5 ;
        result lexbuf
    | "\\not" when !bool_out ->
        close_ngroups 4 ;
        open_aftergroup
          (fun () ->
            if !verbose > 2 then begin
              prerr_endline "NOT" ;
              prerr_stack_string "bool" sbool bool_stack
            end ;
            let b1 = pop bool_stack in
            push bool_stack (not b1)) "NOT";
        open_ngroups 3;
        result lexbuf
    | "\\boolean" when !bool_out ->
        let name = !subst_this (save_arg lexbuf) in
        let b = try
          let _,body = Latexmacros.find_macro ("\\if"^name) in
          match body with
          | Latexmacros.Test cell -> !cell
          | _         -> raise (Error ("Bad \\if"^name^" macro"))
        with
          Latexmacros.Failed -> true  in
        push bool_stack b ;
        result lexbuf
    | "\\isodd" when !bool_out ->
        close_ngroups 3 ;
        open_aftergroup
          (fun () ->
            if !verbose > 2 then begin
              prerr_endline ("ISODD") ;
              prerr_stack_string "int" string_of_int int_stack
            end ;
        let x = pop int_stack in
        push bool_stack (x mod 2 = 1) ;
        if !verbose > 2 then
            prerr_stack_string "bool" sbool bool_stack) "ISODD" ;
        open_ngroups 2 ;
        result lexbuf
    | "\\value" ->
        let name = !subst_this (Save.arg lexbuf) in
        push_int (Counter.value_counter name) ;
        result lexbuf
    | _ ->
        let pat,body = Latexmacros.find_macro lxm in
        let args = make_stack lxm pat lexbuf in
        scan_body
          (function
            | Latexmacros.Subst body ->
                scan_this result body
            | _ -> raise (Error ("Special macro in Get.result")))
          body args ;
        result lexbuf}
| _   {raise (Error ("Bad character in Get.result: ``"^lexeme lexbuf^"''"))}
| eof {()}

{
let get_int expr =
  if !verbose > 1 then
    prerr_endline ("get_int : "^expr) ;
  let old_int = !int_out in
  int_out := true ;
  start_normal display in_math ;
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
  end_normal display in_math ;
  if Lexstate.empty int_stack then
    raise (Error ("``"^expr^"'' has no value as an integer"));
  let r = pop int_stack in
  if !verbose > 1 then
    prerr_endline ("get_int: "^expr^" = "^string_of_int r) ;
  int_out := old_int ;
  r

let get_bool expr =
  if !verbose > 1 then
    prerr_endline ("get_bool : "^expr) ;
  let old_bool = !bool_out in
  bool_out := true ;
  start_normal display in_math ;
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
  end_normal display in_math ;
  if Lexstate.empty bool_stack then
    raise (Error ("``"^expr^"'' has no value as an integer"));
  let r = pop bool_stack in
  if !verbose > 1 then
    prerr_endline ("get_bool: "^expr^" = "^sbool r);
  bool_out := old_bool ;
  r

} 

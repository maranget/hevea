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
let header = "$Id: get.mll,v 1.11 1999-10-01 16:15:17 maranget Exp $"

exception Error of string

let sbool = function
  | true -> "true"
  | false -> "false"

let get_this = ref (fun b s -> assert false)
and register_this = ref (fun s -> ())
and open_env = ref (fun _ -> ())
and close_env = ref (fun _ -> ())
;;

let bool_out = ref false
and int_out = ref false
;;

let int_stack = Stack.create "int_stack"
and bool_stack = Stack.create "bool_stack"
and group_stack = Stack.create "group_stack"
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
    let pat,body = Latexmacros.find_macro lxm in
    let args = make_stack lxm pat lexbuf in
    scan_body
      (function
        | Subst body ->
            scan_this result body
        | CamlCode f -> f lexbuf)
          body args ;
    result lexbuf}
| _   {raise (Error ("Bad character in Get.result: ``"^lexeme lexbuf^"''"))}
| eof {()}

{
let init latexget latexregister latexopenenv latexcloseenv =
  get_this := latexget ;
  register_this := latexregister ;
  open_env := latexopenenv ;
  close_env := latexcloseenv
;;
let def_loc name f =
  silent_def name 0 (CamlCode f) ;
  !register_this name
;;

let def_commands_int () =
  def_loc "\\value"
    (fun lexbuf ->
      let name = !get_this true (save_arg lexbuf) in
      push_int (Counter.value_counter name))
;;


let def_commands_bool () =
  def_loc "\\(" (fun _ -> open_ngroups 7) ;
  def_loc "\\)"  (fun _ -> close_ngroups 7) ;
  def_loc "\\@fileexists"
    (fun lexbuf ->
      let name = !get_this true (save_arg lexbuf) in
      push bool_stack
        (try
          let _ = Myfiles.open_tex name in
          true
        with Myfiles.Except | Myfiles.Error _ -> false)) ;
  def_loc "\\equal"
    (fun lexbuf ->
        let arg1 = save_arg lexbuf in
        let arg2 = save_arg lexbuf in
        push bool_stack (!get_this false arg1 = !get_this false arg2)) ;
  def_loc "\\or"
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
  def_loc "\\and"
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
  def_loc "\\not"
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
  def_loc "\\boolean"
    (fun lexbuf ->
      let name = !get_this true (save_arg lexbuf) in
      let b = try
        let r = !get_this true
            ("\\if"^name^" true\\else false\\fi",get_subst ()) in
        match r with
        | "true" -> true
        | "false" -> false
        | _ -> raise (Misc.Fatal ("boolean value: "^r))
      with
        Latexmacros.Failed -> true  in
      push bool_stack b) ;
  def_loc "\\isodd"
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
      open_ngroups 2) ;
  def_commands_int ()
;;

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
      def_commands_int () ;
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
    raise (Error ("``"^expr^"'' has no value as an integer"));
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

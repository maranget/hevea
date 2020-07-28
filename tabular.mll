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

(* $Id: tabular.mll,v 1.33 2012-06-05 14:55:39 maranget Exp $ *)
{
open Misc
open Lexing
open Table
open Lexstate
open Subst

exception Error of string
;;

type align =
    {hor : string ; mutable vert : string ; wrap : bool ;
      mutable pre : string ; mutable post : string ; width : Length.t}

let make_hor = function
    'c' -> "center"
  | 'l' -> "left"
  | 'r' -> "right"
  | 'p'|'m'|'b' -> "left"
  | _ -> raise (Misc.Fatal "make_hor")

and make_vert = function
  | 'c'|'l'|'r' -> ""
  | 'p' -> "top"
  | 'm' -> "middle"
  | 'b' -> "bottom"
  | _ -> raise (Misc.Fatal "make_vert")

type format =
  Align of align
| Inside of string
| Border of string
;;

(* Patch vertical alignment (for HTML) *)
let check_vert f =
  try
    for i = 0 to Array.length f-1 do
      match f.(i) with
      | Align {vert=s} when s <> "" -> raise Exit
      | _ -> ()
    done ;
    f
  with Exit -> begin
    for i = 0 to Array.length f-1 do
      match f.(i) with
      | Align ({vert=""} as f) ->
          f.vert <- "top"
      | _ -> ()
    done ;
    f
  end

(* Compute missing length (for text) *)
and check_length f =
  for i = 0 to Array.length f - 1 do
    match f.(i) with
    | Align ({wrap=true ; width=Length.NotALength _} as r) ->
        f.(i) <-
           Align
             {r with
              width =
              Length.Percent
                (truncate (100.0 /. float (Array.length f)))}
    | _ -> ()
  done

let border = ref false



let out_table = Table.create (Inside "")

let pretty_format = function
  |   Align {vert = v ; hor = h ; pre = pre ; post = post ; wrap = b ; width = w}
      ->
        "[>{"^pre^"}"^
        ", h="^h^", v="^v^
        ", <{"^post^"}"^(if b then ", wrap" else "")^
        ", w="^Length.pretty w^"]"
  | Inside s -> "@{"^s^"}"
  | Border s -> s

let pretty_formats f =
  Array.iter (fun f -> prerr_string (pretty_format f) ; prerr_string "; ") f

(* For some reason pre/post-ludes are executed right to left *)
let concat_pre_post x y = match x, y with
| "", _ -> y
| _, "" -> x
| _,_   -> y ^ "{}" ^ x
} 

rule tfone = parse
| [' ''\t''\n''\r'] {tfone lexbuf}
| '>'
    {let pre = subst_arg lexbuf in
    tfone lexbuf ;
    try
      apply out_table (function
        |  Align a ->
            a.pre <- concat_pre_post pre a.pre ;
        | _ -> raise (Error "Bad syntax in array argument (>)"))
    with Table.Empty ->
      raise (Error "Bad syntax in array argument (>)")}
| "" {tfmiddle lexbuf}

and tfmiddle = parse
| [' ''\t''\n''\r'] {tfmiddle lexbuf}
| ['c''l''r']
  {let f = Lexing.lexeme_char lexbuf 0 in
  let post = tfpostlude lexbuf in
  emit out_table
    (Align {hor = make_hor f ; vert = make_vert f ; wrap = false ;
        pre = "" ;   post = post ; width = Length.Default})}
| ['p''m''b']
  {let f = Lexing.lexeme_char lexbuf 0 in
  let width = subst_arg lexbuf in
  let my_width = Length.main (MyLexing.from_string width) in
  let post = tfpostlude lexbuf in
  emit out_table
    (Align {hor = make_hor f ; vert = make_vert f ; wrap = true ;
          pre = "" ;   post = post ; width = my_width})}
| '#' ['1'-'9']
    {let lxm = lexeme lexbuf in
    let i = Char.code (lxm.[1]) - Char.code '1' in
    Lexstate.scan_arg (scan_this_arg_list tfmiddle) i}
| '%' [^'\n']* '\n'
    {tfmiddle lexbuf}
| [^'|' '@' '<' '>' '!' '#']
    {let lxm = lexeme lexbuf in
    let name = column_to_command lxm in
    let pat,body = Latexmacros.find name in
    let args = Lexstate.make_stack name pat lexbuf in
    let cur_subst = get_subst () in
    Lexstate.scan_body
      (function
        | Lexstate.Subst body ->
            scan_this_list_may_cont
              lexformat lexbuf  cur_subst (string_to_arg body) ;            
        | _ -> assert false)
      body args ;
    let post = tfpostlude lexbuf in
    if post <> "" then
      try
        Table.apply out_table
          (function
            | Align f -> f.post <- post
            | _ -> Misc.warning ("``<'' after ``@'' in tabular arg scanning"))
      with
      | Table.Empty ->
          raise (Error ("``<'' cannot start tabular arg"))}
| eof {()}
| ""
  {let rest =
    Bytes.sub_string lexbuf.lex_buffer lexbuf.lex_curr_pos
      (lexbuf.lex_buffer_len - lexbuf.lex_curr_pos) in
  raise (Error ("Syntax of array format near: "^rest))}

and tfpostlude = parse
| [' ''\t''\n''\r'] {tfpostlude lexbuf}
| '<'
    {let one = subst_arg lexbuf in
    let rest = tfpostlude lexbuf in
    let r = concat_pre_post one rest in
    r}
| eof
    {if MyStack.empty stack_lexbuf then
      ""
    else
      let lexbuf = previous_lexbuf () in
      tfpostlude lexbuf}
| ""  {""}


and lexformat = parse
| [' ''\t''\n''\r'] {lexformat lexbuf}
| '*'
   {let ntimes = save_arg lexbuf in
   let what = save_arg lexbuf in
   let rec do_rec = function
     0 -> lexformat lexbuf
   | i ->
      scan_this_arg lexformat what ; do_rec (i-1) in
   do_rec (Get.get_int_string ntimes)}
| '|' {border := true ; emit out_table (Border "|") ; lexformat lexbuf}
| '@'|'!'
    {let lxm = Lexing.lexeme_char lexbuf 0 in
    let inside = subst_arg lexbuf in
    if lxm = '!' || inside <> "" then emit out_table (Inside inside) ;
    lexformat lexbuf}
| '#' ['1'-'9']
    {let lxm = lexeme lexbuf in
    let i = Char.code (lxm.[1]) - Char.code '1' in
    Lexstate.scan_arg (scan_this_arg_list lexformat) i ;
    lexformat lexbuf}
| eof
    {if MyStack.empty stack_lexbuf then
      ()
    else
      let lexbuf = previous_lexbuf () in
      lexformat lexbuf}
| "" {tfone lexbuf ; lexformat lexbuf}



{
open Parse_opts

let main {arg=s ; subst=env} =
  if !verbose > 1 then prerr_endline ("Table format: "^s);
  let lexbuf =
    if String.length s > 0 && s.[0] = '\\' then
      match Latexmacros.find s with
      | _, Lexstate.Subst s -> MyLexing.from_list s
      | _,_ -> MyLexing.from_string s
    else
      MyLexing.from_string s in
  start_normal env ;
  lexformat lexbuf ;
  end_normal () ;
  let r = check_vert (trim out_table) in
  begin match !destination with
  | (Text | Info) -> check_length r
  | Html -> ()
  end ;
  if !verbose > 1 then begin
    prerr_string "Format parsed: " ;
    pretty_formats r ;
    prerr_endline ""
  end ;
  r
}


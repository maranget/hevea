{
open Misc
open Lexing
open Table
open Lexstate

let header = "$Id: tabular.mll,v 1.15 1999-09-08 20:26:47 maranget Exp $"

exception Error of string
;;

let subst_this = ref (fun s -> s)

let init latexsubst (*latexgetint*) =
  subst_this := latexsubst ;

type align =
    {hor : string ; mutable vert : string ; wrap : bool ;
      mutable pre : string ; mutable post : string ; width : Length.t option}

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


let border = ref false



let push s e = s := e:: !s
and pop s = match !s with
  [] -> raise (Misc.Fatal "Empty stack in Latexscan")
| e::rs -> s := rs ; e

let out_table = Table.create (Inside "")

let pretty_format = function
  |   Align {vert = v ; hor = h ; pre = pre ; post = post ; wrap = b ; width = w}
      ->
        ">{"^pre^"}"^
        "h="^h^" v="^v^
        "<{"^post^"}"^(if b then " wrap" else "")^
	(match w with
	| Some Length.Absolute l -> " w="^string_of_int l
	| Some Length.Percent l -> " w="^string_of_int l^"%"
	| None -> "")
  | Inside s -> "@{"^s^"}"
  | Border s -> s

let pretty_formats f =
  Array.iter (fun f -> prerr_string (pretty_format f) ; prerr_char ',') f


} 

rule tfone = parse
  '>'
    {let pre = !subst_this (Save.arg lexbuf) in
    tfmiddle lexbuf ;
    try
      apply out_table (function
        |  Align a as r -> a.pre <- pre
        | _ -> raise (Error "Bad syntax in array argument (>)"))
    with Failure "Table.apply" ->
      raise (Error "Bad syntax in array argument (>)")}
| "" {tfmiddle lexbuf}

and tfmiddle = parse
  'c'|'l'|'r'
  {let f = Lexing.lexeme_char lexbuf 0 in
  let post = tfpostlude lexbuf in
  emit out_table
    (Align {hor = make_hor f ; vert = make_vert f ; wrap = false ;
        pre = "" ;   post = post ; width = None})}
| 'p'|'m'|'b'
  {let f = Lexing.lexeme_char lexbuf 0 in
  let width = !subst_this (Save.arg lexbuf) in
  let my_width =
    try Some (Length.main (Lexing.from_string width))
    with Length.No -> None in
  let post = tfpostlude lexbuf in
  emit out_table
    (Align {hor = make_hor f ; vert = make_vert f ; wrap = true ;
          pre = "" ;   post = post ; width = my_width})}
| '#' ['1'-'9']
    {let lxm = lexeme lexbuf in
    let i = Char.code (lxm.[1]) - Char.code '1' in
    Lexstate.scan_arg (scan_this tfmiddle) i}

| ['a'-'z''A'-'Z']
    {let lxm = lexeme lexbuf in
    let name = column_to_command lxm in
    let pat,body = Latexmacros.find_macro name in
    let args = Lexstate.make_stack name pat lexbuf in
    Lexstate.scan_body
      (function
        | Lexstate.Subst body -> scan_this lexformat body ;            
        | _ -> assert false)
      body args ;
    let post = tfpostlude lexbuf in
    Table.apply out_table
      (function
        | Align f -> f.post <- post
        | _ -> Misc.warning ("``<'' after ``@'' in tabular arg scanning"))}
| eof {()}
| ""
  {let rest =
    String.sub lexbuf.lex_buffer lexbuf.lex_curr_pos
      (lexbuf.lex_buffer_len - lexbuf.lex_curr_pos) in
  raise (Error ("Syntax of array format near: "^rest))}

and tfpostlude = parse
  '<' {!subst_this (Save.arg lexbuf)}
| ""  {""}


and lexformat = parse
 '*'
   {let ntimes = Save.arg lexbuf in
   let what = Save.arg lexbuf in
   let rec do_rec = function
     0 -> lexformat lexbuf
   | i ->
      let sbuf = Lexing.from_string what in
      lexformat sbuf ; do_rec (i-1) in
   do_rec (Get.get_int ntimes)}
| '|' {border := true ; emit out_table (Border "|") ; lexformat lexbuf}
| '@'|'!'
    {let lxm = Lexing.lexeme_char lexbuf 0 in
    let inside = !subst_this (Save.arg lexbuf) in
    if lxm = '!' || inside <> "" then emit out_table (Inside inside) ;
    lexformat lexbuf}
| '#' ['1'-'9']
    {let lxm = lexeme lexbuf in
    let i = Char.code (lxm.[1]) - Char.code '1' in
    Lexstate.scan_arg (scan_this lexformat) i ;
    lexformat lexbuf}
| eof {()}
| "" {tfone lexbuf ; lexformat lexbuf}



{

let main   s =
  lexformat (Lexing.from_string s) ; check_vert (trim out_table)
}


{
open Lexing
open Latexmacros
open Html

let verbose = ref 0
;;

let out_file = ref (Out.create_null ())
;;

let tex_path =  try
  let r = ref []
  and j = ref 0 in
  let s = Sys.getenv "TEXINPUTS" in
  for i=0 to String.length s-1 do
    match String.get s i with
     ':' ->
        r := String.sub s !j (i- !j) :: !r ;
        j := i+1
    | _  -> ()
  done ;
  r := String.sub s !j (String.length s - !j -1) :: !r ;
  List.rev !r
with Not_found -> ["." ; "/usr/local/lib/tex"]
;;

exception Found of (string * in_channel)
;;

let open_tex filename =
  let filename =
    if Filename.check_suffix filename ".tex" then filename
    else if Filename.check_suffix filename ".sty" then filename
    else filename^".tex" in  
  try
    List.iter (fun dir ->
      try
        let full_name = Filename.concat dir filename in
        let r = open_in full_name in
        raise (Found (full_name,r))
      with Sys_error _ -> ())
    tex_path ;
    failwith ("File not found: "^filename)
  with Found r -> r
;;


let env_extract s =
  let i = String.index s '{'
  and j = String.rindex s '}' in
  String.sub s (i+1) (j-i-1)
;;

let name_extract s =
  let r =
  if s = "\\ " then s
  else
    let j = String.rindex s '\\' in
    let s = String.sub s j (String.length s - j) in
    let j = try String.index s ' ' with Not_found -> String.length s in
    String.sub s 0 j in
  if !verbose > 2 then begin
    Printf.fprintf stderr "name_extract [%s] -> [%s]" s  r ;
    prerr_newline ()
  end ;
  r
and spaces_extract s =
  let i = ref 0 in
  while String.get s !i = ' ' || String.get s !i = '\n' do
    i := !i + 1
  done ;
  String.sub s 0 !i
;;


let binds s = String.get s 0 = '#'
;;

let save_arg lexbuf =
  let arg =  Save.arg lexbuf in
  if !verbose > 2 then
    prerr_endline ("Arg parsed: <"^arg^">") ;
  arg
;;

let rec parse_args_norm pat lexbuf = match pat with
  [] -> []
| s1::s2::pat ->
   if binds s1 then
     if binds s2 then begin
        let arg = save_arg lexbuf in
        arg::parse_args_norm (s2::pat) lexbuf
     end else
        let arg = Save.arg_delim lexbuf s2 in
        arg::parse_args_norm pat lexbuf
   else begin
     Save.eat_delim lexbuf s1 ;
     parse_args_norm (s2::pat) lexbuf
   end
| [s] ->
    if binds s then
      [save_arg lexbuf]
    else begin
      Save.eat_delim lexbuf s ;
      []
    end
;;

let rec parse_nargs n lexbuf = match n with
  0 -> []
| _ ->
   let arg = Save.arg lexbuf in
   arg::parse_nargs (n-1) lexbuf
;;

let rec parse_args_opt pat lexbuf = match pat with
  [] -> []
| def::rest ->
   let arg = try Save.opt lexbuf with Save.NoOpt -> def in
   if !verbose > 2 then begin
     Printf.fprintf stderr "Parse opt : %s\n" arg
   end ;
   arg::parse_args_opt rest lexbuf
;;

let skip_opt lexbuf =
  let _ = try Save.opt lexbuf with Save.NoOpt -> "" in
  ()
and save_opt def lexbuf =
  let r = try Save.opt lexbuf with Save.NoOpt -> def in
  r
;;


let parse_args (popt,pat) lexbuf =
  let opts =  parse_args_opt popt lexbuf in
  let args =  parse_args_norm pat lexbuf in
  opts,args
;;

let to_image = ref true
;;

type format = Align of string | Inside of string
;;

let pretty_format = function
  Align s -> s
| Inside s -> "@{"^s^"}"
;;

let pretty_formats f =
  Array.iter (fun f -> prerr_string (pretty_format f) ; prerr_char '|') f
;;



let push s e = s := e:: !s
and pop s = match !s with
  [] -> failwith "Empty stack"
| e::rs -> s := rs ; e
;;

let if_level = ref 0
;;

exception IfFalse
;;

let verb_delim = ref (Char.chr 0)
;;
let cur_env = ref ""
and stack_env = ref []
and env_level = ref 0
and in_math = ref false
and stack_in_math = ref []
;;

let open_ital () = Html.open_mod (Style "I")
;;

let open_script_font () =
  let n = Html.get_fontsize () in
  Html.open_mod (Font (if n >= 1 then n-1 else n))
;;

let big_size () =  Html.open_mod (Font 7)
;;

(* Horizontal display *)
let display_arg  verbose =
  if verbose > 0 then
    "VALIGN=middle BORDER=1 CELLSPACING=0 CELLPADDING=0"
  else
    "VALIGN=middle CELLSPACING=0 CELLPADDING=0"
;;

let open_display () =
  if !display then begin
    if !verbose > 1 then
       prerr_endline "open display" ;
    Html.open_display (display_arg !verbose)
  end

and item_display () =
  if !display then begin
    Html.item_display ()
  end
;;

let close_display () =
  if !display then begin
    Html.close_display ()
  end

and close_pending_display () =
  if !display then begin
    Html.erase_display ();
  end
;;

(* vertical display *)
let open_vdisplay () =
  if !verbose > 1 then
    prerr_endline "open_vdisplay";
  Html.open_block "TABLE" (display_arg !verbose)

and close_vdisplay () =
  if !verbose > 1 then
    prerr_endline "close_vdisplay";
  Html.close_block "TABLE"

and open_vdisplay_row s =
  if !verbose > 1 then
    prerr_endline "open_vdisplay_row";
  Html.open_block "TR" "" ;
  Html.open_block "TD" s ;
  open_display ()

and close_vdisplay_row () =
  if !verbose > 1 then
    prerr_endline "close_vdisplay_row";
  close_display () ;
  Html.force_block "TD" "&nbsp;" ;
  Html.close_block "TR"
;;


let open_center () =  Html.open_block "DIV" "ALIGN=center"

and close_center () = Html.close_block "DIV"
;;



let iput_opt def arg =
  if arg = def then ()
  else begin
    Image.put "[" ;
    Image.put arg ;
    Image.put "]"
  end
;;

let iput_arg arg =
  Image.put "{" ;
  Image.put arg ;
  Image.put "}"
;;

let iput_newpage () =
  Image.put "\\hrule\n\\newpage\n\n" ;
  Html.put "<IMG SRC=\"" ;
  Html.put (Image.page ()) ;
  Html.put "\">"
;;

let unparse_args opts args =
  let rec do_rec op cl = function
    [] -> ""
  | s::rest -> op^s^cl^do_rec op cl rest in
  do_rec "[" "]" opts^do_rec "{" "}" args
;;

let scan_this lexfun s =
  if !verbose > 2 then begin
    Printf.fprintf stderr "scan_this : [%s]" s ;
    prerr_endline ""  
  end ;
  let lexer = Lexing.from_string s in
  let r = lexfun lexer in
  if !verbose > 2 then begin
    Printf.fprintf stderr "scan_this : over" ;
    prerr_endline ""
  end ;
  r
;;

let put_delim delim i =
  if !verbose > 2 then
    prerr_endline
     ("put_delim: ``"^delim^"'' ("^string_of_int i^")") ;
  if delim <> "." then begin
    Html.begin_item_display () ;
    Symb.put_delim Html.skip_line Html.put delim i ;
    Html.end_item_display ()
  end
;;

let default_format = Align "left"
;;

type in_table = Table | Border | NoTable | Tabbing
;;

let is_table = function
  (Table|Border) -> true
| _       -> false

and is_tabbing = function
  Tabbing -> true
| _ -> false
;;

let cur_format = ref [||]
and stack_format = ref []
and cur_col = ref 0
and stack_col = ref []
and in_table = ref NoTable
and border = ref false
and stack_table = ref []
;;

exception EndInside
;;
exception NoMulti
;;

let is_inside = function
  Inside _ -> true
| _ -> false

and as_inside = function
  Inside s -> s
| _        -> ""

and as_align = function
  Align s -> "ALIGN="^s
| _       -> failwith "as_align"

and as_colspan = function
  1 -> ""
| n -> " COLSPAN="^string_of_int n
;;

let get_col format i =
  let r = 
    if i >= Array.length format then default_format
    else format.(i) in
  if !verbose > 2 then begin
   Printf.fprintf stderr "get_col : %d: " i ;
   prerr_string (pretty_format r) ;
   prerr_char '\t' ;
   pretty_formats format ;
   prerr_newline ()
  end ;
  r
;;

let show_inside main format i =
  let t = ref i in
  begin try while true do
    begin match get_col format !t with
      Inside s ->
        Html.open_block "TD" "ALIGN=center";
        scan_this main s ;
        Html.force_block "TD" ""
    | _ -> raise EndInside
    end ;
    t := !t+1
  done with EndInside -> ()
  end ;
  !t
;;

let rec eat_cols n format i = match n with
  0 -> i
| _ ->
   if is_inside (get_col format i) then
     eat_cols n format (i+1)
   else
     eat_cols (n-1) format (i+1)
;;



let find_align format =
  let t = ref 0 in
  while is_inside (get_col format !t) do
    t := !t+1
  done ;
  !t
;;

let show_inside_multi main format i j =
  let rec show_rec i =
    if i >= j then ()
    else begin
      scan_this main (as_inside (get_col format i)) ;
      show_rec (i+1)
    end in
  show_rec i
;;

let close_multi = ref []
;;

let open_col main  =
  cur_col :=  show_inside main !cur_format !cur_col ;
  Html.open_block "TD" ("NOWRAP "^as_align (get_col !cur_format !cur_col)) ;
  open_display () ;
  push close_multi (0,[||])
;;


let open_row () =
  cur_col := 0 ;
  Html.open_block "TR" ""

and close_row () =
  Html.close_block "TR"
;;


let do_hline main =
  if !verbose > 2 then begin
    Printf.fprintf stderr "hline: %d %d" !cur_col (Array.length !cur_format) ;
    prerr_newline ()
  end ;
    close_pending_display () ;
    Html.erase_block "TD" ;
    Html.erase_block "TR" ;
    Html.open_block "TR" "" ;
    Html.open_block
      "TD"
      ("ALIGN=center HEIGHT=2"^
      as_colspan (Array.length !cur_format)) ;
    Html.put "<HR NOSHADE SIZE=2>" ;
    Html.close_block "TD" ;
    close_row () ;
    open_row () ;
    open_col main
;;

let change_td_pending args =
  close_pending_display () ;
  Html.change_block "TD" args  ;
  open_display ()
;;

let do_multi n format main =
  let i = find_align format
  and new_cur_col = eat_cols (n-1) !cur_format !cur_col in
  change_td_pending
    (as_align (get_col format i)^
    as_colspan (new_cur_col - !cur_col+1)) ;
  show_inside_multi main format 0 i ;
  push close_multi (i+1,format) ;
  cur_col := new_cur_col
;;


let close_col main =
  close_display ();
  cur_col := !cur_col + 1;
  Html.force_block "TD" "&nbsp;";
  let (i,format) = pop close_multi in
  show_inside_multi main format i (Array.length format-1) ;
  cur_col := show_inside main !cur_format !cur_col
;;


      
let new_env env lexfun lexbuf =
  push stack_env !cur_env   ;
  cur_env := env ;
  incr env_level ;
  if !verbose > 1 then prerr_endline "++" ;
  if !verbose > 0 then begin
    Location.print_pos () ;
    Printf.fprintf stderr "Begin : %s <%d>" env !env_level ;
    prerr_endline ""
  end ;
  lexfun lexbuf
;;

let error_env close_e open_e =
  raise (Failure (close_e^" closes : "^open_e))
;;

let close_env env endaction lexfun lexbuf =
  endaction () ;
  if !verbose > 0 then begin
    Printf.fprintf stderr "End : %s <%d>" env !env_level ;
    prerr_endline  ""
  end ;
  if env = !cur_env then begin  
    decr env_level ;
    if !verbose > 1 then prerr_endline "--" ;
    cur_env := pop stack_env ;
    lexfun lexbuf
  end else
    error_env env !cur_env
;;

let standard_sup_sub main what sup sub =
  try
    let _ = String.index sup '\\'
    and _ = String.index sub '\\' in
    if !display then begin
      item_display () ;
      open_vdisplay () ;
      open_script_font () ;
      if sup <> "" then begin
        open_vdisplay_row "" ;
        scan_this main sup ;
        close_vdisplay_row ()
      end ;           
      open_vdisplay_row "" ;
      what ();
      close_vdisplay_row () ;
      if sub <> "" then begin
        open_vdisplay_row "" ;
        scan_this main sub ;
        close_vdisplay_row ()
      end ;
        close_vdisplay () ;
        item_display ()
    end else begin
      what ();
      Html.open_group "SUB" ;
      scan_this main sub;
      Html.close_group ();
      Html.open_group "SUP" ;
      scan_this main sup;
      Html.close_group ()
    end with Not_found  -> begin
      what ();
      Html.put ("<SUB>") ;
      scan_this main sub ;
      Html.put ("</SUB>") ;
      Html.put ("<SUP>") ;
      scan_this main sup ;
      Html.put ("</SUP>")
    end
;;


let limit_sup_sub main what sup sub =
  if sup = "" && sub = "" then
    what ()
  else begin
    item_display () ;
    open_vdisplay () ;
    open_vdisplay_row "ALIGN=center" ;
    open_script_font () ;
    scan_this main sup ;
    close_vdisplay_row () ;
    open_vdisplay_row "ALIGN=center" ;
    what () ;
    close_vdisplay_row () ;
    open_vdisplay_row "ALIGN=center" ;
    open_script_font () ;
    scan_this main sub ;
    close_vdisplay_row () ;
    close_vdisplay () ;
    item_display ()
  end
;;

let int_sup_sub main what sup sub =
  if sup = "" && sub = "" then
    what ()
  else begin
    item_display () ;
    what () ;
    item_display () ;    
    open_vdisplay () ;
    open_vdisplay_row "ALIGN=left" ;
    open_script_font () ;
    scan_this main sup ;
    close_vdisplay_row () ;
    open_vdisplay_row "ALIGN=left" ;
    Html.put "&nbsp;" ;
    close_vdisplay_row () ;
    open_vdisplay_row "ALIGN=left" ;
    open_script_font () ;
    scan_this main sub ;
    close_vdisplay_row () ;
    close_vdisplay () ;
    item_display ()
  end
;;

}

rule  main = parse
   "%" ' '* ("BEGIN"|"begin") ' '+ ("IMAGE"|"image")  [^'\n']* '\n'
    {Image.dump image lexbuf}
|  '%' [^ '\n']* '\n'
   {if !verbose > 0 then
     Printf.fprintf stderr "Comment : %s" (lexeme lexbuf) ;
   main lexbuf}
(* included images *)
| ".PS\n" {Image.dump image lexbuf}
| "\\box\\graph" ' '*
    {if !verbose > 2 then prerr_endline "Graph" ;
    Image.put "\\box\\graph\n";
    iput_newpage () ;
    main lexbuf}
| "\\epsf" ' '*
    {let lxm = lexeme lexbuf in
    let arg = Save.arg lexbuf in
    let opt1 = save_opt "" lexbuf in
    let opt2 = save_opt "" lexbuf in
    Image.put lxm ;
    iput_arg arg ;
    iput_opt "" opt1 ;
    iput_opt "" opt2 ;
    Image.put_char '\n' ;
    iput_newpage () ;
    main lexbuf}
| "\\special"
   {let lxm = lexeme lexbuf in
   let arg = save_arg lexbuf in
   Image.put lxm ;
   iput_arg arg ;
   Image.put_char '\n' ;
   iput_newpage () ;
   main lexbuf}
| "\\documentstyle" 
    {let opts = parse_args_opt [""] lexbuf in
    let arg =  Save.arg lexbuf in
    Image.put "\\documentstyle" ;
    Image.put (unparse_args opts [arg]) ;
    Image.put "\n" ;
    main lexbuf}
(* Paragraphs *)
  | ("\\par" | "\n\n") '\n' *
    {if not !display then Html.par () ;
    main lexbuf }
| "\\input" | "\\include"
     {let filename = Save.arg lexbuf in
     if !verbose > 0 then
       Printf.fprintf stderr "input file : %s \n" filename ;
     let filename,input = open_tex filename in
     let buf = Lexing.from_channel input in
     Location.set filename buf ;
     new_env "*input" main buf ;
     Location.restore () ;
     close_env "*input" (fun () -> ()) main lexbuf}
(* subscripts and superscripts *)
  | ('_' | '^')
     {let lxm = lexeme lexbuf in
     let sup,sub = match lxm with
       "^" ->
         let sup = Save.arg lexbuf in
         let sub = get_sub lexbuf in
         sup,sub
     | _   ->
         let sub = Save.arg lexbuf in
         let sup = get_sup lexbuf in
         sup,sub in
     standard_sup_sub main (fun () -> ()) sup sub ;
     main lexbuf}
(* Math mode *)
| "$" | "$$"
     {let lxm = lexeme lexbuf in
     let dodo = lxm <> "$" in
     let math_env = if dodo then "*display" else "*math" in
     if !in_math then begin
       in_math := pop stack_in_math ;
       close_env
          math_env
          (if dodo then
             (fun () ->
               close_display () ;
               close_center () ;
               display := false)
           else if !display then close_display
           else close_group)
          main lexbuf
     end else begin
       push stack_in_math !in_math ;
       in_math := true ;
       let lexfun lb =
         if dodo then begin
           display  := true ;
           open_center() ;
           open_display () ;
           open_ital ()
         end else begin
           if !display then open_display ()
           else open_group "I"
         end;
         main lb in
       new_env math_env lexfun lexbuf
     end}
|   ("\\mbox" | "\\hbox" | "\\vbox" | "\\makebox") [^'{']* '{'
    {if !in_math then begin
       push stack_in_math !in_math ;
       in_math := false ;
       let lexfun lb =
         if !display then begin
           item_display () ; open_group "" ; open_display ()
         end else
           open_group "" ;
         open_mod (Style "RM") ;
         main lb in
       new_env "*mbox" lexfun lexbuf
    end else begin
      new_env " " (fun lb -> open_group "" ; main lb) lexbuf
    end}
(* Definitions of  simple macros *)
  | "\\def" [' ''\n']* '\\' (['A'-'Z' 'a'-'z']+ | [^ 'A'-'Z' 'a'-'z']) ' '*
     {let lxm = lexeme lexbuf in
     let name = name_extract lxm in
     let args_pat = defargs lexbuf in
     let body = defbody lexbuf in
     if (!to_image) then
       Image.put
         ("\\def"^name^
         (List.fold_right (fun s r -> s^r) args_pat ("{"^body^"}\n"))) ;
     def_macro_pat name ([],args_pat) [Subst body] ;
     main lexbuf
    }
  | ("\\renewcommand" | "\\newcommand") ' '*
    {let name = Save.arg lexbuf in
    let nargs = parse_args_opt ["0" ; ""] lexbuf in
    let body = Save.arg lexbuf in
    let nargs = Char.code (String.get (List.hd nargs) 0) - Char.code '0' in
    let rec make_defargs = function
      0 -> ""
    | i -> make_defargs (i-1)^"#"^string_of_int i in
    scan_this main ("\\def"^name^make_defargs nargs^unparse_args [] [body]) ;
    main lexbuf}
  | "\\newenvironment" ' '*
     {let args = parse_nargs 3 lexbuf in
     let name,body1,body2 = match args with
       [n ; b1 ; b2 ] -> n,b1,b2
     | _              -> failwith "Newenvironment" in
     def_env name [Subst body1] [Subst body2];     
     main lexbuf}
  | "\\newtheorem" | "\\renewtheorem"
      {let lxm = lexeme lexbuf in
      let name = Save.arg lexbuf in
      skip_opt lexbuf ;
      let caption = Save.arg lexbuf in
      skip_opt lexbuf ;
      Image.put (lxm^unparse_args [] [name; caption]^"\n") ;
      to_image := false ;
      scan_this main ("\\def\\"^name^"{\\par{\\bf "^caption^"}}") ;
      scan_this main ("\\def\\end"^name^"{}") ;
      to_image := true ;
      main lexbuf}
  | "\\let\\" (['A'-'Z' 'a'-'z']+ '*'? | [^ 'A'-'Z' 'a'-'z']) '='
     {let lxm = lexeme lexbuf in
     let name = String.sub lxm 4 (String.length lxm - 5) in
     let alt = Save.arg lexbuf in
     begin try
       let nargs,body = find_macro alt in
       def_macro_pat name nargs body
     with Not_found -> () end ;
     Image.put lxm ;
     Image.put alt ;
     main lexbuf}
(* Raw html, latex only *)
| "\\begin" ' '* "{rawhtml}"
     {rawhtml lexbuf; main lexbuf }
| "\\begin" ' '* "{latexonly}"
     { latexonly lexbuf; main lexbuf }

(* tabbing *)
| "\\begin" ' '* "{tabbing}"
   {let lexfun lb =
     Html.open_block "TABLE" "CELLSPACING=0 CELLPADDING=0" ;
     Html.delay (fun _ -> ()) ;
     Html.open_block "TR" "" ;
     Html.open_block "TD" "" ;
     main lb in
   push stack_table !in_table ;
   in_table := Tabbing ;
   new_env "tabbing" lexfun lexbuf}
| "\\end" ' '* "{tabbing}"
   {Html.close_block "TD" ;
   Html.close_block "TR" ;
   let _ = Html.flush () in
   Html.close_block "TABLE" ;
   in_table := pop stack_table ;
   close_env "tabbing" (fun () -> ()) main lexbuf}
 | [' ''\n']* ("\\>" | "\\=")  [' ''\n']*
    {if is_tabbing !in_table then begin
      Html.force_block "TD" "&nbsp;";
      Html.open_block "TD" ""
    end ;
    main lexbuf}
 |  [' ''\n']* "\\kill"  [' ''\n']*
    {if is_tabbing !in_table then begin
      Html.force_block "TD" "&nbsp;";
      Html.close_block "TR" ;
      Html.forget () ;
      Html.delay (fun _ -> ()) ;
      Html.open_block "TR" "" ;
      Html.open_block "TD" ""
    end ;
    main lexbuf}
(* tables and array *)
| "\\begin" ' '* ("{tabular}" | "{array}")
    {
    let lxm = lexeme lexbuf  in
    let env = env_extract lxm in
    border := false ;
    skip_opt lexbuf ;
    let format = Array.of_list (scan_this tformat (Save.arg lexbuf)) in
    push stack_format !cur_format ;
    push stack_col !cur_col ;
    push stack_table !in_table ;
    cur_format := format ;
    in_table := (if !border then Border else Table);
    let lexfun lb =
      item_display () ;
      if !border then
        Html.open_block "TABLE" "BORDER=1 CELLSPACING=0 CELLPADDING=1"
      else
        Html.open_block "TABLE" "CELLSPACING=2 CELLPADDING=0" ;
      open_row() ;
      open_col main ;
      main lb in
    new_env env lexfun lexbuf}
  | "\\\\"? [' ' '\n']* "\\end" ' '* ("{tabular}" | "{array}")
      {let lxm = lexeme lexbuf in
      let env = env_extract lxm in
      if env = !cur_env then begin
       close_display () ;
       Html.close_block "TD" ;
       Html.close_block "TR" ;
       Html.close_block "TABLE" ;
       in_table := pop stack_table ;
       cur_col := pop stack_col ;
       cur_format := pop stack_format ;
       close_env env
         item_display
         main lexbuf
      end else begin
        error_env env !cur_env ;
        main lexbuf
      end}
  | "\\left"
      {if !display then begin
        end_item_display () ;
        let delim = Save.arg lexbuf in
        Html.delay (put_delim delim) ;
        begin_item_display () ;
        scan_this main "{"
      end ;     
      main lexbuf}
  | "\\right"
      {if !display then begin
        let delim = Save.arg lexbuf in
        scan_this main "}" ;
        end_item_display () ;
        let vsize = Html.flush () in
        put_delim delim vsize ;
        begin_item_display ()
      end ;
      main lexbuf}
  | "\\over"
      {if !display then begin
        let mods = Html.insert_vdisplay
          (fun () ->
             open_vdisplay () ;
             open_vdisplay_row "ALIGN=center") in
        close_vdisplay_row () ;
        open_vdisplay_row "ALIGN=center" ;
        Html.open_mods mods ;
        Html.put "<HR NOSHADE SIZE=2>" ;
        close_vdisplay_row () ;
        open_vdisplay_row "ALIGN=center" ;
        Html.open_mods mods ;
        Html.freeze (fun () -> close_vdisplay_row () ; close_vdisplay ()) ;
        main lexbuf        
      end else begin
        Html.put "/" ;
        main lexbuf
      end}
  |  [' ''\n']* "\\hline" [' ''\n']* ("\\\\"  [' ''\n']*)?
     {if !in_table = Table then
       do_hline main ;
     main lexbuf}
  | [' ''\n']* "&"  [' ''\n']*
     {if is_table !in_table  then begin
        close_col main ; 
        open_col main
     end ;
     main lexbuf}
  | ['\n'' ']* "\\\\"
      {let _ = parse_args_opt [""] lexbuf in
      if is_table !in_table  then begin
         close_col main ; close_row () ;
         open_row () ; open_col main
      end else if is_tabbing !in_table then begin
        Html.force_block "TD" "&nbsp;";
        Html.close_block "TR" ;
        Html.flush () ;
        Html.delay (fun _ -> ()) ;
        Html.open_block "TR" "" ;
        Html.open_block "TD" ""
      end else begin
        Html.skip_line ()
      end ;
      skip_blanks_main lexbuf}
  | ['\n'' ']* "\\multicolumn" 
      {let n = Save.arg lexbuf in      
      let format = scan_this tformat (Save.arg lexbuf) in
      let n = try 
        int_of_string n
        with Failure _ -> raise (Failure "multicolumn") in
      do_multi n (Array.of_list format) main ;
      main lexbuf}
      
      
(* environments *)
|   "\\begin" " "* "{" ['a'-'z']+ '*'?"}"
    {let lxm = lexeme lexbuf in
    let env = env_extract lxm in
    if env = "document" then begin
      Html.set_out !out_file
    end ;
    let lexfun = match env with
      "program" | "verbatim" ->
         (fun lexbuf -> Html.open_block "PRE" "" ; verbenv lexbuf)
    | _ ->
      let macro = "\\"^env in
      let pat,_ = find_macro macro in
      let opt,args = parse_args pat lexbuf in
      let buff = macro^unparse_args opt args in      
      (fun lb ->
         if env <> "document" then Html.open_group "" ;
         scan_this main buff ;
         if env = "document" then Html.open_par () ;
         main lb) in
    new_env env lexfun lexbuf}
|  "\\end" " " * "{" ['a'-'z']+ '*'? "}"
    {let lxm = lexeme lexbuf in
    let env = env_extract lxm in
    close_env
      env
        (fun () ->
          if env = "document" then Html.close_par () ;
          scan_this main ("\\end"^env) ;
          if env <> "document" then Html.close_group ())
      main lexbuf}
| ("\\prog" | "\\verb") _
   {let lxm = lexeme lexbuf in
   verb_delim := String.get lxm (String.length lxm-1) ;
   Html.open_group "CODE" ;
   new_env "*verb" inverb lexbuf}
| "\\item" ' '*
    {let arg = List.hd (parse_args_opt [""] lexbuf) in
    Html.item (fun () -> scan_this main arg) ;
    main lexbuf}
(* Bibliographies *)
  | "\\cite{"
    {let args = Save.cite_args lexbuf in
    open_group "CITE" ;
    put_char '[' ;
    let rec do_rec = function
      [] -> ()
    | [x] -> Html.loc_ref x
    | x::rest ->
        Html.loc_ref x ;
        put ", " ;
        do_rec rest in
    do_rec args ;
    put_char ']' ;
    close_group () ;
    main lexbuf
    }
(* Ignore font definitions ... *)
  | "\\font" "\\" ['A'-'Z' 'a'-'z']+ ' '* '='? ' '* ['a'-'z' 'A'-'Z' '0'-'9']+
      {main lexbuf}
(* conditionals *)
  | "\\newif"
      {let arg = Save.arg lexbuf in
      newif arg ;
      main lexbuf}
  | "\\else"  {skip_false lexbuf}
  | "\\fi"    {main lexbuf}


(* General case for commands *)
  | "\\" '@'? ((['A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'])
      {let lxm = lexeme lexbuf in
      let rec exec stack = function
        [] -> ()
      | i::rest -> begin match i with
            Print str -> Html.put str
          | Print_arg i ->
              let lex_one = Lexing.from_string stack.(i) in
              main lex_one
          | Print_fun (f,i) -> scan_this main (f stack.(i))
          | Print_saved -> Html.put !reg
          | Save_arg i -> reg := stack.(i)
          | New_count i -> Counter.def_counter stack.(i)
          | Set_count (i,j) ->
              let x =
                try int_of_string stack.(j)
                with Failure _ -> 0 in
              Counter.set_counter stack.(i) x
          | Add_count (i,j) ->
              let x =
                try int_of_string stack.(j)
                with Failure _ -> 0
              and c = Counter.find_counter stack.(i) in
              c := !c + x
          | Print_count (f,i) ->
              let c = Counter.find_counter stack.(i) in
              Html.put (f !c)
          | Test cell ->
              if not !cell then raise IfFalse
              else
                if !verbose > 2 then
                  prerr_endline "Seen if as true"
          | SetTest (cell,b) -> cell := b
          | Env s -> Html.open_mod s
          | Open (s,args) -> Html.open_block s args
          | Close s       -> Html.close_block s
          | ItemDisplay   -> item_display ()
          | Subst body ->
            if !verbose > 2 then
              Printf.fprintf stderr "user macro : %s\n" body ;
            let lex_one = Lexing.from_string body in
            let body_instance = Subst.subst lex_one stack in
            if !verbose > 2 then
              Printf.fprintf stderr "subst: %s\n" body_instance ;
            scan_this main body_instance
          | IfCond (b,t,f) ->
             if !verbose > 2 then
               prerr_endline ("IfCond: "^if !b then "true" else "false") ;
             if !b then exec stack t else exec stack f
          end ;
        exec stack rest in

        let name = name_extract lxm in
        let pat,body = find_macro name in
        if Latexmacros.limit name || Latexmacros.big name then begin
           let do_what = (fun () -> exec [||] body) in
           let sup,sub = get_sup_sub lexbuf in
           if !display && Latexmacros.limit name then
             limit_sup_sub main do_what sup sub
           else if !display &&  Latexmacros.int name then
             int_sup_sub main do_what sup sub
           else
             standard_sup_sub main do_what sup sub ;
           main lexbuf
        end else begin
          let opts,args = parse_args pat lexbuf in
          let stack = Array.of_list (opts@args) in
          if !verbose > 2 then begin
            Printf.fprintf stderr "macro: %s\n"  name ;
            for i = 0 to Array.length stack-1 do
              Printf.fprintf stderr "\t#%d = %s\n" (i+1) stack.(i)
            done
          end ;
          try
            exec stack body ;
            if (!verbose > 2) then prerr_endline ("Cont after macro"^name) ;
            if not !in_math &&
               ((pat = ([],[])) || Latexmacros.invisible name) then
              skip_blanks_main lexbuf
            else
              main lexbuf
          with Invalid_argument "vect_item" as x ->
              Printf.fprintf stderr "Bad arg in %s\n" name ;
              raise (Failure "get_arg")
          | IfFalse -> begin
             if (!verbose > 2) then
               prerr_endline ("Cont after iffalse:"^name) ;
             skip_false lexbuf
          end
        end}

| "`\\" [^'a'-'z' 'A'-'Z' '0'-'9']
    {Html.put_char (lexeme_char lexbuf 2) ; main lexbuf}
| "<"         { Html.put "&lt;"; main lexbuf }
| ">"         { Html.put "&gt;"; main lexbuf }
| "~"         { Html.put "&nbsp;"; main lexbuf }
| "\\{" {Html.put_char '{' ; main lexbuf}
| "\\}" {Html.put_char '}' ; main lexbuf}
| "{"
    {if !verbose > 2 then prerr_endline "Open brace" ;
    if !display then begin
      item_display () ; Html.open_group "" ; open_display ()
    end else
      Html.open_group "" ;
    new_env " " main lexbuf}
| "}"
    {if !verbose > 2 then prerr_endline "Close brace" ;
    let env = " " in
    let close_fun = if !display then
      (fun () -> close_display () ; close_group () ; item_display ())
    else close_group in
    if env = !cur_env then
      close_env env close_fun  main lexbuf
    else if !cur_env = "*mbox" then begin
      in_math := pop stack_in_math ;
      close_env
        !cur_env close_fun main lexbuf
    end else begin
      error_env env !cur_env ;
      main lexbuf
    end}
| eof
   {if !verbose > 1 then Printf.fprintf stderr "Eof\n" ; ()}
| '\n'
  {if not !display then
    Html.put_char '\n' ;
  main lexbuf}
| _ 
   {let lxm = lexeme_char lexbuf 0 in
   Html.put_char lxm ; main lexbuf}

and rawhtml = parse
    "\\end{rawhtml}" { () }
  | _           { Html.put_char(lexeme_char lexbuf 0); rawhtml lexbuf }

and latexonly = parse
    "\\end{latexonly}" { () }
  | _           { latexonly lexbuf }

and defargs = parse 
  '#' ['1'-'9'] | [^'#' '{']+
    {let lxm = lexeme lexbuf in
    lxm::defargs lexbuf}
| "" {[]}

and defbody = parse "" {Save.arg lexbuf}

and verbenv = parse
  "\\end" " " * "{" ['a'-'z'] + "}"
    {let lxm = lexeme lexbuf in
    let env = env_extract lxm in
    if env = !cur_env then
      close_env env (fun () -> Html.close_block "PRE")
        main lexbuf
    else begin
      Html.put lxm ;
      verbenv lexbuf
    end}
| "\\esc" ' '*
    {if !cur_env <> "program" then begin
      Html.put (lexeme lexbuf)
    end else begin
      let arg = Save.arg lexbuf in
      scan_this main ("{"^arg^"}")
    end ;
    verbenv lexbuf}
| "<"         { Html.put "&lt;"; verbenv lexbuf }
| ">"         { Html.put "&gt;"; verbenv lexbuf }
| "~"         { Html.put "&nbsp;"; verbenv lexbuf }
| _  { Html.put_char (lexeme_char lexbuf 0) ; verbenv lexbuf}

and inverb = parse
  "<"         { Html.put "&lt;"; inverb lexbuf }
| ">"         { Html.put "&gt;"; inverb lexbuf }
| _
  {let c = lexeme_char lexbuf 0 in
  if c = !verb_delim then begin
    Html.close_group () ;
    close_env "*verb" (fun () -> ())  main lexbuf
  end else begin
    Html.put_char c ;
    inverb lexbuf
  end}

and image = parse
  ".PE\n"
     {Image.put_char '\n' ; Image.close_image  () ; main lexbuf}
|  "%" ' '* ("END"|"end") ' '+ ("IMAGE"|"image")  [^'\n']* '\n'
     {Image.put_char '\n' ; Image.close_image  () ;
     iput_newpage () ;
     main lexbuf}
| [^'\n']*'\n'
     {let s = lexeme lexbuf in
     Image.put s ;
     image lexbuf}

and tformat = parse
  'c' {Align "center"::tformat lexbuf}
| 'l' {Align "left"::tformat lexbuf}
| 'r' {Align "right"::tformat lexbuf}
| '|' {border := true ; tformat lexbuf}
| '@'
    {let inside = Save.arg lexbuf in
    Inside inside::tformat lexbuf}
| _   {tformat lexbuf}
| eof {[]}

and skip_blanks_main = parse
  ' ' * '\n'? ' '* {main lexbuf}
| eof              {main lexbuf}

and skip_blanks = parse 
  [' ']* '\n'? [' ']* {()}
| eof {()}


and get_sup_sub = parse
  '^'
    {let sup = Save.arg lexbuf in
    sup,get_sub lexbuf}
| '_'
    {let sub = Save.arg lexbuf in
    get_sup lexbuf,sub}
| "" {("","")}

and get_sup = parse
  '^'  {Save.arg lexbuf}
| ""   {""}

and get_sub = parse
  '_'  {Save.arg lexbuf}
| ""   {""}

and skip_false = parse
  "\\if" ['a'-'z' 'A'-'Z']+
     {if_level := !if_level + 1 ;
     skip_false lexbuf}
| "\\else" ['a'-'z' 'A'-'Z']+
     {skip_false lexbuf}
| "\\else"
     {if !if_level = 0 then main lexbuf
     else skip_false lexbuf}
| "\\fi" ['a'-'z' 'A'-'Z']+
     {skip_false lexbuf}
| "\\fi"
     {if !if_level = 0 then main lexbuf
     else begin
       if_level := !if_level -1 ;
       main lexbuf
     end}
| _ {skip_false lexbuf}

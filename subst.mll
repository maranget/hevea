(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: subst.mll,v 1.20 2012-06-05 14:55:39 maranget Exp $           *)
(***********************************************************************)
{
open Printf
open Misc
open Lexstate

let subst_buff = Out.create_buff ()
;;
} 

let command_name =
 '\\' ((['@''A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'] | "\\*")


rule subst expn = parse
| '#' ['1'-'9'] as lxm
    {if is_plain '#' then begin
      let i = Char.code (lxm.[1]) - Char.code '1' in
      scan_arg
        (fun arg -> scan_this_arg_list (subst expn) arg) i
    end else
      Out.put subst_buff lxm ;
    subst expn lexbuf}
| '#' '#'
    {if is_plain '#' then
      Out.put_char subst_buff '#'
    else
      Out.put subst_buff "##" ;
    subst expn lexbuf}
|  "\\#" | '\\' | [^'\\' '#']+
    {Out.blit subst_buff lexbuf ; subst expn lexbuf}
| "\\@print" as lxm
    {Save.start_echo () ;
    let _ = Save.arg lexbuf in
    let real_arg = Save.get_echo () in
    Out.put subst_buff lxm ;
    Out.put subst_buff real_arg ;
    subst expn lexbuf}
|  command_name as cmd
    {
     if expn then begin
       try
         let pat,body = Latexmacros.find_fail cmd in
         begin match body with
         | Subst _ when not (Latexmacros.get_saved_macro cmd) ->
             if !verbose > 2 then eprintf "EXPAND: %s\n" cmd ;
             let args = make_stack cmd pat lexbuf in
             Out.put_char subst_buff '{' ;
             scan_body
               (function
                 | Subst body -> scan_this_list (subst expn) body
                 | _ -> assert false)
               body args ;
              Out.put_char subst_buff '}'
         | _ -> Out.put subst_buff cmd
         end
       with Latexmacros.Failed -> Out.put subst_buff cmd
     end else begin
       Out.put subst_buff cmd
     end ;
    subst expn lexbuf}
|  eof {()}
| "" {raise (Error "Empty lexeme in subst")}

and do_translate = parse
| "\\@print" as lxm
    {fun f ->
      Save.start_echo () ;
      let _ = Save.arg lexbuf in
      let real_arg = Save.get_echo () in
      Out.put subst_buff lxm ;
      Out.put subst_buff real_arg ;
      do_translate lexbuf f}
| command_name
    {fun f ->
      Out.blit subst_buff lexbuf ;
      do_translate lexbuf f}
| _ as lxm
    {fun f ->
      Out.put_char subst_buff (f lxm) ;
      do_translate lexbuf f}
| eof {fun _ -> Out.to_string subst_buff}

{

let sharp_inside s =
  try ignore (String.index s '#') ; true with Not_found -> false

let do_do_subst_this expn ({arg=arg ; subst=env} as x) =
  if not (is_top env) && sharp_inside arg then begin
    if !verbose > 1 then begin
      Printf.fprintf stderr "subst_this : [%s]\n" arg ;
      prerr_args ()
    end ;
    let _ = scan_this_arg (subst expn) x in
    let r = Out.to_string subst_buff in
    if !verbose > 1 then
      prerr_endline ("subst_this ["^arg^"] = "^r);
    r
  end else
    arg

let sharp_inside_list xs = List.exists sharp_inside xs

let do_do_subst_this_list expn ({arg=xs ; subst=env} as x) =
  if not (is_top env) && sharp_inside_list xs then begin
    if !verbose > 1 then begin
      fprintf stderr "subst_this_list : [%a]\n" Lexstate.pretty_body xs ;
      prerr_args ()
    end ;
    let _ = scan_this_arg_list (subst expn) x in
    let r = Out.to_list subst_buff in
    if !verbose > 1 then
      eprintf "subst_this_list [%a] = [%a]\n"
        Lexstate.pretty_body xs
        Lexstate.pretty_body r ;
    r
  end else
    xs

let do_subst_this_list x =
  String.concat "" (do_do_subst_this_list false x)
let do_subst_this x = do_do_subst_this false x

let subst_list {arg=args ; subst=env} =
  List.map
    (fun arg -> do_subst_this {arg=arg; subst=env})
    args

let subst_this s = do_subst_this (mkarg s (get_subst ()))
let subst_arg lexbuf = do_subst_this (save_arg lexbuf)  
and subst_opt def lexbuf = do_do_subst_this_list false  (save_opt def lexbuf)  

let subst_body lexbuf = do_do_subst_this_list false (save_body lexbuf)
let subst_arg_list lexbuf = do_do_subst_this_list false (save_body lexbuf)

let subst_expn_arg lexbuf = do_do_subst_this true (save_arg lexbuf)  
let subst_expn_body lexbuf = do_do_subst_this_list true (save_body lexbuf)

let translate f s =
  let lexbuf = MyLexing.from_string s in
  do_translate lexbuf f

let lowercase s = translate Char.lowercase s
and uppercase s = translate Char.uppercase s

} 

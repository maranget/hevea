(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: subst.mll,v 1.17 2006-02-01 17:34:17 maranget Exp $           *)
(***********************************************************************)
{
open Misc
open Lexstate
open Lexing


let subst_buff = Out.create_buff ()
;;
} 

let command_name =
 '\\' ((['@''A'-'Z' 'a'-'z']+ '*'?) | [^ 'A'-'Z' 'a'-'z'] | "\\*")

rule subst = parse
| '#' ['1'-'9'] as lxm
    {if is_plain '#' then begin
      let i = Char.code (lxm.[1]) - Char.code '1' in
      scan_arg
        (fun arg -> scan_this_arg subst arg) i
    end else
      Out.put subst_buff lxm ;
    subst lexbuf}
| '#' '#'
    {if is_plain '#' then
      Out.put_char subst_buff '#'
    else
      Out.put subst_buff "##" ;
    subst lexbuf}
|  "\\#" | '\\' | [^'\\' '#']+
    {Out.blit subst_buff lexbuf ; subst lexbuf}
| "\\@print" as lxm
    {Save.start_echo () ;
    let _ = Save.arg lexbuf in
    let real_arg = Save.get_echo () in
    Out.put subst_buff lxm ;
    Out.put subst_buff real_arg ;
    subst lexbuf}
|  command_name
    {Out.blit subst_buff lexbuf ;
    subst lexbuf}
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

let do_subst_this ({arg=arg ; subst=env} as x) =
  if not (is_top env) then begin
    try
      let _ = String.index arg '#' in
      if !verbose > 1 then begin
        Printf.fprintf stderr "subst_this : [%s]\n" arg ;
        prerr_args ()
      end ;
      let _ = scan_this_arg subst x in
      let r = Out.to_string subst_buff in
      if !verbose > 1 then
        prerr_endline ("subst_this ["^arg^"] = "^r);
      r
    with Not_found -> arg
  end else
    arg
;;

let subst_this s = do_subst_this (mkarg s (get_subst ()))

let subst_arg lexbuf = do_subst_this (save_arg lexbuf)  
and subst_opt def lexbuf = do_subst_this (save_opt def lexbuf)  
let subst_body = subst_arg

let translate f s =
  let lexbuf = Lexing.from_string s in
  do_translate lexbuf f

let lowercase s = translate Char.lowercase s
and uppercase s = translate Char.uppercase s

} 

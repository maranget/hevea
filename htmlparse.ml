(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: htmlparse.ml,v 1.8 2005-06-24 08:34:23 maranget Exp $         *)
(***********************************************************************)
open Lexeme
open Htmllex
open Tree

exception Error of string

let error msg lb = 
  raise (Error msg)
;;

let buff = ref None

let next_token lexbuf = match !buff with
| Some tok -> buff := None ; tok
| None -> Htmllex.next_token lexbuf

and put_back lexbuf tok = match !buff with
| None -> buff := Some tok
| _    -> error "Put back" lexbuf

let txt_buff = Buff.create ()

let rec to_close tag lb = match next_token lb with
| Close (t,txt) as tok when t=tag -> tok
| Open (t,_,txt) when t=tag ->
    Buff.put txt_buff txt ;
    Buff.put txt_buff (Htmllex.to_string (to_close tag lb)) ;
    to_close tag lb
| Eof -> error ("Eof in to_close") lb
| tok ->
    Buff.put txt_buff (Htmllex.to_string tok);
    to_close tag lb
    
let rec tree lexbuf =
  match next_token lexbuf with
  | (Eof|Close (_,_)) as tok-> put_back lexbuf tok ; None
  | Open (STYLE,_,txt) ->
      let otxt = txt
      and ctxt = Htmllex.to_string (to_close STYLE lexbuf) in
      let txt = Buff.to_string txt_buff in
(*
      let css = Htmllex.styles (Lexing.from_string txt) in
      Pp.styles stderr css ;
*)
      Some (Text (otxt^txt^ctxt))
  | Open (SCRIPT,_,txt) ->
      Buff.put txt_buff txt ;
      Buff.put txt_buff (Htmllex.to_string (to_close SCRIPT lexbuf)) ;
      Some (Text (Buff.to_string txt_buff))
  | Open (tag,attrs,txt) ->
      let fils = trees lexbuf in
      begin match next_token lexbuf with
      | Close (ctag,ctxt) when tag=ctag ->          
          Some
            (match tag with
            | A ->
                ONode (txt,ctxt,fils)
            | _ ->
              Node
               ({tag=tag ; attrs=attrs ; txt=txt ; ctxt=ctxt},fils))
      | tok ->
          error (Htmllex.to_string tok ^ " closes "^txt) lexbuf
      end
  | Lexeme.Text txt -> Some (Text txt)
  | Lexeme.Blanks txt -> Some (Blanks txt)

and trees lexbuf = match tree lexbuf with
| None -> []
| Some t -> t::trees lexbuf

let rec do_main lexbuf = match tree lexbuf with
| None ->
    begin match next_token lexbuf with
    | Eof ->  []
    | tok  -> error ("Unexpected " ^ to_string tok) lexbuf
    end
| Some (Text _ as last) -> [last]
| Some t -> t :: do_main lexbuf

let reset () =  Buff.reset txt_buff 

let main lexbuf =
  try
    do_main lexbuf
  with
  | e -> reset () ; raise e


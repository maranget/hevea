(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: htmlparse.ml,v 1.11 2008-01-22 18:08:37 maranget Exp $         *)
(***********************************************************************)

open Lexeme
open Htmllex
open Tree

exception Error of string

let error msg _lb = raise (Error msg)
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
| Close (t,_) as tok when t=tag -> tok
| Open (t,_,txt) when t=tag ->
    Buff.put txt_buff txt ;
    Buff.put txt_buff (Htmllex.to_string (to_close tag lb)) ;
    to_close tag lb
| Eof -> error ("Eof in to_close") lb
| tok ->
    Buff.put txt_buff (Htmllex.to_string tok);
    to_close tag lb
    
let rec tree cls lexbuf =
  match next_token lexbuf with
  | (Eof|Close (_,_)) as tok-> put_back lexbuf tok ; None
  | Open (STYLE,_,txt) ->
      let otxt = txt
      and ctxt = Htmllex.to_string (to_close STYLE lexbuf) in
      let txt = Buff.to_string txt_buff in
      let txt =	match cls with
      | None -> txt
      | Some cls ->
	  let css = Htmllex.styles (MyLexing.from_string txt) in
          let buff = Buff.create () in
          Buff.put_char buff '\n' ;
          List.iter
            (fun cl -> match cl with
            | Css.Other txt ->
                Buff.put buff txt ;
                Buff.put_char buff '\n'
            | Css.Class (name, addname, txt) ->
                if Emisc.Strings.mem name cls then begin
                  Buff.put_char buff '.' ;
                  Buff.put buff name ;
                  begin match addname with
                  | None -> ()
                  | Some n -> 
                      Buff.put_char buff ' ' ;
                      Buff.put buff n
                  end ;
                  Buff.put buff txt ;
                  Buff.put_char buff '\n'
                end)
            css ;
          Buff.to_string buff in
      Some (Text (otxt^txt^ctxt))
  | Open (SCRIPT,_,txt) ->
      Buff.put txt_buff txt ;
      Buff.put txt_buff (Htmllex.to_string (to_close SCRIPT lexbuf)) ;
      Some (Text (Buff.to_string txt_buff))
  | Open (tag,attrs,txt) ->
      let fils = trees cls lexbuf in
      begin match next_token lexbuf with
      | Close (ctag,ctxt) when tag=ctag ->          
          Some
            (match tag with
            | A|SUP|SUB ->
                ONode (txt,ctxt,fils)
            | _ ->
              Node
               ({tag=tag ; attrs=attrs ; txt=txt ; ctxt=ctxt},fils))
      | tok ->
          error (Htmllex.to_string tok ^ " closes "^txt) lexbuf
      end
  | Lexeme.Text txt -> Some (Text txt)
  | Lexeme.Blanks txt -> Some (Blanks txt)

and trees cls lexbuf = match tree cls lexbuf with
| None -> []
| Some t -> t::trees cls lexbuf

let rec do_main cls lexbuf = match tree cls lexbuf with
| None ->
    begin match next_token lexbuf with
    | Eof ->  []
    | tok  -> error ("Unexpected " ^ to_string tok) lexbuf
    end
| Some (Text _ as last) -> [last]
| Some t -> t :: do_main cls lexbuf

let reset () =  Buff.reset txt_buff 

let main cls lexbuf =
  try
    do_main cls lexbuf
  with
  | e -> reset () ; raise e


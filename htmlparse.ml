(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: htmlparse.ml,v 1.3 2001-05-25 09:20:45 maranget Exp $"            *)
(***********************************************************************)
open Lexeme
open Htmllex
open Tree

exception Error of string

let error msg lb =  raise (Error msg)
;;

let buff = ref None

let next_token lexbuf = match !buff with
| Some tok -> buff := None ; tok
| None -> Htmllex.next_token lexbuf

and put_back lexbuf tok = match !buff with
| None -> buff := Some tok
| _    -> error "Put back" lexbuf

let rec tree lexbuf =
  match next_token lexbuf with
  | (Eof|Close (_,_)) as tok-> put_back lexbuf tok ; None
  | Open (tag,attrs,txt) ->
      let fils = trees lexbuf in
      begin match next_token lexbuf with
      | Close (ctag,ctxt) when tag=ctag ->
          Some
            (Node
               ({tag=tag ; attrs=attrs ; txt=txt ; ctxt=ctxt},fils))
      | tok ->
          error (Htmllex.to_string tok ^ " closes "^txt) lexbuf
      end
  | Lexeme.Text txt -> Some (Text txt)
  | Lexeme.Blanks txt -> Some (Blanks txt)

and trees lexbuf = match tree lexbuf with
| None -> []
| Some t -> t::trees lexbuf

let rec main lexbuf = match tree lexbuf with
| None ->
    begin match next_token lexbuf with
    | Eof ->  []
    | tok  -> error ("Unexpected " ^ to_string tok) lexbuf
    end
| Some (Text _ as last) -> [last]
| Some t -> t :: main lexbuf
  
    


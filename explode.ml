(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: explode.ml,v 1.4 2001-05-25 12:37:21 maranget Exp $           *)
(***********************************************************************)

open Lexeme
open Htmltext
open Tree

let of_styles env r = match env with
| [] -> r
| _  -> Node (env,[r])


let rec tree env t k = match t with
| Text s ->
    of_styles env (Text s)::k
| Blanks s ->
    of_styles
      (List.filter (fun s -> not (Htmltext.blanksNeutral s)) env)
      (Blanks s)::
    k
| Node (s,ts) ->
    if s.Tree.tag = A then
      ONode
        (s.Tree.txt, s.Tree.ctxt,
         List.fold_right (tree env) ts [])::k
    else
      begin try
        let new_env = Htmltext.add_style s env in
        List.fold_right (tree new_env) ts k
      with
      | Split (s,env) ->
          let ts = List.fold_right (tree []) ts [] in
          let now =
            if Util.is_blanks ts then
              (List.filter (fun s -> not (Htmltext.blanksNeutral s)) env)
            else
              env in
          match ts with
          | [] -> k
          | _ ->
              of_styles now (Node ([s],ts))::k
      end
| ONode (so,sc,ts) -> assert false

let trees ts =  List.fold_right (tree []) ts []

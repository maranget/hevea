(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: pp.ml,v 1.2 2001-05-25 09:20:48 maranget Exp $"            *)
(***********************************************************************)
open Printf 
open Tree

let  potag chan ({txt=txt} as s)= output_string chan txt ; s

let rec pctag chan  {ctxt=txt} = output_string chan txt


let rec tree po pc chan = function
  | Text txt | Blanks txt -> output_string chan txt
  | Node (styles, ts) ->
      let styles = po chan styles in
      trees po pc chan ts ;
      pc chan styles
  | ONode (so,sc,ts) ->
      output_string chan so ;
      trees po pc chan ts ;
      output_string chan sc

and trees po pc chan = function
  | [] -> ()
  | t::rem -> tree po pc chan t ; trees po pc chan rem
  
let ptree chan t = tree potag pctag chan t
and ptrees chan ts = trees potag pctag chan ts
      
open Htmltext

let rec sep_font = function
  | [] -> [],[]
  | {nat=(Size (Int _)|Face _|Color _)} as s::rem ->
      let fs,os = sep_font rem in
      s::fs,os
  | s::rem ->
      let fs,os = sep_font rem in
      fs,s::os


let rec do_potags chan = function
  | [] -> ()
  | {txt=txt}::rem ->
      output_string chan txt ;
      do_potags chan rem

let rec do_pctags chan = function
  | [] -> ()
  | {ctxt=txt}::rem ->
      do_pctags chan rem ;
      output_string chan txt

let potags chan x =
  let fs,os = sep_font x in
  let styles = match fs with
  | [] -> os
  | {ctxt=ctxt}::_ ->
      let txt =
        "<" ^ String.sub ctxt 2 4 ^
        List.fold_right
          (fun {txt=atxt} r -> atxt ^ r)
          fs ">" in
      {nat=Other ; txt=txt ; ctxt=ctxt}::os in
  do_potags chan styles ;
  styles
      
and pctags chan x = do_pctags chan x

let tree chan t = tree potags pctags chan t
and trees chan ts = trees potags pctags chan ts

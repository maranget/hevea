(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: pp.ml,v 1.6 2012-06-05 14:55:39 maranget Exp $                *)
(***********************************************************************)
open Printf
open Lexeme
open Tree

let  potag chan ({txt=txt;_} as s)= output_string chan txt ; s

let pctag chan  {ctxt=txt;_} = output_string chan txt

let ppattr (_,s) = s

let ppattrs attrs = String.concat " " (List.map ppattr attrs)

let rec tree po pc chan = function
  | Text txt -> output_string chan txt
  | Blanks txt ->
      output_string chan txt
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

let sep_font =
  List.partition
    (function
      | { nat=(Size (Int _)|Face _|Color _)} -> true
      | _ -> false)

let sep_span =
  List.partition
    (function
      | { nat=Fstyle _} -> true
      | _ -> false)
  

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

let close_to_open ctag =
  sprintf "<%s" (String.sub ctag 2 (String.length ctag-3))

let fmtfont fs k = match fs with
| [] -> k
| {ctxt=ctxt}::_ ->
    let txt =
      close_to_open ctxt ^
      List.fold_right
        (fun {txt=atxt} r -> atxt ^ r)
        fs ">" in
    {nat=Other; txt=txt; ctxt=ctxt;}::k

let do_fmtfontsyle n v =
  let tag =
    match n with
    | Ffamily -> "font-family"
    | Lexeme.Fstyle -> "font-style" 
    | Fvariant -> "font-variant"
    | Fweight -> "font-weight"
    | Fsize -> "font-size"
    | Fcolor -> "color"
    | Fbgcolor -> "background-color" in
  sprintf "%s:%s" tag v

let fmtfontsyle = function
  | {nat=Fstyle (n,v)} -> do_fmtfontsyle n v
  | _ -> assert false

let as_fontstyle = function
  | {nat=Fstyle (n,_)} -> n
  | _ -> assert false

let fmtfontsyles fs =
  let fs =
    List.sort
      (fun f1 f2 -> Pervasives.compare (as_fontstyle f1) (as_fontstyle f2))
      fs in
  sprintf " style=\"%s\"" (String.concat ";" (List.map fmtfontsyle fs))

let fmtspan fs k = match fs with
| [] -> k
| {ctxt=ctxt}::_ ->
    let txt =
      close_to_open ctxt ^
      fmtfontsyles fs ^
      ">" in
    {nat=Other; txt=txt; ctxt=ctxt;}::k

let potags chan x =
  let fs,os = sep_font x in
  let ss,os = sep_span os in
  let styles = fmtfont fs (fmtspan ss os) in
(*  output_char chan '[' ; *)
  do_potags chan styles ;
(*  output_char chan ']' ; *)
  styles
      
and pctags chan x = do_pctags chan x

let tree chan t = tree potags pctags chan t
and trees chan ts = trees potags pctags chan ts

open Css

let style chan = function
  | Other txt -> output_string chan txt
  | Class (name, addname, txt) ->
      output_char chan '.' ; output_string chan name ;
      (match addname with
      | None -> ()
      | Some n -> output_char chan ' ' ; output_string chan n) ;
      output_string chan txt

let styles chan ids =
  List.iter
    (fun id ->
      style chan id ;
      output_char chan '\n')
    ids

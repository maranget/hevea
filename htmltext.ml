(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: htmltext.ml,v 1.13 2012-06-05 14:55:39 maranget Exp $          *)
(***********************************************************************)

open Emisc
open Lexeme

type tsize = Int of int | Big | Small
    
type nat =
  | Style of tag
  | Size of tsize
  | Color of string
  | Face of string
  | Fstyle of fontstyle * string
  | Other

type t_style = {nat : nat ; txt : string ; ctxt : string}
type style = t_style list

let rec do_cost seen_span seen_font r1 r2 = function
  | [] -> r1,r2
  | {nat=(Size (Int _)|Color _|Face _);_}::rem ->
      do_cost seen_span true (if seen_font then r1 else 1+r1) (1+r2) rem
  | {nat=(Fstyle _);_}::rem ->
      do_cost true seen_font (if seen_span then r1 else 1+r1) (1+r2) rem
  | _::rem -> do_cost seen_span seen_font (1+r1) r2 rem

let cost ss = do_cost false false 0 0 ss

exception No

let add_size d = match !basefont + d with
| 1|2|3|4|5|6|7 as x -> x
| _ -> raise No

let size_val = function
  | "+1" -> add_size 1
  | "+2" -> add_size 2
  | "+3" -> add_size 3
  | "+4" -> add_size 4
  | "+5" -> add_size 5
  | "+6" -> add_size 6
  | "-1" -> add_size (-1)
  | "-2" -> add_size (-2)
  | "-3" -> add_size (-3)
  | "-4" -> add_size (-4)
  | "-5" -> add_size (-5)
  | "-6" -> add_size (-6)
  | "1" -> 1
  | "2" -> 2
  | "3" -> 3
  | "4" -> 4
  | "5" -> 5
  | "6" -> 6
  | "7" -> 7
  | _   -> raise No

let color_val s = match String.lowercase s with
| "#000000" -> "black"
| "#c0c0c0" -> "silver"
| "#808080" -> "gray"
| "#ffffff" -> "white"
| "#800000" -> "maroon"
| "#ff0000" -> "red"
| "#800080" -> "purple"
| "#ff00ff" -> "fuschia"
| "#008000" -> "green"
| "#00ff00" -> "lime"
| "#808000" -> "olive"
| "#ffff00" -> "yellow"
| "#000080" -> "navy"
| "#0000ff" -> "blue"
| "#008080" -> "teal"
| "#00ffff" -> "aqua"
| s -> s

let same_style s1 s2 = match s1.nat, s2.nat with
| Style t1, Style t2 -> t1=t2
| Other, Other -> s1.txt = s2.txt
| Size s1, Size s2 -> s1=s2
| Color c1, Color c2 -> c1=c2
| Face f1, Face f2 -> f1=f2
| Fstyle (a1,v1),Fstyle (a2,v2) -> a1 = a2 && v1 = v2
| _,_ -> false

let is_color = function
  | Color _
  | Fstyle (Fcolor,_) -> true
  | _ -> false

and is_size = function
  | Size _
  | Fstyle (Fsize,_) -> true
  | _ -> false

and is_face = function
  | Face _ -> true
  | _ -> false

exception NoProp

let get_prop = function
  | Size _ -> is_size
  |  Fstyle (Fsize,_)  -> is_size
  | Face _-> is_face
  | Color _ | Fstyle (Fcolor,_) -> is_color
  | _       -> raise NoProp

let neutral_prop p = p (Color "")

let is_font = function
  | Size (Int _) | Face _ | Color _ -> true
  | _ -> false

let is_span (n:nat) = match n with
  | Fstyle _ -> true
  | _ -> false

let font_props = [is_size ; is_face ; is_color]
let span_props = [is_size; is_face; ]

exception Same 

let rec rem_prop p = function
  | s::rem ->
      if p s.nat then rem
      else
        let rem = rem_prop p rem in
        s::rem
  | [] -> raise Same

let rec rem_style s = function
  | os::rem ->
      if same_style s os then rem
      else
        let rem = rem_style s rem in
        os::rem
  | [] -> raise Same

type env = t_style list
let empty_env = []

exception Split of t_style * env

let add s env =
  let new_env =
    try
      let p = get_prop s.nat in
      try
        s::rem_prop p env
      with
      |  Same ->
          match s.nat with
          | Size (Int x) when x = !basefont -> env
          | _ -> s::env
    with
    | NoProp ->
        try
          s::rem_style s env
        with
        | Same ->
            s::env in
  match s.nat with
  | Other ->
      begin match new_env with
      | _::env -> raise (Split (s,env))
      | _ -> assert false
      end
  | _ -> new_env


(* For FONT tag *)  

let add_fontattr txt ctxt a env =
  let nat = match a with
  | SIZE s  -> Size (Int (size_val s))
  | COLOR s -> Color (color_val s)
  | FACE s  -> Face s
  | ASTYLE _|CLASS _|OTHER   -> raise No in
  add {nat=nat ; txt=txt ; ctxt=ctxt} env

let do_addattrs  myadd txt ctxt attrs env = match attrs with
| []  -> env
| _   ->
    let rec do_rec = function
      | [] -> env
      | (a,atxt)::rem ->
          myadd
            atxt
            ctxt
            a
            (do_rec rem) in
    try do_rec attrs with
    | No -> add {nat=Other ; txt=txt ; ctxt=ctxt} env

let add_fontattrs = do_addattrs add_fontattr

(* For SPAN tag *)
let add_spanattr txt ctxt a env =
  let (nat:nat) = match a with
  | ASTYLE (a,v) -> Fstyle (a,v)
  | SIZE _| COLOR _| FACE _ |CLASS _|OTHER   -> raise No in
  add {nat=nat ; txt=txt ; ctxt=ctxt} env

let add_spanattrs = do_addattrs add_spanattr

let add_style
    {Lexeme.tag=tag ; Lexeme.attrs=attrs ; Lexeme.txt=txt ; Lexeme.ctxt=ctxt}
    env
    =
  match tag with
  | FONT -> add_fontattrs txt ctxt attrs env
  | A    -> assert false
  | BIG ->
      if attrs=[] then
        add {nat=Size Big ; txt=txt ; ctxt=ctxt} env
      else
        add {nat=Other ; txt=txt ; ctxt=ctxt} env
  | SMALL ->
      if attrs=[] then
        add {nat=Size Small ; txt=txt ; ctxt=ctxt} env
      else
        add {nat=Other ; txt=txt ; ctxt=ctxt} env
  | SPAN -> add_spanattrs txt ctxt attrs env
  | _ ->
      if attrs=[] then
        add {nat=Style tag ; txt=txt ; ctxt=ctxt} env
      else
        add {nat=Other ; txt=txt ; ctxt=ctxt} env
      
let blanksNeutral s = match s.nat with
| Size _ | Style (U|TT|CODE|SUB|SUP) | Other
| Fstyle ((Fsize|Ffamily|Fvariant|Fbgcolor),_)-> false
| _ -> true

let partition_color styles =
  List.partition (fun s -> not (is_color s.nat)) styles

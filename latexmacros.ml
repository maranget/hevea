(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

let header = "$Id: latexmacros.ml,v 1.50 1999-05-21 14:46:52 maranget Exp $" 
open Misc
open Parse_opts
open Symb
open Lexstate

exception Failed
exception Error of string

type env =
  Style of string
| Font of int
| Color of string

let pretty_env = function
  Style s -> "Style: "^s
| Font i  -> "Font size: "^string_of_int i
| Color s  -> "Font color: "^s
;;



let cmdtable =
  (Hashtbl.create 97 : (string, (pat * action)) Hashtbl.t)
;;

let pretty_macro n acs =
  pretty_pat n ;
  pretty_action acs

let def_coltype name pat action =
  if !verbose > 1 then begin
   Printf.fprintf stderr "def_coltype %s = " name;
   pretty_macro pat action
  end ;
  let cname = Misc.column_to_command name in
  try
    let _ = Hashtbl.find cmdtable cname in () ;
    warning ("Column "^name^" is already defined, not redefining it")
  with
    Not_found ->
      Hashtbl.add cmdtable cname (pat,action)
;;

let def_macro_pat name pat action =
  if !verbose > 1 then begin
   Printf.fprintf stderr "def_macro %s = " name;
   pretty_macro pat action
  end ;
  try
    let _ = Hashtbl.find cmdtable name in () ;
    warning ("ignoring definition of "^name) ;
    raise Failed
  with
    Not_found ->
      Hashtbl.add cmdtable name (pat,action)
;;

let redef_macro_pat name pat action =
  if !verbose > 1 then begin
   Printf.fprintf stderr "redef_macro %s = " name;
   pretty_macro pat action
  end ;
  try
    let _ = Hashtbl.find cmdtable name in
    Hashtbl.add cmdtable name (pat,action)
  with
    Not_found -> begin
      warning ("defining a macro with \\renewcommand, "^name);
      Hashtbl.add cmdtable name (pat,action)
  end
;;
let provide_macro_pat name pat action =
  if !verbose > 1 then begin
   Printf.fprintf stderr "provide_macro %s = " name;
   pretty_macro pat action
  end ;
  try
    let _ = Hashtbl.find cmdtable name in
    raise Failed
  with
    Not_found -> begin
      if !verbose > 1 then begin
        Location.print_pos () ;
        prerr_string "providing non existing "; prerr_endline name
      end ;
      Hashtbl.add cmdtable name (pat,action)
  end
;;

let silent_def_pat name pat action =  Hashtbl.add cmdtable name (pat,action)
;;

let make_pat opts n =
  let n_opts = List.length opts in
  let rec do_rec r i =
    if i <=  n_opts  then r
    else do_rec (("#"^string_of_int i)::r) (i-1) in
  opts,do_rec [] n
;;

let silent_def name n action =  silent_def_pat name (make_pat [] n) action
;;

let def_macro name nargs body =
  def_macro_pat name (make_pat [] nargs) body
and redef_macro name nargs body =
  redef_macro_pat name (make_pat [] nargs) body
;;
let def_code name f = def_macro name 0 (CamlCode f)
and redef_code name f = redef_macro name 0 (CamlCode f)
and def_name_code name f = def_macro name 0 (CamlCode (f name))
;;
     
let def_env name body1 body2 =
  try
    def_macro ("\\"^name) 0 body1 ;
    def_macro ("\\end"^name) 0 body2
  with Failed -> begin
    warning ("not defining environment "^name);
    raise Failed
  end
;;

let def_env_pat name pat b1 b2 =
  try
    def_macro_pat ("\\"^name) pat b1 ;
    def_macro ("\\end"^name) 0 b2
  with Failed -> begin
    warning ("not defining environment "^name);
    raise Failed
  end

and redef_env_pat name pat b1 b2 =
  redef_macro_pat ("\\"^name) pat b1 ;
  redef_macro ("\\end"^name) 0 b2
;;

let unregister name =  Hashtbl.remove cmdtable name
;;

let find_macro name =
  try
    Hashtbl.find cmdtable name
  with Not_found ->
    warning ("unknown macro: "^name) ;
    (([],[]),(Subst ""))

(* Does a user macro exist ? *)
let exists_macro name = 
  try
    let _ = Hashtbl.find cmdtable name in
    true
  with Not_found ->
    false

let is_subst_noarg body pat = match body with
| CamlCode _ -> false
| _ -> pat = ([],[])
;;


(* Base LaTeX macros *)

def_macro "\\bgroup" 0 (Subst "{") ;
def_macro "\\egroup" 0 (Subst "}") ;


def_macro_pat "\\makebox" (["" ; ""],["#1"]) (Subst "\\warning{makebox}\\mbox{#3}") ;
def_macro_pat "\\framebox" (["" ; ""],["#1"]) (Subst "\\warning{framebox}\\fbox{#3}")
;;

(* macro static properties *)

let invisible = function
  "\\nofiles"
| "\\pagebreak" | "\\nopagebreak" | "\linebreak"
| "\\nolinebreak" | "\\label" | "\\index"
| "\\vspace" | "\\glossary" | "\\marginpar"
| "\\figure" | "\\table"
| "\\nostyle" | "\\rm" | "\\tt"
| "\\bf" | "\\em" | "\\it" | "\\sl" 
| "\\tiny" | "\\footnotesize" | "\\scriptsize"
| "\\small" | "\\normalsize" | "\\large" | "\\Large" | "\\LARGE"
| "\\huge" | "\\Huge"
| "\\purple" | "\\silver" | "\\gray" | "\\white"
| "\\maroon" | "\\red" | "\\fuchsia" | "\\green"
| "\\lime" | "\\olive" | "\\yellow" | "\\navy"
| "\\blue" | "\\teal" | "\\aqua" | "\\else" | "\\fi"
| "\\char" -> true
| name ->
    (String.length name >= 3 && String.sub name 0 3 = "\\if")
;;

let limit = function
  "\\limits"
| "\\underbrace"
| "\\sum"
| "\\prod"
| "\\coprod"
| "\\bigcap"
| "\\bigcup"
| "\\bigsqcap"
| "\\bigsqcup"
| "\\bigodot"
| "\\bigdotplus"
| "\\biguplus"
| "\\det" | "\\gcd" | "\\inf" | "\\liminf" | "\\lim" |
   "\\limsup" | "\\max" | "\\min" | "\\Pr" | "\\sup" -> true
| _ -> false
;;

let int = function
  "\\int"
| "\\oint" -> true
| _ -> false
;;

let big = function
  "\\sum"
| "\\prod"
| "\\coprod"
| "\\int"
| "\\oint"
| "\\bigcap"
| "\\bigcup"
| "\\bigsqcap"
| "\\bigsqcup"
| "\\bigodot"
| "\\bigdotplus"
| "\\biguplus" -> true
| _ -> false

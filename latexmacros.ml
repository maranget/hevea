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

let header = "$Id: latexmacros.ml,v 1.58 2000-05-26 17:05:55 maranget Exp $" 
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
and prim_table = Hashtbl.create 5

let pretty_table () =
  let t = Hashtbl.create 97
  and count = ref 0 in
  let incr k =
    incr count ;
    let r =
      try Hashtbl.find t k with
      | Not_found ->
          let r = ref 0 in
          Hashtbl.add t k r ;
          r in
    incr r in
  Hashtbl.iter (fun k _ -> incr k) cmdtable ;
  Printf.fprintf stderr
      "Table size: %d\n" !count ;
  Hashtbl.iter
    (fun k r ->
      if !r > 1 then
        Printf.fprintf stderr "%s: %d\n" k !r)
    t ;
  flush stderr
    
type saved =  (string, (pat * action)) Hashtbl.t *
 (string, (unit -> unit)) Hashtbl.t

let checkpoint () =
  let prim_checked = Hashtbl.create 5
  and cmd_checked = Hashtbl.create 17 in
  Misc.copy_hashtbl prim_table prim_checked ;
  Misc.copy_hashtbl cmdtable cmd_checked ;
  cmd_checked, prim_checked

and hot_start (cmd_checked, prim_checked) = 
  Misc.copy_hashtbl prim_checked prim_table ;
  Misc.copy_hashtbl cmd_checked cmdtable
  
(* Primitives *)
let register_init name f =
  if !verbose > 1 then
    prerr_endline ("Registering primitives for package: "^name);
  try
    let _ = Hashtbl.find prim_table name in
    fatal
      ("Attempt to initlialize primitives for package "^name^" twice")
  with
  | Not_found ->  Hashtbl.add prim_table name f

and exec_init name =
   if !verbose > 1 then
     prerr_endline ("Initializing primitives for package: "^name) ;
  try
    let f = Hashtbl.find prim_table name in
    try f () with
      Failed ->
        Misc.warning
         ("Bad trip while initializing primitives for package: "^name)
  with Not_found -> ()
;;   

let pretty_macro n acs =
  pretty_pat n ;
  prerr_string " -> " ;
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

let redef_macro_pat_once name pat action =
  if !verbose > 1 then begin
   Printf.fprintf stderr "redef_macro_once %s = " name;
   pretty_macro pat action
  end ;
  try
    let _ = Hashtbl.find cmdtable name in
    Hashtbl.remove cmdtable name ;
    Hashtbl.add cmdtable name (pat,action)
  with
    Not_found -> begin
      warning ("defining a macro with \\renewcommand, "^name);
      Hashtbl.add cmdtable name (pat,action)
  end

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

let silent_def_pat name pat action = 
  Hashtbl.add cmdtable name (pat,action) ;
  if !verbose > 1 then begin
    Printf.fprintf stderr "texdef_macro %s = " name;
    pretty_macro pat action
  end

let silent_def_pat_once name pat action = 
  Hashtbl.remove cmdtable name ;
  Hashtbl.add cmdtable name (pat,action) ;
  if !verbose > 1 then begin
    Printf.fprintf stderr "texdef_macro %s = " name;
    pretty_macro pat action
  end
;;

let make_pat opts n =
  let n_opts = List.length opts in
  let rec do_rec r i =
    if i <=  n_opts  then r
    else do_rec (("#"^string_of_int i)::r) (i-1) in
  opts,do_rec [] n
;;

let silent_def name n action =  silent_def_pat name (make_pat [] n) action
and silent_def_once name n action =
  silent_def_pat_once name (make_pat [] n) action
;;

let def_macro name nargs body =
  def_macro_pat name (make_pat [] nargs) body
and redef_macro name nargs body =
  redef_macro_pat name (make_pat [] nargs) body
and redef_macro_once name nargs body =
  redef_macro_pat_once name (make_pat [] nargs) body
;;
let def_code name f = def_macro name 0 (CamlCode f)
and redef_code name f = redef_macro name 0 (CamlCode f)
and def_name_code name f = def_macro name 0 (CamlCode (f name))
;;

let start_env env = "\\"^ env
and end_env env = "\\end"^env
;;

let def_env name body1 body2 =
  try
    def_macro (start_env name) 0 body1 ;
    def_macro (end_env name) 0 body2
  with Failed -> begin
    warning ("not defining environment "^name);
    raise Failed
  end
;;

let def_env_pat name pat b1 b2 =
  try
    def_macro_pat (start_env name) pat b1 ;
    def_macro (end_env name) 0 b2
  with Failed -> begin
    warning ("not defining environment "^name);
    raise Failed
  end

and redef_env_pat name pat b1 b2 =
  redef_macro_pat (start_env name) pat b1 ;
  redef_macro (end_env name) 0 b2
;;

let unregister name =  Hashtbl.remove cmdtable name
;;

let find_macro name =
  try
    Hashtbl.find cmdtable name
  with Not_found ->
    warning ("unknown macro: "^name) ;
    (([],[]),(Subst ""))

and silent_find_macro name =
  try
    Hashtbl.find cmdtable name
  with Not_found ->
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

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

let header = "$Id: latexmacros.ml,v 1.47 1999-05-14 17:54:52 maranget Exp $" 
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

let make_pat opts n =
  let n_opts = List.length opts in
  let rec do_rec r i =
    if i <=  n_opts  then r
    else do_rec (("#"^string_of_int i)::r) (i-1) in
  opts,do_rec [] n
;;

let def_macro name nargs body =
  def_macro_pat name (make_pat [] nargs) body
and redef_macro name nargs body =
  redef_macro_pat name (make_pat [] nargs) body
;;
let def_code name f = def_macro name 0 (CamlCode f)
and redef_code name f = redef_macro name 0 (CamlCode f)
and def_name_code name f =
  def_macro name 0 (CamlCode (f name))
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
    (([],[]),(Print ""))

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

let no_dot = function
  "." -> ""
| s   -> s in
def_macro "\\bgroup" 0 (Subst "{") ;
def_macro "\\egroup" 0 (Subst "}") ;


def_macro_pat "\\makebox" (["" ; ""],["#1"]) (Subst "\\warning{makebox}\\mbox{#3}") ;
def_macro_pat "\\framebox" (["" ; ""],["#1"]) (Subst "\\warning{framebox}\\fbox{#3}") ;



(* Maths *)
def_macro "\\alpha" 0 (Print alpha);
def_macro "\\beta" 0 (Print beta);
def_macro "\\gamma" 0 (Print gamma);
def_macro "\\delta" 0 (Print delta);
def_macro "\\epsilon" 0 (Print epsilon);
def_macro "\\zeta" 0 (Print zeta);
def_macro "\\eta" 0 (Print eta);
def_macro "\\theta" 0 (Print theta);
def_macro "\\vartheta" 0 (Print vartheta);
def_macro "\\iota" 0 (Print iota);
def_macro "\\kappa" 0 (Print kappa);
def_macro "\\lambda" 0 (Print lambda);
def_macro "\\nu" 0 (Print nu);
def_macro "\\xi" 0 (Print xi);
def_macro "\\pi" 0 (Print pi);
def_macro "\\varpi" 0 (Print varpi);
def_macro "\\rho" 0 (Print rho);
def_macro "\\varrho" 0 (Print varrho);
def_macro "\\sigma" 0 (Print sigma);
def_macro "\\varsigma" 0 (Print varsigma);
def_macro "\\tau" 0 (Print tau);
def_macro "\\upsilon" 0 (Print upsilon);
def_macro "\\phi" 0 (Print phi);
def_macro "\\varphi" 0 (Print varphi);
def_macro "\\chi" 0 (Print chi);
def_macro "\\psi" 0 (Print psi);
def_macro "\\omega" 0 (Print omega);

def_macro "\\Gamma" 0 (Print upgamma);
def_macro "\\Delta" 0 (Print updelta);
def_macro "\\Theta" 0 (Print uptheta);
def_macro "\\Lambda" 0 (Print uplambda);
def_macro "\\Xi" 0 (Print upxi);
def_macro "\\Pi" 0 (Print uppi);
def_macro "\\Sigma" 0 (Print upsigma);
def_macro "\\Upsilon" 0 (Print upupsilon);
def_macro "\\Phi" 0 (Print upphi);
def_macro "\\Psi" 0 (Print uppsi);
def_macro "\\Omega" 0 (Print upomega);
();;

def_macro "\\circ" 0 (Print circ);;
def_macro "\\@bullet" 0 (Print bullet);;
def_macro "\\cap" 0 (Print cap);;
def_macro "\\cup" 0 (Print cup);;
def_macro "\\sqcap" 0 (Print sqcap);;
def_macro "\\sqcup" 0 (Print sqcup);;
def_macro "\\vee" 0 (Print vee);;
def_macro "\\wedge" 0 (Print wedge);;
def_macro "\\setminus" 0 (Print setminus);;
def_macro "\\bigtriangleup" 0 (Print bigtriangleup);;
def_macro "\\bigtriangledown" 0 (Print bigtriangledown);;
def_macro "\\triangleleft" 0 (Print triangleleft);;
def_macro "\\triangleright" 0 (Print triangleright);;
def_macro "\\lhd" 0 (Print triangleleft);;
def_macro "\\rhd" 0 (Print triangleright);;
def_macro "\\leq" 0 (Print leq);;
def_macro "\\subset" 0 (Print subset);;
def_macro "\\notsubset" 0 (Print notsubset);;
def_macro "\\subseteq" 0 (Print subseteq);;
def_macro "\\@sqsubset" 0 (Print display_sqsubset);;
def_macro "\\in" 0 (Print elem);;

def_macro "\\geq" 0 (Print geq);;
def_macro "\\supset" 0 (Print supset);;
def_macro "\\supseteq" 0 (Print supseteq);;
def_macro "\\@sqsupset" 0 (Print display_sqsupset);;
def_macro "\\equiv" 0 (Print equiv);;
def_macro "\\ni" 0 (Print ni);;


def_macro "\\approx" 0 (Print approx);;
def_macro "\\neq" 0 (Print neq);;
def_macro "\\propto" 0 (Print propto);;
def_macro "\\perp" 0 (Print perp);;

def_macro "\\leftarrow" 0 (Print leftarrow);;
def_macro "\\Leftarrow" 0 (Print upleftarrow);;
def_macro "\\rightarrow" 0 (Print rightarrow);;
def_macro "\\Rightarrow" 0 (Print uprightarrow);;
def_macro "\\leftrightarrow" 0 (Print leftrightarrow);;
def_macro "\\Leftrightarrow" 0 (Print upleftrightarrow);;
def_macro "\\longleftarrow" 0 (Print longleftarrow);;
def_macro "\\longrightarrow" 0 (Print longrightarrow);;
def_macro "\\longleftrightarrow" 0 (Print longleftrightarrow);;

def_macro "\\aleph" 0 (Print aleph);;
def_macro "\\wp" 0 (Print wp);;
def_macro "\\Re" 0 (Print upre);;
def_macro "\\Im" 0 (Print upim);;
def_macro "\\prim" 0 (Print prim);;
def_macro "\\nabla" 0 (Print nabla);;
def_macro "\\surd" 0 (Print surd);;
def_macro "\\angle" 0 (Print angle);;
def_macro "\\exists" 0 (Print exists);;
def_macro "\\forall" 0 (Print forall);;
def_macro "\\partial" 0 (Print partial);;
def_macro "\\diamond" 0 (Print diamond);;
def_macro "\\clubsuit" 0 (Print clubsuit);;
def_macro "\\diamondsuit" 0 (Print diamondsuit);;
def_macro "\\heartsuit" 0 (Print heartsuit);;
def_macro "\\spadesuit" 0 (Print spadesuit);;
def_macro "\\infty" 0 (Print infty);;

def_macro "\\lfloor" 0 (Print lfloor);;
def_macro "\\rfloor" 0 (Print rfloor);;
def_macro "\\lceil" 0 (Print lceil);;
def_macro "\\rceil" 0 (Print rceil);;
def_macro "\\langle" 0 (Print langle);;
def_macro "\\rangle" 0 (Print rangle);;

def_macro "\\notin" 0 (Print notin);;

def_macro "\\uparrow" 0 (Print uparrow);;
def_macro "\\Uparrow" 0 (Print upuparrow);;
def_macro "\\downarrow" 0 (Print downarrow);;
def_macro "\\Downarrow" 0 (Print updownarrow);;

def_macro "\\oplus" 0 (Print oplus);;
def_macro "\\otimes" 0 (Print otimes);;
def_macro "\\ominus" 0 (Print ominus);;

def_macro "\\@int" 0 (Print int);;
def_macro "\\@displayint" 0 (Print display_int);;
();;




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

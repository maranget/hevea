(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

(*  $Id: package.ml,v 1.107 2007-06-14 14:34:22 maranget Exp $    *)

module type S = sig  end

module Make
  (Dest : OutManager.S)  (Image : ImageManager.S)
  (Scan : Latexscan.S)  : S =
struct
open Misc
open Lexing
open Lexstate
open Latexmacros
open Subst
open MyStack
open Scan
;;

(*********************************************************)
(* Accents are here, to make latexscan.mll a bit smaller *)
(* Accent commands use a mapping from ascii to           *)
(* unicode entities with accents                         *)   
(*  - When no char with accent exists (html) or can      *)
(*    be outputed (text), command                        *)
(*    \text@accent{\cmd}{name}{arg} is scanned           *)
(* where \cmd is accent command name                     *)
(*       name is some internal name (eg \' -> accute)    *)
(*       arg is the argument to \cmd                     *)
(* See iso-sym.hva, for the definition of \text@accent   *)
(*********************************************************)

let put_empty empty =
  if empty <> OutUnicode.null then Dest.put_unicode empty
  else raise OutUnicode.CannotTranslate 

exception DiacriticFailed of string * string

let do_def_diacritic _verb _name f empty = 
  (fun lexbuf ->    
    let arg0 = save_arg lexbuf in
    let arg = get_prim_onarg arg0 in
    try match String.length arg with
    | 0 -> put_empty empty
    | 1 ->
        let c = arg.[0] in
        if c = ' ' then put_empty empty
        else Dest.put_unicode (f arg.[0])
    | _ -> raise OutUnicode.CannotTranslate
    with
    | OutUnicode.CannotTranslate
    | Misc.CannotPut ->	raise (DiacriticFailed (Subst.do_subst_this arg0,arg)))

let def_diacritic name internal f empty =
  def_code name
    (fun lexbuf ->
      try do_def_diacritic true name f empty lexbuf
      with DiacriticFailed (p,input) ->
	scan_this main
	  ("\\text@accent{"^internal^"}{"^name^"}{\\@print{"^input^"}}{"^p^"}"))
;;

open OutUnicode

let () =
  def_diacritic "\\'"  "acute" OutUnicode.acute acute_alone ;
  def_diacritic "\\`"  "grave" OutUnicode.grave grave_alone ;
  def_diacritic "\\^"  "circumflex" OutUnicode.circumflex circum_alone ;
  def_diacritic "\\\"" "diaeresis" OutUnicode.diaeresis diaeresis_alone ;
  def_diacritic "\\c"  "cedilla" OutUnicode.cedilla cedilla_alone ;
  def_diacritic "\\~"  "tilde" OutUnicode.tilde tilde_alone ;
  def_diacritic "\\="  "macron" OutUnicode.macron macron_alone ;
  def_diacritic "\\H"  "doubleacute" OutUnicode.doubleacute doubleacute_alone ;
(* package fc, see later ? *)
(* def_diacritic "\\G"  "doublegrave" OutUnicode.doublegrave 0x2F5 ; *)
  def_diacritic "\\u"  "breve" OutUnicode.breve breve_alone ;
  def_diacritic "\\."  "dotabove" OutUnicode.dotabove dotabove_alone ;
  def_diacritic "\\d"  "dotbelow" OutUnicode.dotbelow dotbelow_alone ;
  def_diacritic "\\b"  "linebelow" OutUnicode.linebelow  linebelow_alone ;
  def_diacritic "\\k"  "ogonek" OutUnicode.ogonek ogonek_alone ;
  def_diacritic "\\r"  "ringabove" OutUnicode.ring ring_alone ;
  def_diacritic "\\v"  "caron" OutUnicode.caron  caron_alone ;
  def_diacritic "\\textcircled" "circled" OutUnicode.circled circled_alone ;
(* activated by amssymb *)
  def_diacritic "\\@doublestruck" "doublestruck" OutUnicode.doublestruck null ;
  ()
;;

(*
 Specialized version of \IfDisplay (plain.hva) for command names,
 so as to avoid inserting \fi at end of scanned text
*)
def_code "\\DisplayChoose"
  (fun lexbuf ->
    let cmd1 = get_csname lexbuf in
    let cmd2 = get_csname lexbuf in
    if !display then
      expand_command cmd1 lexbuf
    else
      expand_command cmd2 lexbuf)
;;

(**************)
(* Delimiters *)
(**************)
def_code "\\process@delim@one"
  (fun lexbuf ->
    let n = Get.get_int (save_arg lexbuf) in
    let n = if n < 2 then 2 else n in
    let mid = get_csname lexbuf in
    for _i = 1 to n-1 do
      scan_this main mid ; Dest.skip_line () ;
    done ;
    scan_this main mid)
;;


def_code "\\process@delim@top"
  (fun lexbuf ->
    let n = Get.get_int (save_arg lexbuf) in
    let top = get_csname lexbuf in
    let mid = get_csname lexbuf in
    scan_this main top ;
    if n > 1 then begin
      Dest.skip_line () ; scan_this main mid ;
      for _i = 1 to n-2 do
        Dest.skip_line () ; scan_this main mid ;
      done
    end)
;;

def_code "\\process@delim@dow"
  (fun lexbuf ->
    let n = Get.get_int (save_arg lexbuf) in
    let mid = get_csname lexbuf in
    let dow = get_csname lexbuf in
    for _i = 1 to n-1 do
      scan_this main mid ; Dest.skip_line () ;
    done ;
    scan_this main dow)
;;

def_code "\\process@delim@three"
  (fun lexbuf ->
    let n = Get.get_int (save_arg lexbuf) in
    let top = get_csname lexbuf in
    let mid = get_csname lexbuf in
    let dow = get_csname lexbuf in
    scan_this main top ; scan_this main "\\@top@br" ;
    for _i = 1 to n-2 do
      scan_this main mid ; Dest.skip_line () ;
    done ;
    scan_this main dow)
;;

def_code "\\process@delim@four"
  (fun lexbuf ->
    let n = Get.get_int (save_arg lexbuf) in
    let ext = get_csname lexbuf in
    let top = get_csname lexbuf in
    let mid = get_csname lexbuf in
    let dow = get_csname lexbuf in
    scan_this main top ; Dest.skip_line () ;
    for _i = 1 to (n-3+1)/2 do
      scan_this main ext ; Dest.skip_line () ;
    done ;
    scan_this main mid ;  Dest.skip_line () ;
    for _i = 1 to (n-3+1)/2 do
      scan_this main ext ; Dest.skip_line () ;
    done ;
    scan_this main dow)
;;

let int_sup_sub lexbuf =
  let n = Get.get_int (save_arg lexbuf) in
  if !display then begin
    let {limits=_limits ; sup=sup ; sub=sub} = save_sup_sub lexbuf in
    Dest.int_sup_sub false n
      (scan_this_arg main) (fun () -> ()) sup sub true
  end
;;

def_code "\\int@sup@sub" int_sup_sub
;;

(* Direct ahustement of vsize *)
def_code "\\@addvsize"
  (fun lexbuf ->
    let n =  Get.get_int (save_arg lexbuf) in
    Dest.addvsize n)
;;

(*****************)
(* Cummunication *)
(*****************)

def_code "\\typeout"
  (fun lexbuf ->
    let what = Scan.get_prim_arg lexbuf in
    prerr_endline what )
;;

def_code "\\hva@warn"
  (fun lexbuf ->
    let what = Subst.subst_arg lexbuf in
    warning what )
;;

def_code "\\@lexbuf"
  (fun _ ->
    prerr_endline ("LEXBUF: "^string_of_int (MyStack.length stack_lexbuf)))
;;

def_code "\\@macros"
  (fun _ -> Latexmacros.pretty_table ())
;;


let def_print name s =
  def_code name (fun _ -> Scan.translate_put_unicode_string s)
;;

def_print "\\@basein" Parse_opts.base_in ;
def_print "\\jobname" Parse_opts.base_out ;
def_print "\\jobname@base" (Filename.basename Parse_opts.base_out) ;
def_print "\\@heveacomline"
  (Array.fold_right
     (fun arg r -> arg^" "^r)
     Sys.argv "") ;
def_print "\\@heveaversion" Version.version ;
def_print "\\@hevealibdir" Mylib.libdir
;;

def_code "\\@heveaverbose"
  (fun lexbuf ->    
    let lvl = Get.get_int (save_arg lexbuf) in
    Misc.verbose := lvl)
;;


def_code "\\typemacro"
  (fun lexbuf ->
    let name = Scan.get_csname lexbuf in
    let pat,body = Latexmacros.find name in
    Latexmacros.pretty_macro pat body)
;;

(* External style-sheet *)

def_code "\\hva@dump@css"
  (fun _ ->
     let name = match Parse_opts.base_out with
     | "" ->
         let r = "out.css" in
         warning ("Outputing style sheet to default file: "^r) ;
         r
     | base -> base ^ ".css" in
     begin try
       let stys =
         Dest.to_string 
           (fun () -> scan_this main "\\hevea@css") in
       let chan = open_out name in
       output_string chan stys ;
       close_out chan
     with
     | Sys_error msg ->
         warning ("Trouble while outputing style sheet: "^msg)
     end ;
     scan_this main (Printf.sprintf "\\@getprintnostyle{%s}" name))
;;


(* A few subst definitions, with 2 optional arguments *)

def "\\makebox" (latex_pat ["" ; ""] 3)
    (Subst "\\hva@warn{makebox}\\mbox{#3}") ;
def "\\framebox" (latex_pat ["" ; ""] 3)
    (Subst "\\hva@warn{framebox}\\fbox{#3}")
;;

(*********************)
(* 'Token' registers *)
(*********************)


def_code "\\newtokens"
  (fun lexbuf ->
    let toks = Scan.get_csname lexbuf in
    if Latexmacros.exists toks then
      Misc.warning ("\\newtokens redefines command ``"^toks^"''") ;      
    Latexmacros.def toks zero_pat (Toks []))
;;

let get_tokens toks = match Latexmacros.find_fail toks with
| _,Toks r-> r
| _ -> raise Failed
;;

def_code "\\resettokens"
  (fun lexbuf ->
    let toks = Scan.get_csname lexbuf in 
    try
      ignore (get_tokens toks) ;
      Latexmacros.def toks zero_pat (Toks [])
    with Failed ->
      Misc.warning ("\\resettokens for "^toks^" failed"))
;;

def_code "\\addtokens"
  (fun lexbuf ->
    let toks = Scan.get_csname lexbuf in
    let arg = Subst.subst_arg lexbuf in
    begin try
      let l = get_tokens toks in
      Latexmacros.def toks zero_pat (Toks (arg::l))
    with Failed ->
      Misc.warning ("\\addtokens for "^toks^" failed")
    end)
;;

def_code "\\addrevtokens"
  (fun lexbuf ->
    let toks = Scan.get_csname lexbuf in
    let arg = Subst.subst_arg lexbuf in
    begin try 
      let l = get_tokens toks in
      Latexmacros.def toks zero_pat (Toks (l@[arg]))
    with Failed ->
      Misc.warning ("\\addtokens for "^toks^" failed")
    end)
;;

def_code "\\appendtokens"
 (fun lexbuf ->
   let toks1 = Scan.get_csname lexbuf in
   let toks2 = Scan.get_csname lexbuf in
   begin try
     let l1 = get_tokens toks1
     and l2 = get_tokens toks2 in
      Latexmacros.def toks1 zero_pat (Toks (l2@l1))
   with Failed ->
     Misc.warning ("\\addtokens for "^toks1^" and "^toks2^" failed")
   end)
;;

(* Useful ???
def_code "\\lrtokens"
  (fun lexbuf ->
    let toks = Scan.get_csname lexbuf in
    let out = Out.create_buff () in

    let kont =
      let once = ref false in
      (fun lexbuf ->
        if not !once then begin
          once := true ;
          begin try match Latexmacros.find_fail toks with
          | _,Toks l ->
              let arg = Out.to_string out in
              Latexmacros.def toks zero_pat (Toks (l@[arg]))
          | _ -> raise Failed
          with Failed ->
            Misc.warning ("\\lrtokens for "^toks^" failed")
          end
        end ;
        main lexbuf) in
    start_other_scan "lrtokens" (copy kont "lrtokens" out) lexbuf)

;;
*)


(*********************************)
(* Stacks of command definitions *)
(*********************************)

def_code "\\hva@newstack"
  (fun lexbuf ->
     let name = get_prim_arg lexbuf in
     let stack = MyStack.create name in
     def_code
       ("\\@push"^name)
       (fun lexbuf ->
         let cmd = Scan.get_csname lexbuf in
         let def = Latexmacros.find cmd in
         MyStack.push stack def) ;
     def_code
       ("\\@pop"^name)
       (fun lexbuf ->
         let cmd = Scan.get_csname lexbuf in
         try
           let pat,body = MyStack.pop stack in
           Latexmacros.def cmd pat body
         with Fatal _ ->
           warning (Printf.sprintf "Pop empty stack '%s'" name)))
;;

let call_subst lexbuf =
  let csname = get_csname lexbuf in
  let arg = subst_arg lexbuf in
  let exec = csname^" "^arg in
  if !verbose > 1 then begin
    prerr_string "\\@callsubst: " ;
    prerr_endline exec ;
  end ;
  scan_this  main exec


and call_prim lexbuf =
  let csname = get_csname lexbuf in
  let arg = get_prim_arg lexbuf in
  let exec = csname^" "^arg in
  if !verbose > 1 then begin
    prerr_string "\\@callprim: " ;
    prerr_endline exec ;
  end ;
  scan_this  main exec

and call_subst_opt lexbuf =
  let csname = get_csname lexbuf in  
  let default = subst_arg lexbuf in
  let arg = subst_arg lexbuf in
  let lb = Lexing.from_string arg in
  let opt = try Save.opt lb with Save.NoOpt -> default in
  let rem = Save.remain lb in
  let exec = csname ^ "{" ^ opt ^ "}"  ^ rem  in
  if !verbose > 1 then begin
    prerr_string "\\@callsubstopt: " ;
    prerr_endline exec ;
  end ;
  scan_this  main exec
;;

def_code "\\@funcall" call_subst ;
def_code "\\@callsubst" call_subst ;
def_code "\\@callsubstopt" call_subst_opt ;
def_code "\\@callprim" call_prim ;
;;

def_code "\\@calloptsimple"
  (fun lexbuf ->
    let csname = get_csname lexbuf in
    let arg = subst_arg lexbuf in
    let lb = Lexing.from_string arg in
    let opt = try Some (Save.opt lb) with Save.NoOpt -> None in
    let rem =  Save.remain lb in
    let exec =
       match opt with
       | None ->  csname ^ "{" ^ rem ^ "}"
       | Some opt -> csname ^ "[" ^ opt ^ "]{" ^ rem ^ "}" in
    if !verbose > 1 then begin
      prerr_string "\\@calloptsimple: " ;
      prerr_endline exec ;
    end ;
    scan_this  main exec
  )
;;


(* Haux files parsing hooks before and after reading the file *)
def_code "\\@hauxinit"
  (fun lexbuf ->
    Auxx.init Parse_opts.base_out ;
    check_alltt_skip lexbuf)
;;

def_code "\\@hauxfinal"
  (fun lexbuf ->
    Auxx.final Parse_opts.base_out ;
    check_alltt_skip lexbuf)
;;


let get_raw lexbuf =
  let saved = !raw_chars in
  raw_chars := true ;
  let r = get_prim_arg lexbuf in
  raw_chars := saved ;
  r
;;

def_code "\\@newlabel"
  (fun lexbuf ->
    let name = get_raw lexbuf in
    let arg = get_raw lexbuf in
    Auxx.rset name arg)
;;


def_code "\\@auxwrite"
  (fun lexbuf ->
    let lab = get_raw lexbuf in
    let theref = get_prim_arg lexbuf in
    Auxx.rwrite lab theref)
;;


def_code "\\@auxread"
  (fun lexbuf ->
    let lab = get_raw lexbuf in
    scan_this main (Auxx.rget lab))
;;

def_code "\\@bibread"
  (fun lexbuf ->
    let key = get_raw lexbuf in
    let arg = match Auxx.bget false key with
    | None -> "\\@verbarg{"^key^"}"
    | Some s -> s in
    scan_this main arg)
;;

def_code "\\@bibwrite"
  (fun lexbuf ->
    let pretty = match Subst.subst_arg lexbuf with
    | "\\theheveabib" as s  -> get_prim s
    | s -> s in
    let key = get_raw lexbuf in
    Auxx.bwrite key pretty)
;;


def_code "\\bibcite"
  (fun lexbuf ->
    let name = get_raw lexbuf in
    let arg = Subst.subst_arg lexbuf in
    Auxx.bset name arg)
;;
    
(* Index primitives *)

register_init "index"
  (fun () ->
   def_code "\\@indexwrite"
      (fun lexbuf ->
        let tag = get_prim_opt "default" lexbuf in
        let arg = Subst.subst_arg lexbuf in
        let theref = get_prim_arg lexbuf in
        let lbl = Index.treat  tag arg theref in
        Dest.put lbl) ;

   (* Special indexwrite that does not put an anchor.
      Instead, the anchor is given as an extra argument *)
   def_code "\\@@indexwrite"
      (fun lexbuf ->
        let tag = get_prim_opt "default" lexbuf in
        let arg = Subst.subst_arg lexbuf in
        let theref = get_prim_arg lexbuf in
        let theanchor = get_prim_arg lexbuf in
        Index.treat_anchor tag arg theref theanchor) ;

    (* Special indexwrite where the key is given as fst argument and fully
       processed *)
    def_code "\\@@@indexwrite"
      (fun lexbuf ->
        let tag = get_prim_opt "default" lexbuf in
        let key = get_prim_arg lexbuf in
        let nice = Subst.subst_arg lexbuf in
        let theref = get_prim_arg lexbuf in
        let arg = key ^ "@" ^ nice in
        let lbl = Index.treat  tag arg theref in
        Dest.put lbl) ;
    def_code "\\@printindex"
      (fun lexbuf ->
        let tag =  get_prim_opt "default" lexbuf in
        Index.print (scan_this main) tag) ;

    def_code "\\@indexname"
      (fun lexbuf ->
        let tag = get_prim_opt "default" lexbuf in
        let name = get_prim_arg lexbuf in
        Index.changename tag name) ;

    let new_index lexbuf =
      let tag = get_prim_arg lexbuf in
      let sufin = get_prim_arg lexbuf in
      let sufout = get_prim_arg lexbuf in
      let name = get_prim_arg lexbuf in
      Index.newindex tag sufin sufout name in
    def_code "\\newindex" new_index ;
    def_code "\\renewindex" new_index)    
;;

(* ifthen package *)    
register_init "ifthen"
  (fun () ->
    def_code "\\ifthenelse"
      (fun lexbuf ->
        let cond = save_arg lexbuf in
        let arg_true = save_arg lexbuf in
        let arg_false = save_arg lexbuf in
        scan_this_arg main
          (if Get.get_bool cond then arg_true else arg_false)) ;

    def_code "\\whiledo"
      (fun lexbuf ->
        let test = save_arg lexbuf in
        let body = save_arg lexbuf in
        let btest = ref (Get.get_bool test) in
        while !btest do
          scan_this_arg main body ;
          btest := Get.get_bool test
        done) ;

    def_fun "\\newboolean" (fun s -> "\\newif\\if"^s) ;

    def_code "\\setboolean"
      (fun lexbuf ->
        let name = get_prim_arg lexbuf in
        let arg = save_arg lexbuf in
        let b = Get.get_bool arg in
        scan_this main ("\\"^name^(if b then "true" else "false"))) ;
    ())
;;

                          
(* color package *)
register_init "color"
  (fun () ->
    def_code "\\definecolor"
      (fun lexbuf ->
        Save.start_echo () ;
        let clr = get_prim_arg lexbuf in
        let mdl = get_prim_arg lexbuf in
        let value = get_prim_arg lexbuf in
        Image.put "\\definecolor" ;
        Image.put (Save.get_echo ()) ;
        fun_register (fun () -> Color.remove clr) ;
        Color.define clr mdl value ) ;

    def_code "\\DefineNamedColor"
      (fun lexbuf ->
        let _ = get_prim_arg lexbuf in
        let clr = get_prim_arg lexbuf in
        let mdl = get_prim_arg lexbuf in
        let value = get_prim_arg lexbuf in
        fun_register (fun () -> Color.remove clr) ;
        Color.define clr mdl value ;
        Color.define_named clr mdl value) ;

    let do_getcolor c lexbuf =
      let mdl = get_prim_opt "!*!" lexbuf in    
      let clr = get_prim_arg lexbuf in
      let htmlval = match mdl with
      | "!*!"|"" -> Color.retrieve clr
      | _     -> Color.compute mdl clr in
      Dest.put c ;
      begin match htmlval with
        | Color.Hex x -> Dest.put_char '#' ; Dest.put x
        | Color.Name n -> Dest.put n
      end ;
      Dest.put c in
    def_code "\\@getcolor" (do_getcolor "\"") ;
    def_code "\\@getstylecolor" (do_getcolor "") ;
    ())
;;

register_init "colortbl"
    (fun () ->
      let color_to_string = function
        | Color.Hex x -> "#"^x
        | Color.Name n -> n in
      def_code "\\columncolor"
        (fun lexbuf ->
          let mdl = get_prim_opt "!*!" lexbuf in    
          let clr = get_prim_arg lexbuf in
          let htmlval = match mdl with
          | "!*!" -> Color.retrieve clr
          | _     -> Color.compute mdl clr in
          skip_opt lexbuf ;
          skip_opt lexbuf ;
          Dest.insert_attr "TD" ("bgcolor=\""^color_to_string htmlval^"\"")) ;
      def_code "\\rowcolor"
        (fun lexbuf ->
          let mdl = get_prim_opt "!*!" lexbuf in    
          let clr = get_prim_arg lexbuf in
          let htmlval = match mdl with
          | "!*!" -> Color.retrieve clr
          | _     -> Color.compute mdl clr in
          skip_opt lexbuf ;
          skip_opt lexbuf ;
          Dest.insert_attr "TR" ("bgcolor=\""^color_to_string htmlval^"\"")))
;;

(* xspace package *)
register_init "xspace"
  (fun () ->
    def_code "\\xspace"
    (fun lexbuf ->
      try match Lexstate.full_peek_char lexbuf with
      | '{'|'}'|'~'|'.'|'!'|','|':'|'?'|'/'|'\''|')'|'-'
      | ' '|'\t'|'\n' -> ()
      |  _c -> Dest.put_char ' '
      with Not_found ->
        warning "\\xspace could not reach next char"))
;;

(* sword package *)
register_init "sword"
(fun () ->
      def_code "\\FRAME"
        (fun lexbuf ->
          let _ = lexeme lexbuf in
          (* discard the first 7 arguments *)
          let _ = save_arg lexbuf in 
          let _ = save_arg lexbuf in
          let _ = save_arg lexbuf in
          let _ = save_arg lexbuf in
          let _ = save_arg lexbuf in
          let _ = save_arg lexbuf in
          let _ = save_arg lexbuf in
          (* keep argument 8 *)
          let t = Subst.subst_arg lexbuf in
          (* try to find rightmost material in single quotes *)
          let i = try String.rindex t '\'' with Not_found-> (-1) in
          if i>=0 then begin
            (* we found something, so extract the filename *)
            let j = String.rindex_from t (i - 1) '\'' in
            let s = String.sub t (j + 1) (i - j - 1) in
            let t = Filename.basename (s) in
            let s = Filename.chop_extension (t) in
            (* now form the macro swFRAME whose arg is just the base file
name *)
            let cmd = "\\swFRAME{"^s^"}" in
            (* put it back into the input stream *)
            scan_this main cmd
            end ;
          if i<0 then begin
           (* no filename found: we use a default name and give a warning *)
           warning ("\\FRAME: no filename (missing snapshot?) - using
fallback name");
           let s = "FRAME-graphic-not-found" in
           let cmd = "\\swFRAME{"^s^"}" in
           scan_this main cmd
          end) ;
  def_code "\\UNICODE"
    (fun lexbuf ->
      (* input: \UNICODE{arg} where arg is a hex number, eg 0x23ab *)
      (* output: call to \swUNICODE{arg1}{arg2} where: *)
      (*    arg1 = hex number w/o leading 0, eg x23ab *)
      (*    arg2 = decimal equivalent, eg 9131 *)
      (* it is up to \swUNICODE (in sword.hva) to do final formatting *) 
      let _ = lexeme lexbuf in
      let t = Subst.subst_arg lexbuf in
      let s = string_of_int (int_of_string (t)) in
      let tt = String.sub t (String.index t 'x') (-1+String.length t) in
      let cmd = "\\swUNICODE{" ^tt^"}{"^s^"}" in
      scan_this main cmd)
    )
;;

(* Some strange arg scanning, needed for \DeclareGraphicsRule *)

register_init "graphics"
  (fun () ->
    def_code "\\@verbimagearg"
      (fun lexbuf ->
       let {arg=arg} = save_arg lexbuf in
       Image.put_char '{' ;
       Image.put arg ;
       Image.put_char '}'))
;;

(* url package *)
let verb_arg lexbuf =
  let {arg=url} = save_verbatim lexbuf in
  Scan.translate_put_unicode_string url
;;

def_code "\\@verbarg" verb_arg ;
;;

register_init "url"
  (fun () ->
    def_code "\\@Url" verb_arg ;

    def_code "\\Url"
      (fun lexbuf ->
        Save.start_echo () ;
        let _ = save_verbatim lexbuf in
        let arg = Save.get_echo () in
        scan_this main
          ("\\UrlFont\\UrlLeft\\@Url"^arg^"\\UrlRight\\endgroup")) ;

    let do_urldef lexbuf =
        Save.start_echo () ;
        let name = Scan.get_csname lexbuf in
        let url_macro = Scan.get_csname lexbuf in
        let true_args = Save.get_echo () in
        Save.start_echo () ;
        let _ = save_verbatim lexbuf in
        let arg = Save.get_echo () in
        let what = get_this_main (url_macro^arg) in
        if Scan.echo_toimage () then begin
          Image.put "\\urldef" ;
          Image.put true_args ;
          Image.put arg
        end ;
        Latexmacros.def name zero_pat
          (CamlCode (fun _ -> Dest.put what)) in

    def_code "\\urldef" do_urldef ;
    ())
;;         


(* hyperref (not implemented in fact) *)
register_init "hyperref"
  (fun () ->
    def_code "\\href"
      (fun lexbuf ->
        Save.start_echo () ;
        let _ = save_arg lexbuf in
        let url = Save.get_echo () in
        let {arg=arg ; subst=subst} = save_arg lexbuf in
        scan_this_arg main
          (mkarg ("\\ahref{\\textalltt[]"^url^"}{"^arg^"}") subst)) ;
    def_code "\\hyperimage"
      (fun lexbuf ->
        Save.start_echo () ;
        let _ = save_arg lexbuf in
        let url = Save.get_echo () in
        let _ = save_arg lexbuf in
        scan_this main
          ("\\imgsrc{\\textalltt[]"^url^"}")) ;
    def_code "\\@hyperref"
      (fun lexbuf ->
        let _ = save_arg lexbuf in
        Save.start_echo () ;
        let _ = save_arg lexbuf in
        let url = Save.get_echo () in
        let category = get_prim_arg lexbuf in
        let name = get_prim_arg lexbuf in
        let {arg=text ; subst=subst} = save_arg lexbuf in
        scan_this_arg main
          (mkarg
             ("\\ahref{\\textalltt[]"^url^
              "\\#"^category^"."^name^"}{"^text^"}")
             subst)))
;;

(* (extended) keyval package *)

let keyval_name f k = "\\KV@"^f^"@"^k
let keyval_extra f k = keyval_name f k^"@extra"


let do_definekey lexbuf =
  let argdef = save_opts ["1" ; ""] lexbuf in
  let family = get_prim_arg lexbuf in
  let key = get_prim_arg lexbuf in
  let opt = save_opts [""] lexbuf in
  let body = subst_body lexbuf in
  begin match argdef with
  | {arg=No _}:: _ ->
      begin match opt with
      | [{arg=No _}] ->
          Latexmacros.def (keyval_name family key) one_pat (Subst body)
      | [{arg=Yes opt ; subst=subst}] ->
          Latexmacros.def (keyval_name family key) one_pat (Subst body) ;
          Latexmacros.def
            (keyval_name family key^"@default") zero_pat
            (Subst
               ((keyval_name family key^
                "{"^do_subst_this (mkarg opt subst))^"}"))
      | _ -> assert false
      end
  | [{arg=Yes nargs ; subst=subst} ; opt] ->
      let nargs = Get.get_int (mkarg nargs subst) in
      let extra = keyval_extra key family in
      Latexmacros.def (keyval_name family key) one_pat
        (Subst
           ("\\@funcall{"^extra^"}{#1}")) ;
      begin match opt with
      | {arg=No _} ->
          Latexmacros.def extra (latex_pat [] nargs) (Subst body)
      | {arg=Yes opt ; subst=o_subst} ->
          Latexmacros.def
            extra
            (latex_pat [do_subst_this (mkarg opt o_subst)] nargs)
            (Subst body)
      end
  | _ -> assert false
  end
;;

let do_definekeyopt lexbuf =
  let familly = get_prim_arg lexbuf in
  let key =  get_prim_arg lexbuf in
  let opt = subst_arg lexbuf in
  let body = subst_body lexbuf in
  let name = keyval_name familly key in
  let extra = keyval_extra key familly in
  Latexmacros.def name one_pat
    (Subst
       ("\\@funcall{"^extra^"}{"^opt^"}")) ;
  Latexmacros.def extra one_pat (Subst body)
     
  
let do_setkey lexbuf =
  let family = get_prim_arg lexbuf in
  let arg = subst_arg lexbuf^",," in
  let abuff = Lexing.from_string arg in
  let rec do_rec () =
    let {arg=x} = save_arg_with_delim "," abuff in
    if x <>  "" then begin
      let xbuff = Lexing.from_string (x^"==") in
      check_alltt_skip xbuff ;
      let {arg=key} = save_arg_with_delim "=" xbuff in
      let {arg=value} = save_arg_with_delim "=" xbuff in
      if !verbose > 1 then
        Printf.fprintf stderr "SETKEY, key='%s', value='%s'\n" key value ;
      if key <> "" then begin
        let csname = keyval_name family key in
        if Latexmacros.exists csname then begin
          if value <> "" then
            scan_this main (csname^"{"^value^"}")
          else
            let defname = csname^"@default" in
            if Latexmacros.exists defname then
              scan_this main defname
            else
              warning ("keyval, no default value for key: '"^key^"'")
        end else
          warning ("keyval, unknown key: '"^key^"'")
      end ;
      do_rec ()
    end in
  do_rec ()
;;

register_init "keyval"
  (fun () ->
    def_code "\\define@key" do_definekey ;
    def_code "\\@setkeys" do_setkey
  )
;;

register_init "xypic"
  (fun () ->
    def_code "\\@xyarg" 
      (fun lexbuf ->
        Save.start_echo () ;
        let _ = Lexstate.save_xy_arg lexbuf in
        let r = Save.get_echo () in
        Image.put r
       )
  )
;;

register_init "natbib"
  (fun () ->
    def_code "\\NAT@write"
      (fun lexbuf ->
        let key = get_raw lexbuf in
        let num = get_prim_arg lexbuf in
        let auth = subst_arg lexbuf in
        let year = subst_arg lexbuf in
        let long = subst_arg lexbuf in
        Auxx.bwrite key
          (Printf.sprintf
             "{%s}{%s}{{%s}}{{%s}}"
             num year auth long)) ;
    def_code "\\NAT@bibread"
      (fun lexbuf ->
        let key = get_raw lexbuf in
        let arg =
          match Auxx.bget false key with
          | None -> "{}{??}{}{}"
          | Some s -> s in
        scan_this main ("\\NAT@args" ^ arg)) ;
    ())
;;            

let gput lexbuf c =
  Save.gobble_one_char lexbuf ;
  Dest.put_char c

and gput_unicode lexbuf u =
  Save.gobble_one_char lexbuf ;
  Dest.put_unicode u
;;

let gscan lexbuf cmd =
  Save.gobble_one_char lexbuf ;
  Scan.expand_command_no_skip cmd lexbuf
;;


register_init "german"
  (fun () ->
      def_code "\\@german@dquote"
        (fun lexbuf ->
          if effective !alltt then
            Dest.put_char '"'
          else try
            let c = Save.peek_next_char lexbuf  in
            match c with
            | 'a' | 'A' | 'e' | 'E' | 'i' | 'I' | 'o' | 'O' | 'u'| 'U' ->
                begin try gput_unicode lexbuf (OutUnicode.diaeresis c)
                with OutUnicode.CannotTranslate -> assert false end
            | 's'|'z' -> gput_unicode lexbuf eszett
            | 'c'|'f'|'l'|'m'|'p'|'r'|'t' as c ->
                gput lexbuf c (* for "ck and "ff etc. *)
            | 'S' ->
                Save.gobble_one_char lexbuf ;
                Dest.put "SS"
            | 'Z' ->
                Save.gobble_one_char lexbuf ;
                Dest.put "SZ"
            | '|' | '-'| '"' -> Save.gobble_one_char lexbuf
            | '~'|'=' -> gput lexbuf '-'
            | '`' -> gscan lexbuf "\\glqq"
            | '\'' -> gscan lexbuf "\\grqq"
            | '<' -> gscan lexbuf "\\flqq"
            | '>' -> gscan lexbuf "\\frqq"
            | _ -> Dest.put_char '"'
          with
          | Not_found -> Dest.put_char '"'))
;;

(***************)
(* Translators *)
(***************)

def_code "\\@set@translators"
    (fun lexbuf ->
      let name = get_prim_arg lexbuf in
      OutUnicode.set_translators name)
;;

def_code "\\@set@out@translator"
  (fun lexbuf ->
    let name = get_prim_arg lexbuf in
    OutUnicode.set_output_translator name)
;;

def_code "\\@set@in@translator"
  (fun lexbuf ->
    let name = get_prim_arg lexbuf in
    OutUnicode.set_input_translator name)
;;

(********************************)
(* Counters, change reset lists *)
(********************************)

def_code "\\@addtoreset"
  (fun lexbuf ->
    let name = get_prim_arg lexbuf in
    let within = get_prim_arg lexbuf in
    Counter.addtoreset name within)
;;

register_init "chngcntr"
  (fun () ->
    def_code "\\@removefromreset"
      (fun lexbuf ->
	let name = get_prim_arg lexbuf in
	let within = get_prim_arg lexbuf in
	Counter.removefromreset name within) ;
    ())
;;


(*************************************)
(* Extra font changes, from abisheck *)
(*************************************)

let get_elements str = 
    let len = String.length str in
    let rec all_elements l = match l with
      0 -> []
    | n -> (all_elements (n-1))@[(String.sub str (n-1) 1)] in
    all_elements len
;;   

let str_cat x y = x^y
;;

def_code "\\@mathbb"
  (fun lexbuf -> 
    let arg1 = save_arg lexbuf in
    let str = arg1.arg in
    (*let dummy = print_string str in*)
    let str_list = get_elements str in
    (*let h = List.hd str_list in
    let dummy = print_string h in*)
    let format x = Scan.get_this_main ("\\"^"one@mathbb{"^x^"}") in
    let formatted_list = List.map format str_list in
    let formatted_text = List.fold_left str_cat "" formatted_list in
    (*print_string ("<<"^formatted_text^">>\n") ;*) 
    Dest.put formatted_text
  )
;;

def_code "\\@mathfrak"
  (fun lexbuf -> 
    let str = subst_arg lexbuf in
    let str_list = get_elements str in
    let format x = Scan.get_this_main ("\\"^"one@mathfrak{"^x^"}") in
    let formatted_list = List.map format str_list in
    let formatted_text = List.fold_left str_cat "" formatted_list in
    Dest.put formatted_text
  )
;;

def_code "\\@mathsf"
  (fun lexbuf -> 
    let str = subst_arg lexbuf in
    let str_list = get_elements str in
    let format x = Scan.get_this_main ("\\"^"one@mathsf{"^x^"}") in
    let formatted_list = List.map format str_list in
    let formatted_text = List.fold_left str_cat "" formatted_list in
    Dest.put formatted_text
  )
;;

def_code "\\@mathbf"
  (fun lexbuf -> 
    let str = subst_arg lexbuf in
    (*let str_list = get_elements str in
    let format x = Scan.get_this_main ("\\"^"one@mathbf{"^x^"}") in
    let formatted_list = List.map format str_list in
    let formatted_text = List.fold_left str_cat "" formatted_list in*)
    Dest.put ("<B>"^str^"</B>")
  )
;;

def_code "\\@mathrm"
  (fun lexbuf -> 
    let arg1 = save_arg lexbuf in
    let str = arg1.arg in
    (*let str_list = get_elements str in
    let format x = Scan.get_this_main ("\\"^"one@mathbf{"^x^"}") in
    let formatted_list = List.map format str_list in
    let formatted_text = List.fold_left str_cat "" formatted_list in*)
    Dest.put (str)
  )
;;

def_code "\\@mathcal"
  (fun lexbuf -> 
    let arg1 = save_arg lexbuf in
    let str = arg1.arg in
    let str_list = get_elements str in
    let format x = Scan.get_this_main ("\\"^"one@mathcal{"^x^"}") in
    let formatted_list = List.map format str_list in
    let formatted_text = List.fold_left str_cat "" formatted_list in
    Dest.put formatted_text
  )
;;

def_code "\\@mathtt"
  (fun lexbuf -> 
    let arg1 = save_arg lexbuf in
    let str = arg1.arg in
    let str_list = get_elements str in
    let format x = Scan.get_this_main ("\\"^"one@mathtt{"^x^"}") in
    let formatted_list = List.map format str_list in
    let formatted_text = List.fold_left str_cat "" formatted_list in
    Dest.put formatted_text
  )
;;


end

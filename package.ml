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

(*  $Id: package.ml,v 1.85 2006-03-06 18:34:48 maranget Exp $    *)

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
open Stack
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
  if empty <> 0 then Dest.put_unicode empty
  else raise OutUnicode.CannotTranslate 

exception DiacriticFailed of string

let do_def_diacritic verb name f empty = 
  (fun lexbuf ->
    Save.start_echo () ;
    let arg = save_arg lexbuf in
    let input = Save.get_echo () in
    let arg = get_prim_onarg arg in
    try match String.length arg with
    | 0 -> put_empty empty
    | 1 ->
        let c = arg.[0] in
        if c = ' ' then put_empty empty
        else Dest.put_unicode (f arg.[0])
    | _ -> raise OutUnicode.CannotTranslate
    with
    | OutUnicode.CannotTranslate
    | Misc.CannotPut ->
	raise (DiacriticFailed input))

let def_diacritic name internal f empty =
  def_code name
    (fun lexbuf ->
      try do_def_diacritic true name f empty lexbuf
      with DiacriticFailed input ->
	scan_this main
	  ("\\text@accent{"^internal^"}{"^name^"}{"^input^"}"))
;;

def_diacritic "\\'"  "acute" OutUnicode.acute 0xB4 ;
def_diacritic "\\`"  "grave" OutUnicode.grave 0x50 ;
def_diacritic "\\^"  "circumflex" OutUnicode.circumflex 0x5E ;
def_diacritic "\\\"" "diaeresis" OutUnicode.diaeresis 0xA8 ;
def_diacritic "\\c"  "cedilla" OutUnicode.cedilla 0xB8 ;
def_diacritic "\\~"  "tilde" OutUnicode.tilde 0x7E ;
def_diacritic "\\="  "macron" OutUnicode.macron 0xAF ;
def_diacritic "\\H"  "doubleacute" OutUnicode.doubleacute 0x2DD ;
(* package fc, see later ? *)
(* def_diacritic "\\G"  "doublegrave" OutUnicode.doublegrave 0x2F5 ; *)
def_diacritic "\\u"  "breve" OutUnicode.breve 0x2D8 ;
def_diacritic "\\."  "dotabove" OutUnicode.dotabove 0x2D9 ;
def_diacritic "\\d"  "dotbelow" OutUnicode.dotbelow 0 ;
def_diacritic "\\b"  "linebelow" OutUnicode.linebelow 0x5F ;
def_diacritic "\\k"  "ogonek" OutUnicode.ogonek 0x3DB ;
def_diacritic "\\r"  "ringabove" OutUnicode.ring  0x2DA ;
def_diacritic "\\v"  "caron" OutUnicode.caron 0 ;
def_diacritic "\\textcircled" "circled" OutUnicode.circled 0x25EF ;
(* activated by amssymb *)
def_diacritic "\\@doublestruck" "doublestruck" OutUnicode.doublestruck 0 ;
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
    scan_this main top ; Dest.skip_line () ;
    for _i = 1 to n-2 do
      scan_this main mid ; Dest.skip_line () ;
    done ;
    scan_this main mid)
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
    scan_this main top ; Dest.skip_line () ;
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
    let {limits=limits ; sup=sup ; sub=sub} = save_sup_sub lexbuf in
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

(* Various outworld information *)
let def_print name s =
  def_code name  (fun _ -> Dest.put (Dest.iso_string s))
;;

def_code "\\@lexbuf"
  (fun _ ->
    prerr_endline ("LEXBUF: "^string_of_int (Stack.length stack_lexbuf)))
;;

def_code "\\@macros"
  (fun _ -> Latexmacros.pretty_table ())
;;


def_print "\\@basein" Parse_opts.base_in ;
def_print "\\jobname" Parse_opts.base_out ;
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

(* ``Token'' registers *)
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
    begin try
      ignore (get_tokens toks) ;
      Latexmacros.def toks zero_pat (Toks [])
    with Failed ->
      Misc.warning ("\\resettokens for "^toks^" failed")
    end)
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

def_code "\\typemacro"
  (fun lexbuf ->
    let name = Scan.get_csname lexbuf in
    let pat,body = Latexmacros.find name in
    Latexmacros.pretty_macro pat body)
;;

(* See also the lrtokens env in latexscan.mll *)

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
  for i = 0 to String.length url - 1 do
    Dest.put (Dest.iso url.[i])
  done
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
        Printf.fprintf stderr "SETKEY, key=%s, value=%s\n" key value ;
      let csname = keyval_name family key in
      if Latexmacros.exists csname then begin
        if value <> "" then
          scan_this main (csname^"{"^value^"}")
        else
          let defname = csname^"@default" in
          if Latexmacros.exists defname then
            scan_this main defname
          else
            warning ("keyval, no default value for key: ``"^key^"''")
      end else
        warning ("keyval, uknown key: ``"^key^"''") ;
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

register_init "amsmath"
  (fun () ->
    def_code "\\numberwithin"
      (fun lexbuf ->
        let name = get_prim_arg lexbuf in
        let within = get_prim_arg  lexbuf in
        Counter.number_within name within)
  )
;;

register_init "xypic"
  (fun () -> def_code "\\@xyarg" 
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
          ("{"^num^"}{"^auth^"}{"^year^"}{"^long^"}")) ;
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
            | 's'|'z' -> gput_unicode lexbuf 0xDF
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

(* Used by inputenc, other uses ? *)
def_code "\\@set@out@translator"
    (fun lexbuf ->
      let key = get_prim_arg lexbuf in
      OutUnicode.set_translate key) ;
def_code "\\@set@out@translator@table"
    (fun lexbuf ->
      let name = get_prim_arg lexbuf in
      OutUnicode.set_translate_table name) ;
  ()    
;;

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


(*
def_code "\\cfrac"
  (fun lexbuf ->
    let optarg = save_opt "" lexbuf in
    let arg_up = save_arg lexbuf in
    let arg_down = save_arg lexbuf in
    let opt_str = optarg.arg in
    let align = match opt_str with
      "l" -> "align=left"
    | "r" -> "align=right"
    | "" ->  "align=center"  
    | _ -> "align=center" (* WARNING : need to give appropriate warning *) in
    Dest.open_block "TABLE" "border=0 cellpadding=0 cellspacing=0";
        
    Dest.open_block "TR" "" ;
    Dest.open_block "TD" (align^" valign=bottom") ;
    scan_this_arg Scan.main arg_up;
    Dest.close_block "TD" ;
    Dest.close_block "TR" ;

    Dest.open_block "TR" "" ;
    Dest.open_block "TD" "bgcolor=black" ;
    Dest.open_block "TABLE" 
      "border=0 width=\"100%\" cellspacing=0 cellpadding=1" ;
    Dest.open_block "TR" "" ;
    Dest.open_block "TD" "";
    Dest.close_block "TD" ; 
    Dest.close_block "TR" ;
    Dest.close_block "TABLE" ;
    Dest.close_block "TD" ; 
    Dest.close_block "TR" ;

    Dest.open_block "TR" "" ;
    Dest.open_block "TD" (align^" valign=top") ;
    scan_this_arg Scan.main arg_down;
    Dest.close_block "TD" ;
    Dest.close_block "TR" ;
    Dest.close_block "TABLE")
;;
*)


 end

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

(*  $Id: package.ml,v 1.11 2000-01-19 20:11:18 maranget Exp $    *)

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

(* Various outworld information *)
def_print "\\@basein" Parse_opts.base_in ;
def_print "\\jobname" Parse_opts.base_out ;
def_print "\\@heveacomline"
  (Array.fold_right
     (fun arg r -> arg^" "^r)
     Sys.argv "") ;
def_print "\@heveaversion" Version.version ;
def_print "\@hevealibdir" Mylib.libdir
;;

(* ``Token'' registers *)
def_code "\\newtokens"
  (fun lexbuf ->
    let toks = Subst.subst_csname lexbuf in
    begin try
      def_macro toks 0 (Subst "")
    with Latexmacros.Failed ->
      Misc.warning ("\\newtokens for "^toks^" failed")
    end)
;;

def_code "\\addtokens"
  (fun lexbuf ->
    let toks = Subst.subst_csname lexbuf in
    let arg = Subst.subst_arg lexbuf in
    begin try match find_macro toks with
    | _,Subst s ->
        redef_macro toks 0 (Subst (arg^"%\n"^s))
    | _ -> raise Failed
    with Failed ->
      Misc.warning ("\\addtokens for "^toks^" failed")
    end)
;;

(* try e1 with _ -> e2 *)
def_code "\\@try"
  (fun lexbuf ->
    let e1 = save_arg lexbuf in
    let e2 = save_arg lexbuf in
    let saved = Hot.checkpoint ()
    and saved_lexstate = Lexstate.check_lexstate () in
    try
      scan_this_arg Scan.main e1
    with e -> begin
      Misc.warning ("\\@try caught exception : "^Printexc.to_string e) ;
      Hot.start saved ;
      Lexstate.hot_lexstate saved_lexstate ;
      scan_this_arg Scan.main e2
    end)
;;

(* Aux files parsing *)
def_code "\\@hauxinit"
  (fun lexbuf ->
    Auxx.init Parse_opts.base_out ;
    check_alltt_skip lexbuf)
;;

def_code "\\@newlabel"
  (fun lexbuf ->
    let name = get_prim_arg lexbuf in
    let arg = get_prim_arg lexbuf in
    Auxx.rset name arg)
;;

def_code "\\@fst"
  (fun lexbuf ->
    let arg = Subst.subst_arg lexbuf in
    let fst_arg = Save.arg (Lexing.from_string arg) in
    scan_this main fst_arg)
;;

let do_call lexbuf =
  let csname = subst_csname lexbuf in
  let nargs = Get.get_int (save_arg lexbuf) in
  let arg = subst_arg lexbuf in
  scan_this  main (csname^" "^arg)
;;

def_code "\\@funcall" do_call
;;

def_code "\\@auxwrite"
  (fun lexbuf ->
    let lab = get_prim_arg lexbuf in
    let theref = get_prim_arg lexbuf in
    Auxx.rwrite lab theref)
;;

def_code "\\@auxread"
  (fun lexbuf ->
    let lab = get_prim_arg lexbuf in 
    scan_this main (Auxx.rget lab))
;;


def_code "\\bibcite"
  (fun lexbuf ->
    let name = get_prim_arg lexbuf in
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

    def_code "\\@getcolor"
      (fun lexbuf ->
        let mdl = get_prim_opt "!*!" lexbuf in    
        let clr = get_prim_arg lexbuf in
        let htmlval = match mdl with
        | "!*!" -> Color.retrieve clr
        | _     -> Color.compute mdl clr in
        Dest.put_char '"' ;
        Dest.put_char '#' ;
        Dest.put htmlval ;
        Dest.put_char '"'))
;;

(* sword package *)
register_init "sword"
(fun () ->
      def_code "\\FRAME"
        (fun lexbuf ->
          let lxm = lexeme lexbuf in
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
          end
  )
  )
;;

(* url package *)
let verb_arg lexbuf =
  let url,_ = save_verbatim lexbuf in
  for i = 0 to String.length url - 1 do
    Dest.put (Dest.iso url.[i])
  done
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

    let do_urldef csname lexbuf =
        Save.start_echo () ;
        let name = Subst.subst_csname lexbuf in
        let url_macro = Subst.subst_csname lexbuf in
        let true_args = Save.get_echo () in
        Save.start_echo () ;
        let _ = save_verbatim lexbuf in
        let arg = Save.get_echo () in
        let what = get_this_main (url_macro^arg) in
        if csname = "\\urldef" then begin
          if !env_level > 0  then begin
            Image.put "\\urldef" ;
            Image.put true_args ;
            Image.put arg
          end ;
          try def_print name what ; macro_register name with
          | Latexmacros.Failed -> ()
        end else begin
          silent_def csname 0 (CamlCode (fun _ -> Dest.put what)) ;
          macro_register name
        end in
    def_name_code "\\urldef" do_urldef ;
    def_name_code "\\urltexdef" do_urldef ;
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
        let arg,subst = save_arg lexbuf in
        scan_this_arg main
          (("\\ahref{\\textalltt[]"^url^"}{"^arg^"}"),subst)) ;
    def_code "\\hyperimage"
      (fun lexbuf ->
        Save.start_echo () ;
        let _ = save_arg lexbuf in
        let url = Save.get_echo () in
        let _ = save_arg lexbuf in
        scan_this main
          ("\\imgsrc{\\textalltt[]"^url^"}")) ;
    def_code "\\hyperref"
      (fun lexbuf ->
        Save.start_echo () ;
        let url = save_arg lexbuf in
        let url = Save.get_echo () in
        let category = get_prim_arg lexbuf in
        let name = get_prim_arg lexbuf in
        let text,subst = save_arg lexbuf in
        scan_this_arg main
          ("\\ahref{\\textalltt[]"^url^
           "\\#"^category^"."^name^"}{"^text^"}",subst)))
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
  | (No _,_):: _ ->
      begin match opt with
      | [No _,_] ->
          silent_def (keyval_name family key) 1
            (Subst body)
      | [Yes opt,subst] ->
          silent_def (keyval_name family key) 1
            (Subst body) ;
          silent_def
            (keyval_name family key^"@default") 0
            (Subst
               (keyval_name family key^" "^do_subst_this (opt,subst)^"="))
      | _ -> assert false
      end
  | [Yes nargs, subst ; opt] ->
      let nargs = Get.get_int (nargs,subst) in
      let extra = keyval_extra key family in
      silent_def (keyval_name family key) 1
        (Subst
           ("\\@funcall{"^extra^"}{"^string_of_int nargs^"}{#1}")) ;
      begin match opt with
      | No _,_ ->
          silent_def extra nargs (Subst body)
      | Yes opt,o_subst ->
          silent_def_pat
            extra
            (make_pat [do_subst_this (opt,o_subst)] nargs)
            (Subst body)
      end
  | _ -> assert false
  end
;;

let do_setkey lexbuf =
  let family = get_prim_arg lexbuf in
  let arg = subst_arg lexbuf^",," in
  let abuff = Lexing.from_string arg in
  let rec do_rec () =
    let x,_ = save_arg_with_delim "," abuff in
    if x <>  "" then begin
      let xbuff = Lexing.from_string (x^"==") in
      check_alltt_skip xbuff ;
      let key,_ = save_arg_with_delim "=" xbuff in
      let value,_ = save_arg_with_delim "=" xbuff in
      let csname = keyval_name family key in
      if exists_macro csname then begin
        if value <> "" then
          scan_this main (csname^"{"^value^"}")
        else
          scan_this main (csname^"@default")
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
  
  
end

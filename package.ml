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

(*  $Id: package.ml,v 1.3 1999-10-13 08:21:26 maranget Exp $    *)

module type S = sig  end

module Make
  (Dest : OutManager.S)  (Image : ImageManager.S)
  (Scan : Latexscan.S)  : S =
struct
open Misc
open Lexing
open Lexstate
open Latexmacros
open Stack
open Scan
;;

def_print "\\jobname" Parse_opts.base_out ;
def_print "\\@heveacomline"
  (Array.fold_right
     (fun arg r -> arg^" "^r)
     Sys.argv "") ;
def_print "\@heveaversion" Version.version ;
def_print "\@hevealibdir" Mylib.libdir
;;

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
(* extract the filename, assumed to be the rightmost material in single
   quotes *)
        let i = String.rindex t '\'' in
        let j = String.rindex_from t (i - 1) '\'' in
        let s = String.sub t (j + 1) (i - j - 1) in
        let t = Filename.basename (s) in
        let s = Filename.chop_extension (t) in
(* now form the macro swFRAME whose arg is just the base file name *)
        let cmd = "\\swFRAME{"^s^"}" in
(* put it back into the input stream *)
        scan_this main cmd))
;;

register_init "url"
  (fun () ->
    def_code "\\@Url"
      (fun lexbuf ->
        let url,_ = save_verbatim lexbuf in
        for i = 0 to String.length url - 1 do
          Dest.put (Dest.iso url.[i])
        done) ;

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
          if !env_level > 0 then begin
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


end

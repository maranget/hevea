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

(*  $Id: package.ml,v 1.64 2004-07-22 18:55:05 thakur Exp $    *)

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
let def_print name s =
  def_code name  (fun _ -> Dest.put (Dest.iso_string s))
;;

def_code "\\@lexbuf"
  (fun lexbuf ->
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

(* Unicode entities given in hexa *)
def_code "\\@unicode"
 (fun lexbuf ->
   let arg = get_prim_arg lexbuf in
   Scanf.sscanf arg "%x" (fun x -> Dest.put ("&#"^string_of_int x^";")))
;;

     
(* ``Token'' registers *)
def_code "\\newtokens"
  (fun lexbuf ->
    let toks = Scan.get_csname lexbuf in
    if Latexmacros.exists toks then
      Misc.warning ("\\newtokens redefines command ``"^toks^"''") ;      
    Latexmacros.def toks zero_pat (Toks []))
;;

def_code "\\resettokens"
  (fun lexbuf ->
    let toks = Scan.get_csname lexbuf in
    begin try match Latexmacros.find_fail toks with
    | _,Toks _ ->
        Latexmacros.def toks zero_pat (Toks [])
    | _ -> raise Failed
    with Failed ->
      Misc.warning ("\\resettokens for "^toks^" failed")
    end)
;;

def_code "\\addtokens"
  (fun lexbuf ->
    let toks = Scan.get_csname lexbuf in
    let arg = Subst.subst_arg lexbuf in
    begin try match Latexmacros.find_fail toks with
    | _,Toks l ->
        Latexmacros.def toks zero_pat (Toks (arg::l))
    | _ -> raise Failed
    with Failed ->
      Misc.warning ("\\addtokens for "^toks^" failed")
    end)
;;
def_code "\\addrevtokens"
  (fun lexbuf ->
    let toks = Scan.get_csname lexbuf in
    let arg = Subst.subst_arg lexbuf in
    begin try match Latexmacros.find_fail toks with
    | _,Toks l ->
        Latexmacros.def toks zero_pat (Toks (l@[arg]))
    | _ -> raise Failed
    with Failed ->
      Misc.warning ("\\addtokens for "^toks^" failed")
    end)
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

    def_code "\\@getcolor"
      (fun lexbuf ->
        let mdl = get_prim_opt "!*!" lexbuf in    
        let clr = get_prim_arg lexbuf in
        let htmlval = match mdl with
        | "!*!"|"" -> Color.retrieve clr
        | _     -> Color.compute mdl clr in
        Dest.put_char '"' ;
        Dest.put_char '#' ;
        Dest.put htmlval ;
        Dest.put_char '"');
    (*******************************************************
    *    A variant of \@getcolor above, except that it     *
    *    returns the color in hexa minus the quotation     *
    *    marks. e.g #00cc00 as opposed to "#00cc00".       *
    *******************************************************)
    def_code "\\@getstylecolor"
      (fun lexbuf ->
        let mdl = get_prim_opt "!*!" lexbuf in    
        let clr = get_prim_arg lexbuf in
        let htmlval = match mdl with
        | "!*!"|"" -> Color.retrieve clr
        | _     -> Color.compute mdl clr in
        Dest.put_char '#' ;
        Dest.put htmlval ))
;;

register_init "colortbl"
    (fun () ->
      def_code "\\columncolor"
        (fun lexbuf ->
          let mdl = get_prim_opt "!*!" lexbuf in    
          let clr = get_prim_arg lexbuf in
          let htmlval = match mdl with
          | "!*!" -> Color.retrieve clr
          | _     -> Color.compute mdl clr in
          skip_opt lexbuf ;
          skip_opt lexbuf ;
          Dest.insert_attr "TD" ("bgcolor=\"#"^htmlval^"\"")) ;
      def_code "\\rowcolor"
        (fun lexbuf ->
          let mdl = get_prim_opt "!*!" lexbuf in    
          let clr = get_prim_arg lexbuf in
          let htmlval = match mdl with
          | "!*!" -> Color.retrieve clr
          | _     -> Color.compute mdl clr in
          skip_opt lexbuf ;
          skip_opt lexbuf ;
          Dest.insert_attr "TR" ("bgcolor=\"#"^htmlval^"\"")))
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
          end) ;
  def_code "\\UNICODE"
    (fun lexbuf ->
      (* input: \UNICODE{arg} where arg is a hex number, eg 0x23ab *)
      (* output: call to \swUNICODE{arg1}{arg2} where: *)
      (*    arg1 = hex number w/o leading 0, eg x23ab *)
      (*    arg2 = decimal equivalent, eg 9131 *)
      (* it is up to \swUNICODE (in sword.hva) to do final formatting *) 
      let lxm = lexeme lexbuf in
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
        let url = save_arg lexbuf in
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
def_code "\\xrightarrow"
  (fun lexbuf ->
    let optarg = save_opt "" lexbuf in
    let arg = save_arg lexbuf in
    Dest.open_block "TABLE" "border=0 cellpadding=0 cellspacing=0";
    Dest.open_block "SMALL" "" ;
    Dest.open_block "TR" "" ;
    Dest.open_block "TD" "" ;
    Dest.put "&nbsp;&nbsp; " ;
    Dest.close_block "TD" ;
    Dest.open_block "TD" "align=center valign=bottom" ;
    scan_this_arg Scan.main arg;
    Dest.close_block "TD" ;
    Dest.open_block "TD" "color=black" ;
    Dest.put "&nbsp;" ;
    Dest.close_block "TD" ;
    Dest.close_block "TR" ;
    Dest.close_block "SMALL";

    Dest.open_block "TR" "" ;
    Dest.open_block "TD" "ALIGN=right" ;
    Dest.put ("<TABLE border=0 cellspacing=0 cellpadding=1 bgcolor=black "^
	      "width=\"100%\"><TR><TD></TD></TR></TABLE>") ;
    Dest.close_block "TD" ;
    Dest.open_block "TD" "align=center" ;
    Dest.put ("<TABLE border=0 cellspacing=0 cellpadding=1 bgcolor=black "^
	      "width=\"100%\"><TR><TD></TD></TR></TABLE>") ;
    Dest.close_block "TD" ;
    Dest.open_block "TD" "ALIGN=left" ;
    Dest.put "&gt" ;
    Dest.close_block "TD" ; 
    Dest.close_block "TR" ;

    Dest.open_block "SMALL" "" ;
    Dest.open_block "TR" "" ;
    Dest.open_block "TD" "" ;
    Dest.put "&nbsp;" ;
    Dest.close_block "TD" ;
    Dest.open_block "TD" "align=center valign=top" ;
    scan_this_arg Scan.main optarg;
    Dest.close_block "TD" ;
    Dest.open_block "TD" "" ;
    Dest.put "&nbsp;" ;
    Dest.close_block "TD" ;
    Dest.close_block "TR" ;
    Dest.close_block "SMALL" ;
    Dest.close_block "TABLE")
;;


def_code "\\xleftarrow"
  (fun lexbuf ->
    let optarg = save_opt "" lexbuf in
    let arg = save_arg lexbuf in
    Dest.open_block "TABLE" "border=0 cellpadding=0 cellspacing=0";
    Dest.open_block "SMALL" "" ;
    
    Dest.open_block "TR" "" ;
    Dest.open_block "TD" "align=center valign=bottom" ;
    scan_this_arg Scan.main arg;
    Dest.close_block "TD" ;
    Dest.close_block "TR" ;

    Dest.open_block "TR" "" ;
    Dest.open_block "TD" "align=center style=\"font-size:7pt;\"" ;
    Dest.put "&larr;&mdash;&mdash;&mdash;&mdash;" ;
    Dest.close_block "TD" ; 
    Dest.close_block "TR" ;

    Dest.open_block "TR" "" ;
    Dest.open_block "TD" "align=center valign=top" ;
    scan_this_arg Scan.main optarg;
    Dest.close_block "TD" ;
    Dest.close_block "TR" ;
    Dest.close_block "SMALL" ;
    Dest.close_block "TABLE")
;;
*)
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
(************************************************************
*                                                           *
*   Package : "bussproofs"                                  *
*                                                           *
*   The type "proof" is used to store the different         *
*   components of the proof.                                *
*							    *
*   It has been extended with LL, RL, DUMMY to also act     *
*   as the type of elements in the "postorder-stack" used   *
*   to construct the proof tree in a single left to right   *
*   lexical pass.					    *
*                                                           *
************************************************************)

type proof  = AXIOM of string * int * string
            | UNARY_INF of proof * string * int *
			string * string *string
            | BINARY_INF of proof * proof * string * int *
                        string * string *string
	    | TRINARY_INF of proof * proof * proof * string *
	                int * string * string * string
            | LL of string
	    | RL of string
	    | DUMMY
;;


(************************************************************
*                                                           *
*   Package : "bussproofs"                                  *
*                                                           *
*   A "stack" along with push, pop, and view_top functions  *
*                                                           *
*   The stack is a list of "proof"s, to keep all operations *
*   simple.						    *
*							    *
************************************************************)

let stack = ref (DUMMY :: [DUMMY]);;

let alwayslinemode = ref "single"

let nextlinemode = ref "single"

let stack_push x = (stack := (x::(!stack)));;

let stack_pop () = 
	let s = !stack
	in match s with 
	      [] -> DUMMY
	    | (x::ls) -> stack := ls; x
;;

let stack_top1 () = 
        let s = !stack
	in match s with
	      [] -> DUMMY
	    | (x::ls) -> x
;;

let stack_top2 () =
        let s = !stack
	in match s with
	      [] -> DUMMY
	    | (x::[]) -> DUMMY
	    | (x::y::ls) -> y
;;

(************************************************************
*                                                           *
*   Package : "bussproofs"                                  *
*                                                           *
*   "update_proof" computes a synthesized attributes        *
*                                                           *
*    h : the distance from the root node in the proof tree  *
*                                                           *
*    It is used for pretty-printing of the HTML table 	    *
*    containing the proof                                   *
*                                                           *
************************************************************)
   
let rec update_proof (pf, h) = match pf with
	  AXIOM (str, x, line)				  -> 
	  	AXIOM (str, h, line)
	| UNARY_INF (pr,str, x, str1, str2, line)	  -> 
		let new_pr = update_proof (pr, h+1) 
		in  
		    UNARY_INF (new_pr, str, h, str1, str2, line)
        | BINARY_INF (pr1,pr2,str, x, str1, str2, line)	  -> 
	        let new_pr1 = update_proof (pr1,h+1) 
		in let new_pr2 = update_proof (pr2, h+1) 
		in
		    BINARY_INF (new_pr1, new_pr2, str, h, str1, str2, line)
	| TRINARY_INF (pr1,pr2,pr3,str, x, str1, str2, line) ->
	        let new_pr1 = update_proof (pr1, h+1)
		in let new_pr2 = update_proof (pr2, h+1)
		in let new_pr3 = update_proof (pr3, h+1)
		in
		    TRINARY_INF (new_pr1, new_pr2, new_pr3, str, h, str1, 
		    str2, line)
     	| _	-> AXIOM ("",0,"")
;;

(************************************************************
*                                                           *
*    Package : "bussproofs"                                 *
*                                                           *
*    HTML support for different kinds of separators for     *
*    proof rules, in particular single, double, solid,      *
*    dotted, dashed, and none.                              *
*							    *
************************************************************)

let hline linestyle = match linestyle with
    "single" -> "<HR NOSHADE SIZE=\"2\">"
	                     (*style=\"border-top:medium solid grey;"^
                            "border-bottom:thin none;\">"*)
  | "double" -> "<HR style=\"border-top:thick double grey;"^
                            "border-bottom:thin none;\">"
  | "none"   -> "&nbsp;"
  | "solid"  -> "<HR style=\"border-top:medium solid grey;"^
                            "border-bottom:thin none;\">"
  | "dotted" -> "<HR style=\"border-bottom:medium dotted grey;"^
                            "border-top:medium none\">"
  | "dashed" -> "<HR style=\"border-bottom:medium dashed grey;"^
                            "border-top:medium none\">"
  | _        -> "<HR NOSHADE SIZE=\"2\">"
;;

(************************************************************
*                                                           *
*    Package : "bussproofs"                                 *
*                                                           *
*    The function "gen_tables" is used for printing out     *
*    proof in the table using recursive calls to create     *
*    tables for premises. "ntabs" is just used to indent    *
*    the HTML code produced.                                *
*							    *
************************************************************)

let rec ntabs n = match n with
    0 -> ""
  | n -> "\t"^(ntabs (n-1));;

let rec gen_tables pf = match pf with
    (AXIOM (s, h, linestyle)) -> 
      (ntabs h)^"<TABLE CLASS=bussproofs ALIGN=center CELLSPACING=\"0\">"^(ntabs h)^"\n"^
      (ntabs h)^"  <TR>"^
      "\n"^(ntabs h)^"    <TD ALIGN=center>\n"^(ntabs h)^"      "^
      s^
      "\n"^(ntabs h)^"    </TD>"^(ntabs h)^"\n"^(ntabs h)^
      "  </TR>\n"^(ntabs h)^"</TABLE>" 

  | (UNARY_INF (p, s, h, str1, str2, linestyle)) -> 
      (ntabs h)^"<TABLE CLASS=bussproofs ALIGN=center CELLSPACING=\"0\">\n"^
      (ntabs h)^"  <TR>\n"^(ntabs h)^"    <TD>&nbsp;</TD>\n"^(ntabs h)^
      "    <TD ALIGN=center VALIGN=bottom>\n"^ 
      (gen_tables p)^
      "\n"^(ntabs h)^"    </TD>\n"^(ntabs h)^"    <TD>&nbsp;</TD>\n"^
      (ntabs h)^"</TR>\n"^(ntabs h)^"  <TR>\n"^(ntabs h)^
      "    <TD>&nbsp;&nbsp;"^
      str1^
      "\n"^(ntabs h)^"    </TD>\n"^(ntabs h)^
      "    <TD>"^hline(linestyle)^"</TD>\n"^(ntabs h)^"    <TD>"^
      str2^
      "&nbsp;&nbsp;\n"^(ntabs h)^"    </TD>\n"^(ntabs h)^"  </TR>\n"^
      (ntabs h)^"<TR>\n"^(ntabs h)^"    <TD>&nbsp;</TD>"^
      "\n"^(ntabs h)^"    <TD ALIGN=center>\n"^(ntabs h)^"        "^
      s^
      "\n"^(ntabs h)^"    </TD>\n"^(ntabs h)^"    <TD>&nbsp;</TD>\n"^
      (ntabs h)^"</TR>\n"^(ntabs h)^"</TABLE>"

  | (BINARY_INF (p1, p2, s, h, str1, str2, linestyle)) ->
      (ntabs h)^"<TABLE CLASS=bussproofs ALIGN=center CELLSPACING=\"0\">\n"^(ntabs h)^
      "  <TR>\n"^(ntabs h)^"    <TD>&nbsp;</TD>"^"\n"^(ntabs h)^
      "    <TD ALIGN=center VALIGN=bottom>\n"^ 
      (gen_tables p1)^
      "\n"^(ntabs h)^"    </TD>\n"^(ntabs h)^"    <TD>&nbsp;&nbsp;</TD>\n"^
      (ntabs h)^"    <TD ALIGN=center VALIGN=bottom>\n"^
      (gen_tables p2)^
      "\n"^(ntabs h)^"    </TD>\n"^(ntabs h)^"    <TD>&nbsp;</TD>\n"^(ntabs h)^
      "  </TR>\n"^(ntabs h)^"  <TR>\n"^(ntabs h)^"    <TD>&nbsp;&nbsp;"^
      str1^
      "\n"^(ntabs h)^"    </TD>\n"^(ntabs h)^
      "    <TD COLSPAN=\"3\">"^hline(linestyle)^"</TD>"^
      "\n"^(ntabs h)^"    <TD>"^
      str2^
      "&nbsp;&nbsp;\n"^(ntabs h)^"    </TD>\n"^(ntabs h)^"  </TR>\n"^
      (ntabs h)^"  <TR>\n"^(ntabs h)^"    <TD>&nbsp;</TD>"^
      "\n"^(ntabs h)^"    <TD COLSPAN=\"3\" ALIGN=center>\n"^(ntabs h)^
      "        "^
      s^
      "\n"^(ntabs h)^"    </TD>\n"^(ntabs h)^"    <TD>&nbsp;</TD>\n"^
      (ntabs h)^"  </TR>\n"^(ntabs h)^"</TABLE>"

  | (TRINARY_INF (p1,p2,p3,s,h, str1, str2, linestyle)) -> 
      (ntabs h)^"<TABLE CLASS=bussproofs ALIGN=center CELLSPACING=\"0\">\n"^(ntabs h)^
      "  <TR>\n"^(ntabs h)^"    <TD>&nbsp;</TD>"^"\n"^(ntabs h)^
      "    <TD ALIGN=center VALIGN=bottom>\n"^ 
      (gen_tables p1)^
      "\n"^(ntabs h)^"    </TD>\n"^(ntabs h)^"    <TD>&nbsp;&nbsp;</TD>"^
      "\n"^(ntabs h)^"    <TD ALIGN=center VALIGN=bottom>\n"^
      (gen_tables p2)^
      "\n"^(ntabs h)^"    </TD>\n"^(ntabs h)^"    <TD>&nbsp;&nbsp;</TD>"^
      "\n"^(ntabs h)^"    <TD ALIGN=center VALIGN=bottom>\n"^
      (gen_tables p3)^
      "\n"^(ntabs h)^"    </TD>\n"^(ntabs h)^"    <TD>&nbsp;</TD>\n"^
      (ntabs h)^"  </TR>\n"^(ntabs h)^"  <TR>\n"^(ntabs h)^
      "    <TD ALIGN=center>&nbsp;&nbsp;"^
      str1^
      "\n"^(ntabs h)^"    </TD>\n"^(ntabs h)^
      "    <TD COLSPAN=\"5\">"^hline(linestyle)^"</TD>"^
      "\n"^(ntabs h)^"    <TD ALIGN=center>"^
      str2^
      "&nbsp;&nbsp;\n"^(ntabs h)^"    </TD>\n"^(ntabs h)^"  </TR>\n"^
      (ntabs h)^"  <TR>\n"^(ntabs h)^"    <TD>&nbsp;</TD>"^"\n"^(ntabs h)^
      "    <TD COLSPAN=\"5\" ALIGN=center>\n"^(ntabs h)^"        "^
      s^
      "\n"^(ntabs h)^"    </TD>\n"^(ntabs h)^"    <TD>&nbsp;</TD>\n"^
      (ntabs h)^"  </TR>\n"^(ntabs h)^"</TABLE>"
  | _ -> ""
;;

(************************************************************
*                                                           *
*   Package : "proof"                                       *
*                                                           *
*   The functions to start and end tables, and change rows  *
*   and columns using "open_block" and "close_block".       *
*                                                           *
************************************************************)

let start_table s1 s2=
  Dest.open_block "TABLE"
    (s1^" ALIGN=center ")(*^"style = \"text-align: center;\" cellspacing=\"1\" cellpadding=\"1\""*) ;
  Dest.open_block "TR" "" ;
  Dest.open_block "TD" s2
;;

let next_row () =
  Dest.close_block "TD" ; 
  Dest.close_block "TR" ; 
  Dest.open_block "TR" "" ;
  Dest.open_block "TD" "ALIGN=center"
;;

let next_column () =
  Dest.close_block "TD" ; 
  Dest.open_block "TD" "ALIGN=center"
;;

let end_table () =
  Dest.close_block "TD" ; 
  Dest.close_block "TR" ; 
  Dest.close_block "TABLE"
;;

(************************************************************
*                                                           *
*   Package : "bussproofs"                                  *
*                                                           *
*   "get_labels_from_stack" gets the left and right labels  *
*   for a particular inference rule if they were specified. *
*                                                           *
*   There are four possibilities in general :		    *
*   	a) Both specified, with right label first 	    *
* 	    (deeper in the stack)			    *
*   	b) Both specified, with left first		    *
*       c) Either one specified				    *
*       d) None specified				    *
*                                                           *
************************************************************)

let get_labels_from_stack () =
 	let top1 = stack_top1 () in
	let top2 = stack_top2 () in
            match (top1,top2) with
	        (LL s1, RL s2) -> 
		      let ll = stack_pop() in
		      let rl = stack_pop() in
		          (s1,s2)
	      | (RL s1, LL s2) -> 
	              let rl = stack_pop() in
		      let ll = stack_pop () in
		          (s2,s1)
	      | (LL s1, _    ) ->
	              let ll = stack_pop() in
		          (s1,"")
	      | (RL s1, _    ) ->
	              let rl = stack_pop() in
		          ("",s1)
	      | (_    , _    ) ->
	              ("","")
;;

(************************************************************
*                                                           *
*   Implementing the proof package "bussproofs"		    *
*                                                           *
************************************************************)

register_init "bussproofs"
    (fun () ->
      def_code "\\AxiomC"
        (fun lexbuf ->
          let arg = save_arg lexbuf in
          let formatted = Scan.get_this_arg_mbox arg in
	  let axiom = AXIOM (formatted, 0, "") in
	  stack_push axiom);
      def_code "\\UnaryInfC"
        (fun lexbuf ->
	  let arg = save_arg lexbuf in
	  let formatted = Scan.get_this_arg_mbox arg in
	  let (left_label,right_label) = get_labels_from_stack () in
	  let proof1 = stack_pop () in
	  let un_inf = UNARY_INF (proof1,formatted,0,
	  			  left_label,right_label,!nextlinemode) in 
	  nextlinemode := !alwayslinemode; stack_push un_inf);
      def_code "\\BinaryInfC"
        (fun lexbuf ->
	  let arg = save_arg lexbuf in
	  let formatted = Scan.get_this_arg_mbox arg in
	  let (left_label,right_label) = get_labels_from_stack () in
	  let proof2 = stack_pop () in
	  let proof1 = stack_pop () in
	  let bi_inf = BINARY_INF (proof1,proof2,formatted,0,
	  			   left_label,right_label,!nextlinemode) in
	  nextlinemode := !alwayslinemode; stack_push bi_inf);
      def_code "\\TrinaryInfC"
        (fun lexbuf ->
	  let arg = save_arg lexbuf in
	  let formatted = Scan.get_this_arg_mbox arg in
	  let (left_label,right_label) = get_labels_from_stack () in
	  let proof3 = stack_pop () in
	  let proof2 = stack_pop () in
	  let proof1 = stack_pop () in
	  let tri_inf = TRINARY_INF (proof1,proof2,proof3,formatted,0,
	  			     left_label,right_label,!nextlinemode) in
	  nextlinemode := !alwayslinemode; stack_push tri_inf);
      def_code "\\LeftLabel"
        (fun lexbuf ->
	  let arg = save_arg lexbuf in
	  let formatted = Scan.get_this_arg_mbox arg in
	  let left_label = LL (formatted) in 
	  stack_push left_label);
      def_code "\\RightLabel"
        (fun lexbuf ->
	  let arg = save_arg lexbuf in
	  let formatted = Scan.get_this_arg_mbox arg in
	  let right_label = (RL formatted) in
	  stack_push right_label);
      def_code "\\doubleLine"
	(fun lexbuf -> (nextlinemode := "double"));
      def_code "\\singleLine"
	(fun lexbuf -> (nextlinemode := "single"));
      def_code "\\noLine"
	(fun lexbuf -> (nextlinemode := "none"));
      def_code "\\solidLine"
	(fun lexbuf -> (nextlinemode := "solid"));
      def_code "\\dottedLine"
	(fun lexbuf -> (nextlinemode := "dotted"));
      def_code "\\dashedLine"
	(fun lexbuf -> (nextlinemode := "dashed"));
      def_code "\\alwaysDoubleLine"
	(fun lexbuf -> (nextlinemode := "double"; alwayslinemode := "double"));
      def_code "\\alwaysSingleLine"
	(fun lexbuf -> (nextlinemode := "single"; alwayslinemode := "single"));
      def_code "\\alwaysNoLine"
	(fun lexbuf -> (nextlinemode := "none"; alwayslinemode := "none"));
      def_code "\\alwaysSolidLine"
	(fun lexbuf -> (nextlinemode := "solid"; alwayslinemode := "solid"));
      def_code "\\alwaysDottedLine"
	(fun lexbuf -> (nextlinemode := "dotted"; alwayslinemode := "dotted"));
      def_code "\\alwaysDashedLine"
	(fun lexbuf -> (nextlinemode := "dashed"; alwayslinemode := "dashed"));
      def_code "\\DisplayProof"
        (fun lexbuf ->
	  let pf = stack_pop () in
          let newpf = update_proof (pf,0) in
	  let inner_text = gen_tables newpf in
	  Dest.put inner_text);
    )
;;

(************************************************************
*                                                           *
*   Implementing the proof package "proof"		    *
*                                                           *
************************************************************)

register_init "proof"
    (fun () ->
      def_code "\\infer"
	(fun lexbuf ->
          let tag = if (Save.if_next_char '=' lexbuf) then 
	                let _ = save_arg lexbuf 
			in (true) 
                    else (false) in
          let optarg2 = save_opt "" lexbuf in
          let is_opt_arg = if ("" = optarg2.arg) then false else true in  
	  let formatted2 = Scan.get_this_arg_mbox optarg2 in
	  let arg3 = save_arg lexbuf in
	  let arg4 = save_arg lexbuf in
	  start_table "CLASS=proof" "ALIGN=center" ;
          def "\\@hevea@amper" zero_pat
	    (CamlCode (fun _ ->  
	      Dest.force_item_display (); 
              Dest.put_nbsp (); Dest.put_nbsp ())
            ) ;
	  Dest.open_display_varg "VALIGN=bottom";	  
	  scan_this_arg Scan.main arg4;
	  Dest.close_display();
          next_column () ; 
          Dest.put_nbsp () ; 
          next_row () ;
	  if (tag=true) then Dest.put "<HR NOSHADE SIZE=\"4\">\n" 
             else Dest.put "<HR NOSHADE SIZE=\"2\">\n" ;
          next_column () ; 
          Dest.put (formatted2^"&nbsp;&nbsp;") ;
          next_row () ;
	  scan_this_arg Scan.main arg3 ;
          next_column () ; 
          Dest.put_nbsp () ; 
	  end_table ()
        ) ;
      def_code "\\infer*"
	(fun lexbuf ->
          let optarg1 = save_opt "" lexbuf in
          let is_opt_arg = if ("" = optarg1.arg) then false else true in  
	  let formatted1 = Scan.get_this_arg_mbox optarg1 in
          let arg2 = save_arg lexbuf in
	  let arg3 = save_arg lexbuf in
	  start_table "CLASS=proof" "ALIGN=center";
          def "\\@hevea@amper" zero_pat
	    (CamlCode (fun _ ->  
	      Dest.force_item_display (); 
              Dest.put_nbsp () ; Dest.put_nbsp ()) ;
            ) ;
	  Dest.open_display_varg "VALIGN=bottom";	  
	  scan_this_arg Scan.main arg3;
	  Dest.close_display();
          next_column () ; 
          Dest.put_nbsp () ;
          next_row () ;
          Dest.put "&#8942\n" ; 
          next_column () ; 
          Dest.put (formatted1^"&nbsp;&nbsp;");
          next_row () ;
	  scan_this_arg Scan.main arg2 ;
          next_column () ; 
          Dest.put_nbsp () ;
	  end_table ()
        ) ;
      def_code "\\deduce"
	(fun lexbuf ->
          let optarg1 = save_opt "" lexbuf in
          let is_opt_arg = if ("" = optarg1.arg) then false else true in  
	  let formatted1 = Scan.get_this_arg_mbox optarg1 in
          let arg2 = save_arg lexbuf in
	  let arg3 = save_arg lexbuf in
	  start_table "CLASS=proof" "ALIGN=center";
          def "\\@hevea@amper" zero_pat
	    (CamlCode (fun _ ->  
	      Dest.force_item_display (); 
              Dest.put_nbsp () ; Dest.put_nbsp ()) (* scan_this_main "~~" *)
            ) ;
	  Dest.open_display_varg "VALIGN=bottom";	  
	  scan_this_arg Scan.main arg3;
	  Dest.close_display();
          next_column () ; 
          Dest.put_nbsp () ;
          next_row () ;
          Dest.put_nbsp () ; 
          next_column () ; 
          Dest.put formatted1 ;
          Dest.put_nbsp () ;
          Dest.put_nbsp () ;
          next_row () ;
	  scan_this_arg Scan.main arg2 ;
          next_column () ; 
          Dest.put_nbsp () ;
	  end_table ()
        ) ;
    )
;;


(************************************************************
*                                                           *
*   Implementing the proofs in package "mathpartir"	    *
*                                                           *
************************************************************)
(*
      def_code "\\inferrule"
	(fun lexbuf ->
          let optarg1 = save_opt "" lexbuf in
          let is_opt_arg = if ("" = optarg1.arg) then false else true in  
	  let arg2 = save_arg lexbuf in
	  let arg3 = save_arg lexbuf in
          let empty2 = ("" = arg2.arg) in
          let empty3 = ("" = arg3.arg) in
	  start_table "ALIGN=left";
          (if is_opt_arg then 
	    (scan_this_arg Scan.main optarg1 ;
	    next_row ()) 
          else next_row ());
	  if empty2 then 
	    scan_this_arg Scan.main arg3
          else if empty3 then 
	    scan_this_arg Scan.main arg2
	  else
	    (scan_this_arg Scan.main arg2;
	    next_row () ; 
            Dest.put ("<TABLE cellspacing=0 cellpadding=1 bgcolor=green"^
		    " width=\"100%\"><TR><TD> </TD></TR></TABLE>\n") ;
            next_row () ; 
            scan_this_arg Scan.main arg3);
          end_table ()
        )
      ;;

      def_code "\\inferrule"
	(fun lexbuf ->
          let optarg1 = save_opt "" lexbuf in
          let is_opt_arg = if ("" = optarg1.arg) then false else true in  
	  let label = Scan.get_this_main ("\\"^"textsc{"^optarg1.arg^"}") in
	  let arg2 = save_arg lexbuf in
	  let arg3 = save_arg lexbuf in
          let empty2 = ("" = arg2.arg) in
          let empty3 = ("" = arg3.arg) in
          (*print_string ("<<e2:"^arg2.arg^">><<e3:"^arg3.arg^">>\n");
          print_string (if empty2 then "TRUE" else "FALSE"); 
	  print_string (if empty3 then "TRUE" else "FALSE"); 
	  print_string "<--\n";*)
	  let str2 = "\\"^"begin{mpr@line}"^(arg2.arg)^"\\"^"end{mpr@line}" in
	  let str3 = "\\"^"begin{mpr@line}"^(arg3.arg)^"\\"^"end{mpr@line}" in
	  let formatted2 = Scan.get_this_main str2 in
	  let formatted3 = Scan.get_this_main str3 in
	  (if is_opt_arg then 
	    (start_table "ALIGN=left";
            Dest.put label ;
	    next_row ()) 
          else start_table "ALIGN=center" );
	  if empty2 then (Dest.put formatted3(*; print_string "O3\n"*))
          else if empty3 then (Dest.put formatted2(*; print_string "O2\n"*))
	  else
	    (Dest.put formatted2;
            next_row () ; 
            Dest.put ("<TABLE cellspacing=0 cellpadding=1 bgcolor=green"^
		    " width=\"100%\"><TR><TD> </TD></TR></TABLE>\n") ;
            next_row () ; 
            Dest.put formatted3);
          end_table ()
        )
      ;;


      def_code "\\inferrules"
	(fun lexbuf ->
          let optarg1 = save_opt "" lexbuf in
          let is_opt_arg = if ("" = optarg1.arg) then false else true in  
	  let label = Scan.get_this_main ("\\"^"textsc{"^optarg1.arg^"}") in
	  let arg2 = save_arg lexbuf in
	  let arg3 = save_arg lexbuf in
          let empty2 = ("" = arg2.arg) in
          let empty3 = ("" = arg3.arg) in
          (*print_string ("<<e2:"^arg2.arg^">><<e3:"^arg3.arg^">>\n");
          print_string (if empty2 then "TRUE" else "FALSE"); 
	  print_string (if empty3 then "TRUE" else "FALSE"); 
	  print_string "<--\n";*)
	  let str2 = "\\"^"begin{mpr@line}"^(arg2.arg)^"\\"^"end{mpr@line}" in
	  let str3 = "\\"^"begin{mpr@line}"^(arg3.arg)^"\\"^"end{mpr@line}" in
	  let formatted2 = Scan.get_this_main str2 in
	  let formatted3 = Scan.get_this_main str3 in
	  (if is_opt_arg then 
	    (start_table "ALIGN=left";
            Dest.put label ;
	    next_row ()) 
          else start_table "ALIGN=center" );
	  if empty2 then (Dest.put formatted3(*; print_string "O3\n"*))
          else if empty3 then (Dest.put formatted2(*; print_string "O2\n"*))
	  else
	    (Dest.put formatted2;
            next_row () ; 
            Dest.put "<HR NOSHADE SIZE=\"2\" COLOR=green >" ;
	    next_column ();
            Dest.put label;
            next_row () ; 
            Dest.put formatted3);
          end_table ()
        )
;;
*)

(*
def_code "\\@boxed"
  (fun lexbuf -> 
    let arg = save_arg lexbuf in
    Dest.open_block "TABLE" "ALIGN=center cellspacing=0 border=\"2\"" ;
    Dest.open_block "TR" "" ;
    Dest.open_block "TD" "" ;
    Dest.open_display () ;
    scan_this_arg Scan.main arg ;
    Dest.close_display () ;
    Dest.close_block "TD" ;
    Dest.close_block "TR" ;
    Dest.close_block "TABLE"    
  )
;;
*)

end

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

(*  $Id: package.ml,v 1.45 2004-05-27 15:57:35 thakur Exp $    *)

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
        Dest.put_char '"'))
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

type proof  = AXIOM of string * string * string
            | UNARY_INF of proof * string * int * int *
			string * string
            | BINARY_INF of proof * proof * string * int * int *
                        string * string
	    | TRINARY_INF of proof * proof * proof * string *
	                int * int * string * string;;

(************************************************************
*                                                           *
*   "update_proof" computes some Synthesized Attributes     *
*                                                           *
*    a) wid : the number of axioms in a proof  		    *
*    b) ht  : the maximum height of the proof tree	    *
*                                                           *
************************************************************)
   
let rec update_proof pf = match pf with
	  AXIOM (str, str1, str2)				-> 
	  	(AXIOM (str,str1,str2),3,1)
	| UNARY_INF (pr,str,wid,ht, str1, str2)			-> 
		let (new_pr, new_wid, new_ht) = update_proof pr 
		in
		    (UNARY_INF (new_pr, str, new_wid+2, new_ht+1, str1, str2), 
		     new_wid+2, new_ht+1)
        | BINARY_INF (pr1,pr2,str,wid,ht, str1, str2) 		-> 
	        let (new_pr1, new_wid1, new_ht1) = update_proof pr1 
		in let (new_pr2, new_wid2, new_ht2) = update_proof pr2 
		in let new_wid = new_wid1 + new_wid2 + 2 
		in let new_ht = (if new_ht1>new_ht2 then new_ht1 
		    	  	  else new_ht2) + 1 
		in
		    (BINARY_INF (new_pr1, new_pr2, str, new_wid, new_ht, 
			str1, str2), new_wid, new_ht)
	 | TRINARY_INF (pr1,pr2,pr3,str,wid,ht, str1, str2) 	->
	        let  (new_pr1, new_wid1, new_ht1) = update_proof pr1
		in let (new_pr2, new_wid2, new_ht2) = update_proof pr2
		in let (new_pr3, new_wid3, new_ht3) = update_proof pr3
		in let new_wid = new_wid1 + new_wid2 + new_wid3 + 2
		in let new_ht = (if new_ht1>new_ht2 & new_ht1>new_ht3 
		     			then new_ht1
		     		   else if new_ht2>new_ht3 
				   	    then new_ht2 
				        else new_ht3) + 1
		in
		     (TRINARY_INF (new_pr1, new_pr2, new_pr3, str, 
		      new_wid, new_ht, str1, str2), new_wid, new_ht);;
   
(************************************************************
*                                                           *
*   "get_proof_col_row" obtains the number of rows and	    *
*    columns of the table in whoch the proof is to be       *
*    							    *
*   a) columns = number of axioms + (2 * number of rules)   *
*   b) rows    = height of tree  			    *
*                                                           *
************************************************************)
   
let get_proof_col_row proof =
    	let (new_pr,new_wid,new_ht) = update_proof proof
	in
	    (new_pr,new_wid,new_ht);;
   
let get_text pf = match pf with
    		  (AXIOM (s, str1, str2)) -> s
      		| (UNARY_INF (p,s,w,h, str1, str2)) -> s
      		| (BINARY_INF (p1,p2,s,w,h, str1, str2)) -> s
      		| (TRINARY_INF (p1,p2,p3,s,w,h, str1, str2)) -> s;;
   
let get_rinf pf = match pf with
    		  (AXIOM (s, str1, str2)) 			->  
    			(AXIOM (s, str1, str2), 1, 1)
      		| (UNARY_INF (p,s,w,h, str1, str2)) 		-> 
      			(UNARY_INF (p,s,w,h, str1, str2), w, h)
      		| (BINARY_INF (p1,p2,s,w,h, str1, str2)) 	-> 
      			(BINARY_INF (p1,p2,s,w,h, str1, str2), w, h)
      		| (TRINARY_INF (p1,p2,p3,s,w,h, str1, str2)) 	->
      		 	(TRINARY_INF (p1,p2,p3,s,w,h, str1, str2), w, h);;
   
let rec gen_next_rinfo l = match l with
        [] -> []
      | ((p,c,r)::rinfo) -> match c with
      	    0 -> (p,0,0)::(gen_next_rinfo rinfo)
	  | n -> match p with
	     	  AXIOM (s, str1, str2) 		   -> 
		  		(p,0,0)::(p,0,0)::(p,0,0)::
				(gen_next_rinfo rinfo)
		| UNARY_INF (p1,s,w,h, str1, str2)         ->
				(p1,0,0)::(get_rinf p1)::(p1,0,0)::
				(gen_next_rinfo rinfo)
		| BINARY_INF (p1,p2,s,w,h, str1, str2)     ->
				(p1,0,0)::(get_rinf p1)::(get_rinf p2)::
				(p2,0,0)::(gen_next_rinfo rinfo)
		| TRINARY_INF (p1,p2,p3,s,w,h, str1, str2) ->
				(p1,0,0)::(get_rinf p1)::(get_rinf p2)::
				(get_rinf p3)::(p3,0,0)::
				(gen_next_rinfo rinfo);;
   
let rec gen_row l = match l with
        [] -> ""
      | ((p,c,r)::rinfo) -> 
          let left = "      <td style=\"vertical-align: center;"^
	             "text-align: center;\"><br>\n      </td>\n"
	  in let right = left
	  in 
	      match c with
      		  0 -> "      <td style=\"vertical-align: center;"^
		       "text-align: center;\"><br>\n      </td>\n"^
		       (gen_row rinfo)
		| n -> left^
		       "      <td colspan=\""^(int_to_string(n-2))^"\""^
		       "style=\"vertical-align: center;"^
		       "text-align: center;\">\n        "^
		       (get_text p)^
		       "<br>\n      </td>\n"^
		       right^
		       (gen_row rinfo);;

let get_labels pf = match pf with
      	  (AXIOM (s,str1,str2)) -> (str1,str2)
      	| (UNARY_INF (p,s,w,h, str1, str2)) -> (str1,str2)
      	| (BINARY_INF (p1,p2,s,w,h, str1, str2)) -> (str1,str2)
      	| (TRINARY_INF (p1,p2,p3,s,w,h, str1, str2)) -> (str1,str2);;

let rec gen_dash_row pf = match pf with
    	  [] -> ""
        | ((p,c,r)::rinfo) -> 
              let (left_label,right_label) = get_labels p
	      in let left = "      <td style=\"vertical-align: center;"^
	                 "text-align: center;\">"^
			 "&nbsp;&nbsp;&nbsp;"^left_label^
			 "<br>\n      </td>\n"
	      in let right = "      <td style=\"vertical-align: center;"^
	                  "text-align: center;\">"^
			  right_label^"&nbsp;&nbsp;&nbsp;"^
			  "<br>\n      </td>\n"
	      in
	         match c with
    	            0 -> "      <td style=\"vertical-align: center;"^
	              "text-align: center;\"><br>\n      </td>\n"^
	              (gen_dash_row rinfo)
	          | n -> left^
	              "      <td colspan=\""^(int_to_string (n-2))^"\""^
	              "style=\"vertical-align: center;"^
	              "text-align: center;\">\n        "^
	              "<HR>"^ (*(if r=1 then "<br>" else "<HR>")^*)
		      "\n      </td>\n"^
		      right^
	              (gen_dash_row rinfo);;

let rec gen_table r  rinfo  result_rows  = match r with
   	0 -> result_rows
      | rnow -> 
    	    let this_row = "    <tr>\n"^(gen_row rinfo)^"    </tr>\n"
	    in let dash_row = "    <tr>\n"^(gen_dash_row rinfo)^"    </tr>\n"
	    in let new_rinfo = gen_next_rinfo rinfo 
   	    in
	     	gen_table (rnow-1) new_rinfo (dash_row^this_row^result_rows);;

let start_table () = "<html>\n<head>\n"^
		"  <meta content=\"text/html; charset=ISO-8859-1\"\n"^
		"  http-equiv=\"content-type\">\n"^
		"  <title>Proof Table</title>\n"^
		"</head>\n<body>\n"^
		"<table style=\"text-align: center;\" "^
		"border=\"0\"\n"^
		" cellspacing=\"1\" cellpadding=\"0\">\n"^
		"  <tbody>\n";;

let end_table () = "  </tbody>\n</table>\n</body>\n</html>";;
   
let pf = BINARY_INF(
    		    BINARY_INF(
  			UNARY_INF(
				AXIOM(
					"G , a |-  b","","Ax"
				),
				"G |- a -> b",0,0,"","->I"
				
			),
			BINARY_INF(
				AXIOM(
					"G |- c","","Ax"
				),
				AXIOM(
				        "G |- d","","Ax"
				),
				"G |- c ^ d",0,0,"","^I"
			),
			"G |- (a -> b) ^ (c ^ d)",0,0,"","^I"
		    ),
		    AXIOM(
			"G |- e","","Ax"
		    ),
		    "G |-  ((a -> b) ^ (c ^ d)) ^ e",0,0,"","^I"
		);;
let (new_pr,cols,rows) = get_proof_col_row pf;;
let inner_text = gen_table rows [(new_pr,cols,rows)] "";;
let text = start_table()^inner_text^end_table();;
*)    
register_init "bussproof"
    (fun () ->
      def_code "\\AxiomC"
        (fun lexbuf ->
          let arg = save_arg lexbuf in
          let formatted = Scan.get_this_arg_mbox arg in
          let branch = AXIOM(formatted,"","") in
	  let (new_pr,cols,rows) = get_proof_col_row branch in
	  let inner_text = gen_table rows [(new_pr,cols,rows)] "" in
	  let text = start_table()^inner_text^end_table() in
	  Dest.put text)
    )
;;

end

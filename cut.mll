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

(* $Id: cut.mll,v 1.61 2010-01-25 15:58:18 maranget Exp $ *)
{

open Printf

type toc_style = Normal | Both | Special


exception Error of string

module type Config = sig
  val verbose : int
  val name_in : string
  val name_out : string
  val toc_style : toc_style
  val cross_links : bool
end

module Make (Config : Config) =
struct

open Config
open Lexing
open MyStack

let count = ref 0

let dir =
  let dir = Filename.dirname name_out in
  if dir = "." then
    None
  else
    Some dir
 
and base =
  let base = Filename.basename name_in in
  try Filename.chop_extension base 
  with Invalid_argument _ -> base

let changed_t = Hashtbl.create 17

let record_changed oldname newname =
  try
    let _ = Hashtbl.find changed_t oldname in
    Hashtbl.replace changed_t oldname newname
  with Not_found ->
    Hashtbl.add changed_t oldname newname

let rec check_changed name =
  try Hashtbl.find changed_t name
  with Not_found -> name


let real_name name =
  let name = check_changed name in
  match dir with
  | None -> name
  | Some dir -> Filename.concat dir name

let real_open_out name = open_out (real_name name)

let some_links = ref false

let env = Hashtbl.create 17

let imgsrc img alt =
  Printf.sprintf "<IMG SRC=\"%s\" ALT=\"%s\">" img alt


let _ =
  Hashtbl.add env "UPTXT" (imgsrc "contents_motif.gif" "Up") ;
  Hashtbl.add env "PREVTXT" (imgsrc "previous_motif.gif" "Previous") ;
  Hashtbl.add env "NEXTTXT" (imgsrc "next_motif.gif" "Next") ;
  ()

let get_env key =
  try Hashtbl.find env key with Not_found -> assert false
  
(* Accumulate all META, LINK and similar tags that appear in the preamble
   in order to output them in the preamble of every generated page. *)

let header_buff = CutOut.create_buff "header-buf"
let style_buff = CutOut.create_buff "style-buf"
let common_headers = ref ""
and link_style = ref ""

let adjoin_to_header s = CutOut.put header_buff s

and adjoin_to_header_char c = CutOut.put_char header_buff c

let finalize_header () =
  if not (CutOut.is_empty style_buff) then begin
    let css_name = Printf.sprintf "%s.css" base in
    link_style :=
       Printf.sprintf
         "<LINK rel=\"stylesheet\" type=\"text/css\" href=\"%s\">\n"
         css_name ;
    adjoin_to_header !link_style ;
    let chan = real_open_out css_name in
    output_string chan (CutOut.to_string style_buff) ;
    close_out chan
  end ;
  common_headers := CutOut.to_string header_buff

let html_buff = CutOut.create_buff "html-buf"
let html_head = ref ""
and html_foot = ref ""
and html_prefix = ref ""

let phase = ref (-1)
;;

let body = ref "<BODY>"
and doctype = ref ""
and html = ref "<HTML>"
;;

let new_filename _from =  
  incr count ;
  Printf.sprintf "%s%0.3d.html" base !count

let out = ref (CutOut.create_null ())
and out_prefix = ref (CutOut.create_null ())
and outname = ref ""
and lastclosed = ref ""
and otheroutname = ref ""
and flowname_stack = (MyStack.create "flowname" : (string * bool) MyStack.t)
and flow_stack = (MyStack.create "flow" : CutOut.t MyStack.t)
;;

let toc = ref !out
and tocname = ref !outname
and otherout = ref !out
;;

let close_loc _ctx _name out =  CutOut.close out

let change_name oldname name =
  if !phase <= 0 then begin
    if verbose > 0 then
      prerr_endline ("Change "^oldname^" into "^name) ;
    record_changed oldname name ;
  end

let start_phase () =
  incr phase ;
  if verbose > 0 then
    prerr_endline ("Starting phase number: "^string_of_int !phase);
  let base_out = Filename.basename name_out in
  outname := base_out ;
  tocname := base_out ;
  otheroutname := "" ;
  count := 0 ;
  if !phase > 0 then begin
    out := CutOut.create_chan (real_name base_out)
  end ;
  toc := !out
;;

let openlist out =
(*  Printf.eprintf "OPEN LIST: %s\n" (CutOut.get_name out) ; *)
  CutOut.put out "<UL>\n"

and closelist out =
(*  Printf.eprintf "CLOSE LIST: %s\n" (CutOut.get_name out) ; *)
  CutOut.put out "</LI></UL>\n"

and itemref fst_item filename s out =
  let filename = check_changed filename in
  if not fst_item then CutOut.put out "</LI>" ;
  CutOut.put out "<LI>" ;
  CutOut.put out "<A HREF=\"" ;
  CutOut.put out filename ;
  CutOut.put out "\">" ;
  CutOut.put out s ;
  CutOut.put out "</A>\n"

and itemanchor fst_item filename label s out =
  let filename = check_changed filename in
  if not fst_item then CutOut.put out "</LI>" ;
  CutOut.put out "<LI>" ;
  CutOut.put out "<A HREF=\"" ;
  CutOut.put out filename ;
  CutOut.put_char out '#' ;
  CutOut.put out label ;
  CutOut.put out "\">" ;
  CutOut.put out s ;
  CutOut.put out "</A>\n"
;;

let delayed_anchor = ref false
and prev_anchor = ref None

let do_putanchor label out =
  CutOut.put out "<A NAME=\"" ;
  CutOut.put out label ;
  CutOut.put out "\"></A>"
;;

let putanchor label out =
  if !delayed_anchor then
    prev_anchor := Some (label, out)
  else
    do_putanchor label out

and really_putanchor () =
  if !phase = 0 then
    delayed_anchor := true
  else match !prev_anchor with
  | Some (label, out) ->
      do_putanchor label out ;
      prev_anchor := None
  | None -> ()
    

let putlink out name txt =
  let name = check_changed name in
  CutOut.put out "<A HREF=\"" ;
  CutOut.put out name ;
  CutOut.put out "\">" ; 
  CutOut.put out txt ;
  CutOut.put out "</A>\n"
;;

let link_buff = CutOut.create_buff "link-buf"

let putlinks  name =
  let links_there = ref false in
  if verbose > 0 then
    prerr_endline ("putlinks: "^name) ;
  begin try
    putlink link_buff (Thread.prev name) (get_env "PREVTXT") ;
    links_there := true
  with Not_found ->
    if verbose > 0 then
      prerr_endline ("No prev link for "^name)
  end ;
  begin try
    putlink link_buff (Thread.up name) (get_env "UPTXT") ;
    links_there := true
  with Not_found -> () end ;
  begin try
    putlink link_buff (Thread.next name) (get_env "NEXTTXT") ;
    links_there := true
  with Not_found -> () end ;
  if !links_there then
    Some (CutOut.to_string link_buff)
  else
    None

let putlinks_start out outname =
  if cross_links then
    match putlinks outname with
    | Some s -> 
        some_links := true ;
        CutOut.put out s ;
        CutOut.put out "<HR>\n"
    | None -> ()

let putlinks_end out outname =
  if cross_links then
    match putlinks outname with
    | Some s -> 
        some_links := true ;
        CutOut.put out "<HR>\n" ;
        CutOut.put out s
    | None -> ()

  
type file_opt = { with_footer : bool ; with_links : bool ; }

let std_file_opt = { with_footer=true; with_links=true ; }

let openhtml opt title out outname =
  CutOut.put out !doctype ; CutOut.put_char out '\n' ;
  CutOut.put out !html ; CutOut.put_char out '\n' ;
  CutOut.put out "<HEAD>\n" ;
  CutOut.put out !common_headers;
  CutOut.put out "<TITLE>" ;
  let title = Save.tagout (Lexing.from_string (!html_prefix^title)) in
  CutOut.put out title ;
  CutOut.put out "</TITLE>\n" ;
  CutOut.put out "</HEAD>\n" ;
  CutOut.put out !body;
  CutOut.put out "\n" ;
  if opt.with_links then putlinks_start out outname ;
  if opt.with_footer then CutOut.put out !html_head


and closehtml opt name out =
  if opt.with_footer then CutOut.put out !html_foot ;
  if opt.with_links then putlinks_end out name ;
  CutOut.put out "</BODY>\n" ;
  CutOut.put out "</HTML>\n" ;
  close_loc "closehtml" name out
;;

let put_sec hd title hde out =
  CutOut.put out hd ;
  CutOut.put_char out '\n' ;
  CutOut.put out title ;
  CutOut.put out hde ;
  CutOut.put_char out '\n'
;;


let put s = CutOut.put !out s
and put_char c = CutOut.put_char !out c
;;

let cur_level = ref (Section.value "DOCUMENT")
and chapter = ref (Section.value "CHAPTER")
and depth = ref 2
;;


(* Open all lists in toc from chapter to sec, with sec > chapter *)
let rec do_open l1 l2 =
  if l1 < l2 then begin
     begin match toc_style with
     | Both -> openlist !toc ; openlist !out_prefix
     | Special -> openlist !out_prefix
     | Normal  -> openlist !toc
    end ;
    do_open (l1+1) l2
  end
;;

(* close from l1 down to l2 *)
let rec do_close l1 l2 =
  if l1 > l2 then begin
     begin match toc_style with
     | Both -> closelist  !toc ; closelist !out_prefix
     | Special -> closelist !out_prefix
     | Normal  -> closelist !toc
     end ;
     do_close (l1-1) l2
  end else
  cur_level := l1
;;

let anchor = ref 0
;;

let open_section sec name =
  if !phase > 0 then begin
    let fst_item =
      if !cur_level > sec then begin
        do_close !cur_level sec ;
        false
      end else if !cur_level < sec then begin
        do_open  !cur_level sec ;
        true
      end else false in
    incr anchor ;
    let label = "toc"^string_of_int !anchor in
    begin match toc_style with
    | Normal ->
        itemanchor  fst_item !outname label name !toc ;
    | Both ->
        itemanchor  fst_item !outname label name !toc ;
        itemanchor  fst_item !outname label name !out_prefix
    | Special    ->
        itemanchor  fst_item !outname label name !out_prefix
    end ;
    putanchor label !out ;
    cur_level := sec
  end else
    cur_level := sec

and close_section sec =
  if !phase > 0 then do_close !cur_level sec
  else
    cur_level := sec
;;

let close_chapter () =
  if verbose > 0 then
    prerr_endline ("Close chapter out="^ !outname^" toc="^ !tocname) ;
  if !phase > 0 then begin
    if !outname <> !tocname then closehtml std_file_opt !outname !out ;
    begin match toc_style with
    | Both|Special ->
      let real_out = real_open_out !outname in
      CutOut.to_chan real_out !out_prefix ;
      CutOut.to_chan real_out !out ;
      close_out real_out
    | Normal -> ()
    end ;
    out := !toc
  end else begin
    lastclosed := !outname ;
    outname := !tocname
  end

and open_chapter fst_item name =
  outname := new_filename ("open_chapter <<"^name^">>") ;
  if verbose > 0 then
    prerr_endline
      ("Open chapter out="^ !outname^" toc="^ !tocname^
       " cur_level="^string_of_int !cur_level) ;
  if !phase > 0 then begin
    begin match toc_style with
    | Both|Special ->
        out_prefix := CutOut.create_buff (!outname ^ "-prefix") ;
        out := !out_prefix ;
        openhtml std_file_opt name !out_prefix !outname
    | Normal ->
        out := CutOut.create_chan (real_name !outname) ;
        openhtml std_file_opt name !out !outname
    end ;
    itemref fst_item !outname name !toc ;
    cur_level := !chapter
  end else begin
    if verbose > 0 then
      prerr_endline ("link prev="^ !lastclosed^" next="^ !outname) ;
    Thread.setup !outname !tocname ;
    Thread.setprevnext !lastclosed !outname ;
    cur_level := !chapter
  end
;;
let setlink set target =
  if !phase = 0 && target <> "" then
    set !outname target

let open_notes_pred sec_notes =
  (sec_notes <> !chapter) ||
  (!cur_level < sec_notes)

let open_notes sticky sec_notes =
  if verbose > 0 && !phase > 0 then 
    Printf.eprintf "Notes flushed as %s (current cut is %s, current level is %s)\n"
      (Section.pretty sec_notes)
      (Section.pretty !chapter)
      (Section.pretty !cur_level) ;
  if
    not sticky && open_notes_pred sec_notes
  then begin
    otheroutname := !outname ;
    outname := new_filename "open_notes" ;
    if !phase > 0 then begin
      otherout := !out ;
      out := CutOut.create_chan (real_name !outname) ;
      CutOut.put !out !doctype ; CutOut.put_char !out '\n' ;
      CutOut.put !out !html ; CutOut.put_char !out '\n' ;
      CutOut.put !out "<HEAD><TITLE>Notes</TITLE>\n" ;
      CutOut.put !out !common_headers ;
      CutOut.put !out "</HEAD>\n" ;
      CutOut.put !out !body ;
      CutOut.put !out "\n"
    end
  end else
   otheroutname := ""

and close_notes () =
  if !otheroutname <> "" then begin
     CutOut.put !out "\n</BODY></HTML>\n" ;
     close_loc "notes" !outname !out ;
     outname := !otheroutname ;
     out := !otherout ;
     otheroutname := ""
  end
;;

let toc_buf = CutOut.create_buff "toc-buf"
and arg_buf = CutOut.create_buff "arg-buf"
;;

let stack = MyStack.create "main"
;;

let save_state newchapter newdepth =
  if verbose > 0 then
    prerr_endline ("New state: "^string_of_int newchapter) ;
  push stack
    (!outname, MyStack.save flowname_stack, MyStack.save flow_stack,
     !chapter,!depth,!toc,!tocname,!cur_level,!lastclosed,!out_prefix) ;
  chapter := newchapter ;
  depth := newdepth ;
  tocname := !outname ;
  lastclosed := "" ;
  toc := !out
;;

let restore_state () =
  if verbose > 0 then prerr_endline ("Restore") ;
  let
    oldoutname, oldflowname, oldflow,
    oldchapter,olddepth,oldtoc,oldtocname,
    oldlevel,_oldlastclosed,oldprefix  = pop stack in
  outname := oldoutname ;
  MyStack.restore flowname_stack oldflowname ;
  MyStack.restore flow_stack oldflow ;
  chapter := oldchapter ;
  depth := olddepth ;
  toc := oldtoc ;
  tocname := oldtocname ;
  lastclosed := !lastclosed ;
  cur_level := oldlevel ;
  out_prefix := oldprefix
;;


let hevea_footer = ref false

let hevea_footer_buff = CutOut.create_buff "hevea-footer-buf"


let close_top lxm =
  CutOut.put !toc !html_foot ;
  putlinks_end !toc !tocname ;
  if !hevea_footer then begin
    CutOut.put !out "<!--FOOTER-->\n" ;
    CutOut.copy hevea_footer_buff !out 
  end ;
  CutOut.put !toc lxm ;
  if !tocname = "" then
    CutOut.flush !toc
  else
   close_loc "top" !tocname !toc
;;

let open_toc () = if !phase > 0 then openlist !toc
and close_toc () = if !phase > 0 then closelist !toc
;;

let close_all () =
  if !cur_level > !chapter then begin
    close_section !chapter ;
    close_chapter () ;
    close_toc ()
  end else if !cur_level = !chapter then begin
    close_chapter () ;
    close_toc ()
  end ;
  cur_level := (Section.value "DOCUMENT")

let openflow with_footer title =
  let new_outname = new_filename "openflow" in
  push flowname_stack (!outname,with_footer) ;
  outname := new_outname ;
  if !phase > 0 then begin
    push flow_stack !out ;
    out := CutOut.create_chan (real_name !outname) ;
    let opt = { with_footer=with_footer; with_links=false; } in
    openhtml opt title !out !outname
  end

and closeflow () =
  let prev_out, with_footer = pop flowname_stack in
  if !phase > 0 then begin
    let opt = { with_links=false; with_footer=with_footer; } in
    closehtml opt !outname !out;
    out := pop flow_stack
  end ;
  outname := prev_out


} 

let alpha = ['a'-'z' 'A'-'Z']
let secname = alpha+
let blank = [' ''\t''\n']

rule main = parse
| "<!--HEVEA" [^'>']* "-->" '\n'?
    {let lxm = lexeme lexbuf in
    if !phase > 0 then begin
      put lxm ;
      put ("<!--HACHA command line is: ") ;
      for i = 0 to Array.length Sys.argv - 1 do
        put Sys.argv.(i) ;
        put_char ' '
      done ;
      put "-->\n"
    end ;
    main lexbuf}
|  "<!--" "FLOW" ' '+
   {let title,b = flowline lexbuf in
   openflow b title ;
   main lexbuf}
| "<!--" "SETENV" ' ' +
   { let pairs = getargs lexbuf in
     if !phase = 0 then begin
       List.iter
         (fun (name, v) -> Hashtbl.replace env name v)
         pairs
     end ;
     main lexbuf }
| "<!--" "LINKS" ' '+
   {let links = getargs lexbuf in
   List.iter
     (fun (name,v) -> match name with
     | "UP" -> setlink Thread.setup v
     | "PREV" -> setlink Thread.setprev v
     | "NEXT" -> setlink Thread.setnext v
     | _ -> ())
     links ;
   main lexbuf}
| "<!--" "PREFIX" ' '+
   {let l = getargs lexbuf in   
   if !phase = 0 then begin
     match l with
     | [] -> ()
     | (_,v)::_ -> html_prefix := v
   end ;
   main lexbuf}
| "<!--" "END" ' '+ "FLOW" ' '* "-->" '\n'?
   {closeflow () ;
   main lexbuf}
| "<!--" "NAME" ' '+
    {let name = tocline lexbuf in
    change_name !outname name ;
    main lexbuf} 
| "<!--SEC ANCHOR" ' '* "-->"    
    {really_putanchor () ; main lexbuf }
|  "<!--" ("TOC"|"toc") ' '+ (secname as arg) ' '+
    {let sn = 
      if String.uppercase arg = "NOW" then !chapter
      else Section.value arg in
    let name = tocline lexbuf in
    if verbose > 1 then begin
      prerr_endline ("TOC "^arg^" "^name)
    end;
    if sn < !chapter then begin
      if !cur_level >= !chapter then begin
        close_section (!chapter) ;
        close_chapter () ;
        close_toc ()
      end ;
      cur_level := sn
    end else if sn = !chapter then begin
      let fst_item =
        if !cur_level < sn then begin
          open_toc () ;
          true
        end else begin
          close_section !chapter ;
          close_chapter  () ;
          false
        end in
      open_chapter  fst_item name
    end else if sn <= !chapter + !depth then begin (* sn > !chapter *)
      if !cur_level < !chapter then begin
        open_toc () ;
        open_chapter true ""
      end ;
      close_section sn ;
      open_section sn name
    end ;
    main lexbuf}
| "<!--CUT STYLE" ' '+ (alpha+ as style) ' '* "-->"
    {
     Section.set_style style ;
     main lexbuf 
    }
| "<!--CUT DEF" ' '+ (secname as name) ' '* (['0'-'9']+ as i_opt)?
    {let chapter = Section.value (String.uppercase name) in
    let depth = match i_opt with
    | None -> !depth 
    | Some s -> int_of_string s in
    skip_endcom lexbuf ;
    save_state chapter depth ;
    cur_level := Section.value "DOCUMENT" ;
    main lexbuf}
| "<!--SEC END" ' '* "-->" '\n'?
    {if !phase > 0 then begin
      match toc_style with
      | Both|Special when !out == !out_prefix ->
          out := CutOut.create_buff "out-buf"
      | _ -> ()
    end ;
    main lexbuf}
| "<!--CUT END" ' '* "-->" '\n'?
    {close_all () ;
      restore_state () ;
      main lexbuf}
| "<!--BEGIN" ' '+ "NOTES" ' '+ (secname as sec_notes)
    {skip_endcom lexbuf ;
    open_notes false (Section.value sec_notes) ;     
    main lexbuf}
| "<!--BEGIN" ' '+ "STICKYNOTES" ' '+ (secname as sec_notes)
    {skip_endcom lexbuf ;
    open_notes true (Section.value sec_notes) ;     
    main lexbuf}
| "<!--END" ' '+ "NOTES" ' '* "-->" '\n'?
    {if !otheroutname <> "" then
      close_notes ();
      main lexbuf}
| "<A" ' '+
    {if !phase > 0 then put (lexeme lexbuf) ;
    aargs lexbuf}
| "<!--HTML" ' '* "HEAD" ' '* "-->" '\n' ?
    {let head = save_html lexbuf in
    if !phase = 0 then
      html_head := head
    else
      CutOut.put !out head;
    main lexbuf}
| "<!--HTML" ' '* "FOOT" ' '* "-->" '\n' ?
    {let foot =  save_html lexbuf in
    if !phase = 0 then begin
      html_foot := foot
    end ;
    main lexbuf}
| "<!--FOOTER-->" '\n'?
    {if !phase = 0 then hevea_footer := true ;
     close_all () ;
     footer lexbuf}
| "<!DOCTYPE"  [^'>']* '>'
    {let lxm = lexeme lexbuf in
    if !phase = 0 then
      doctype := lxm
    else
      CutOut.put !out lxm;
    main lexbuf}
| "<HTML"  [^'>']* '>'
    {let lxm = lexeme lexbuf in
    if !phase = 0 then
      html := lxm
    else
      CutOut.put !out lxm;
    main lexbuf}
| "<BODY" [^'>']* '>'
    {let lxm = lexeme lexbuf in
    if !phase = 0 then
      body := lxm
    else begin
      CutOut.put !out lxm ;
      putlinks_start !out !outname
    end ;
    main lexbuf}
| "<HEAD" [^'>']* '>'
    {put (lexeme lexbuf);
      if !phase = 0 then begin
        if verbose > 0 then prerr_endline "Collect header" ;
        collect_header lexbuf
      end else begin
        repeat_header lexbuf
      end ;
      main lexbuf}
| "</BODY>" _ * 
    {let lxm = lexeme lexbuf in
    close_all () ;
    if !phase > 0 then begin
      close_top lxm
    end}
|  _ as lxm
    {if !phase > 0 then put_char lxm ;
    main lexbuf}
| eof
    {raise (Error ("No </BODY> tag in input file"))}

and save_html = parse
| "<!--END" ' '* ['A'-'Z']+ ' '* "-->" '\n'?
    {let s = CutOut.to_string html_buff in
    if verbose > 0 then
      prerr_endline ("save_html -> ``"^s^"''");
    s}
|  _
    {let lxm = lexeme_char lexbuf 0 in    
    CutOut.put_char html_buff lxm ;
    save_html lexbuf}
| eof
    {raise (Misc.Fatal ("End of file in save_html"))}

and collect_header = parse
| "</HEAD>"
    {finalize_header () ;
    if verbose > 0 then begin
      prerr_string "Header is: '" ;
      prerr_string !common_headers ;
      prerr_endline "'"
    end}
| '\n'? "<TITLE" [^'>']* '>'
    {skip_title lexbuf ; collect_header lexbuf}
| "<STYLE" blank+ "type" blank* '=' blank* '"' "text/css" '"' blank* '>'
    {collect_style lexbuf ;  collect_header lexbuf}
| _ as lxm
    {adjoin_to_header_char lxm;
    collect_header lexbuf}

and repeat_header = parse
| "</HEAD>" as lxm
    {put (!link_style) ; put lxm }
| "<STYLE" blank+ "type" blank* '=' blank* '"' "text/css" '"' blank* '>'
    {skip_style lexbuf ; repeat_header lexbuf}
| _ as lxm
    {put_char lxm ; repeat_header lexbuf}

and collect_style = parse
| "</STYLE>" '\n'? { () }
| _ as c
    { CutOut.put_char style_buff c ; collect_style lexbuf }

and skip_style = parse
| "</STYLE>" '\n'? { () }
| _ { skip_style lexbuf }

and skip_title = parse
|  "</TITLE>" '\n'? {()}
|  _          {skip_title lexbuf}
    
and footer = parse
    "</BODY>" _* as lxm 
    {if !phase > 0 then begin
      close_top lxm 
    end}
| _  as lxm {if !phase = 0 then begin CutOut.put_char hevea_footer_buff lxm end ;
       footer lexbuf}
| eof {raise (Misc.Fatal ("End of file in footer (no </BODY> tag)"))}


and tocline = parse
    "-->" '\n' ? {CutOut.to_string toc_buf}
| _
    {CutOut.put_char toc_buf (lexeme_char lexbuf 0) ;
      tocline lexbuf}


and flowline = parse
| "<ARG TITLE>"
    {let title = arg lexbuf in
    let _,b = flowline lexbuf in
    title,b}
| "<ARG FOOTER>"
    {let yes_no = arg lexbuf in
    let b = match yes_no with "YES" -> true | _ -> false in
    let title,_ = flowline lexbuf in
    title,b}
| "-->" '\n'? {"",true}
| eof {raise (Misc.Fatal "Unclosed comment")}
| _   {flowline lexbuf}

and getargs = parse
| "-->" '\n'? {[]}
| "<ARG" ' '* 
    {let name = argname lexbuf in
    let r = arg lexbuf in
    (name,r)::getargs lexbuf}
| eof {raise (Misc.Fatal "Unclosed comment")}
| _   {getargs lexbuf}

and argname = parse
| ['a'-'z''A'-'Z']* '>'
    {let lxm = lexeme lexbuf in
    String.sub lxm 0 (String.length lxm-1)}
| "" {raise (Misc.Fatal "ARG title")}

and arg = parse
| "</ARG>"  {CutOut.to_string arg_buf}
| _         {CutOut.put_char arg_buf (Lexing.lexeme_char lexbuf 0) ; arg lexbuf}
| eof       {raise (Misc.Fatal "Unclosed arg")}


and aargs = parse
| ("name"|"NAME") ' '* '=' ' '*
  {if !phase = 0 then begin
    let name = refname lexbuf in
    Cross.add name !outname
  end else
    put (lexeme lexbuf) ;
  aargs lexbuf}
| ("href"|"HREF") ' '* '=' ' '*
   {if !phase > 0 then begin
      let lxm = lexeme lexbuf in
      let name = refname lexbuf in
      try
        let newname =
          if String.length name > 0 && String.get name 0 = '#' then
            Cross.fullname
	      check_changed
	      !outname (String.sub name 1 (String.length name-1))
          else name in
        put lxm ;
        put "\"" ;
        put newname ;
        put "\""
      with Not_found -> ()
    end ;
    aargs lexbuf}
| '>'
  {if !phase > 0 then put_char '>' ;
  main lexbuf}
| _
  {if !phase > 0 then put_char (lexeme_char lexbuf 0) ;
  aargs lexbuf}
| eof
  {raise (Error "Bad <A ...> tag")}

and refname = parse
|  '"' ([^'"']* as name) '"'
|  ''' ([^''']* as name) '''
| (['a'-'z''A'-'Z''0'-'9''.''_''-']+ as name)
   { name }
| "" {raise (Error "Bad reference name syntax")}

and skip_blanks = parse
  ' '* {()}

and skip_endcom  = parse
  ' '* "-->" '\n'? {()}
| ""               {raise (Error "Bad HTML comment syntax")}
and skip_aref = parse
  "</A>" {()}
| _      {skip_aref lexbuf}  

{

let do_lex lexbuff =
  main lexbuff ;
  !some_links

end
} 

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

let header = "$Id: foot.ml,v 1.17 2001-01-05 13:59:57 maranget Exp $" 
open Parse_opts

let some = ref false
;;


let anchor = ref 0
;;

let mark_to_anchor = Hashtbl.create 17
and anchor_to_note = Hashtbl.create 17
;;

(*
let hot_start () =
  some := false ;
  anchor := 0 ;
  Hashtbl.clear mark_to_anchor ;
  Hashtbl.clear anchor_to_note
*)
type saved =
    (int, int) Hashtbl.t
  * (int, int * string * string) Hashtbl.t * int * bool

let checkpoint () =
   Misc.clone_hashtbl mark_to_anchor,  Misc.clone_hashtbl anchor_to_note,
  !anchor, !some

and hot_start (t1,t2,i,b) =
  Misc.copy_hashtbl t1 mark_to_anchor ;
  Misc.copy_hashtbl t2 anchor_to_note ;
  anchor := i ;
  some := b

let step_anchor mark =
  incr anchor ;
  Hashtbl.remove mark_to_anchor mark ;
  Hashtbl.add mark_to_anchor mark !anchor
;;

let get_anchor mark =
  let r =
    try Hashtbl.find mark_to_anchor mark
    with Not_found -> begin
      step_anchor mark ;
      !anchor
    end in
  r
;;
  
let register mark themark text =
  some := true ;
  let anchor = get_anchor mark in
  begin try
    let _ = Hashtbl.find anchor_to_note anchor in    
    Misc.warning "erasing previous footnote" ;
    Hashtbl.remove  anchor_to_note anchor
  with Not_found -> ()
  end ;
  Hashtbl.add anchor_to_note anchor (mark,themark,text)
;;


let flush lexer sec_notes sec_here =
  if !some && Section.value sec_here <= Section.value sec_notes then begin
    some := false ;
    lexer ("\\begin{thefootnotes}{"^sec_notes^"}") ;
    let all = ref [] in
    Hashtbl.iter
      (fun anchor (mark,themark,text) ->
        all := ((mark,anchor),(themark,text)) :: !all)
      anchor_to_note ;
    all := Sort.list
         (fun ((m1,a1),_) ((m2,a2),_) ->
           (a1 < a2) ||
           ((a1 = a2) && (m1 <= m2))) !all ;
    List.iter
      (fun ((_,anchor),(themark,text)) ->
        lexer
          ("\\item["^
           "\\@noteref{text}{note}{"^
           string_of_int anchor^
           "}{\\@print{"^themark^"}}]") ;
        lexer ("\\@print{"^text^"\n}"))
      !all ;
    lexer "\\end{thefootnotes}" ;
    Hashtbl.clear mark_to_anchor ;
    Hashtbl.clear anchor_to_note ;
  end
;;


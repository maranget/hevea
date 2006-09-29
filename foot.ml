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

let header = "$Id: foot.ml,v 1.22 2006-09-29 13:53:59 maranget Exp $" 
open Parse_opts

let some = ref false
;;


let anchor = ref 0
;;

let mark_to_anchor = Hashtbl.create 17
and anchor_to_note = Hashtbl.create 17
;;

let fst_stack = Stack.create_init "fst_stack" 0
and some_stack = Stack.create "some_stack"

type saved =
    (int, int) Hashtbl.t
  * (int, int * string * string) Hashtbl.t * int * bool
  * int Stack.saved * bool Stack.saved


let checkpoint () =
  Hashtbl.copy mark_to_anchor,
  Hashtbl.copy anchor_to_note,
  !anchor, !some,
  Stack.save fst_stack, Stack.save some_stack

and hot_start (t1,t2,i,b,fst_saved,some_saved) =
  Misc.copy_int_hashtbl t1 mark_to_anchor ;
  Misc.copy_int_hashtbl t2 anchor_to_note ;
  anchor := i ;
  some := b ;
  Stack.restore fst_stack fst_saved ;
  Stack.restore some_stack some_saved ;
  ()

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


let sub_notes () =
  Stack.push fst_stack !anchor ;
  Stack.push some_stack !some ;
  some := false


let flush sticky lexer sec_notes sec_here =
  if !some && Section.value sec_here <= Section.value sec_notes then begin
(*
    Misc.warning
      (Printf.sprintf "NOTES %s (%s)" sec_here sec_notes) ;
*)
    some := false ;
    let fst = Stack.top fst_stack in
    lexer
      ("\\begin{thefootnotes}" ^
       (if sticky then "[STICKY]" else "") ^
       "{"^sec_here^"}") ;
    let all = ref [] in
    Hashtbl.iter
      (fun anchor (mark,themark,text) ->
        if anchor > fst then
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
    List.iter
      (fun ((m,a),_) ->
        Hashtbl.remove mark_to_anchor m ;
        Hashtbl.remove anchor_to_note a)
      !all
  end
;;

let end_notes () =
  some := Stack.pop some_stack ;
  ignore (Stack.pop fst_stack)

  

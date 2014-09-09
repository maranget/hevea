(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Additions to the standard Lexing module *)

open Lexing
open Printf

let verbose = false

(* Avoid one string copy *)
let zero_pos = {
  pos_fname = "";
  pos_lnum = 1;
  pos_bol = 0;
  pos_cnum = 0;
};;

let from_string s =
  { refill_buff = (fun lexbuf -> lexbuf.lex_eof_reached <- true);
    lex_buffer = Bytes.of_string s ;
    lex_buffer_len = String.length s;
    lex_abs_pos = 0;
    lex_start_pos = 0;
    lex_curr_pos = 0;
    lex_last_pos = 0;
    lex_last_action = 0;
    lex_mem = [||];
    lex_eof_reached = true;
    lex_start_p = zero_pos;
    lex_curr_p = zero_pos;
  }


type lex_list =
    { mutable pos : int ;
      mutable xs : string list ; }

let pp_lex_list chan p =
  fprintf chan "pos=%i, xs=[%s]" p.pos (String.concat "|" p.xs)
  
let show s os len =
  let s1 = String.sub s 0 os
  and s2 = String.sub s os len
  and s3 = String.sub s len (String.length s-len) in
  sprintf "[%s-%s-%s]" s1 s2 s3

let vblit src os dst od len =
  if verbose && os > 0 && len <> String.length src then
    eprintf "BLIT: %s\n" (show src os len) ;
  String.unsafe_blit src os dst od len

let refill_from_list p buff =
  (*
    xs : list of string,
    pos: starting position in xs
    rem: nchars that remains,
    r: nchars copied and result.
  *)
  let rec do_rec xs pos rem r = match xs with
  | [] -> p.xs <- [] ; p.pos <- 0 ; r
  | x::ys ->
      let len = String.length x in
      if len-pos < rem then begin (* copy all x and recurse *)
        let ncpy = len-pos in
        vblit x pos buff r ncpy ;
        do_rec ys 0 (rem-ncpy) (r+ncpy)
     end  else begin (* stop now *)
        vblit x pos buff r rem ;
        p.pos <- pos+rem ;
        p.xs <- xs ;
        r+rem
      end in
  (fun n ->
    if verbose then
      eprintf "REFILL: n=%i %a\n" n pp_lex_list p ;
    let r = do_rec p.xs p.pos n 0 in
    if verbose then
      eprintf "DONE: r=%i %a\n" r pp_lex_list p ;
    r)
    

let from_list = function
  | [] -> from_string ""
  | [s] -> from_string s
  | xs ->
      Lexing.from_function (refill_from_list { pos = 0; xs = xs; })

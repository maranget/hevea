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

open Printf
open Lexing
module S = SimpleRope

let _header = "$Id: out.ml,v 1.25 2012-06-05 14:55:39 maranget Exp $"
let verbose = ref 0

type t =
  | Rope of S.t ref
  | Chan of out_channel
  | Null

let dump chan r = S.output chan r

let debug chan = function
  | Rope out ->
      output_char chan '*';
      dump chan !out ;
      output_char chan '*';
      ()
  | Chan _ -> output_string chan "*CHAN*"
  | Null -> output_string chan "*NULL*"

let free _ = ()

let create_buff () =  Rope (ref S.empty)

let create_chan chan = Chan chan

let create_null () = Null

let is_null = ( = ) Null

and is_empty = function
  | Rope r -> S.length !r = 0
  | _ -> false

let reset = function
  | Rope r -> r := S.empty
  | _ -> raise (Misc.fatal "Out.reset")

let get_pos = function
  | Rope r -> S.length !r
  | _ -> 0

let erase_start n = function
  | Rope r -> r := S.sub !r n (S.length !r - n)
  | _ -> raise (Misc.fatal "Out.erase_start")

let put out s = match out with
  | Null -> ()
  | Chan chan -> output_string chan s
  | Rope r -> r := S.append !r (S.of_string s)

(* To be used only in a lexer action *)
let blit out lexbuf =
  put out (lexeme lexbuf)

let put_char out c =
  match out with
  | Null -> ()
  | Chan chan -> output_char chan c
  | Rope r -> r := S.append_char !r c


let flush = function
  | Chan chan -> flush chan
  | _ -> ()

let iter f = function
  | Null -> ()
  | Chan _ -> raise (Misc.fatal "Out.iter")
  | Rope r -> S.iter_range f !r 0 (S.length !r)

let iter_next f = function
  | Null -> ()
  | Chan _ -> raise (Misc.fatal "Out.iter_next")
  | Rope r ->
      let rec do_rec next =
        let c = next () in
        if c <> -1 then begin
          f (Char.chr c) next;
          do_rec next
        end in
      do_rec
        (let k = ref 0 in
         fun () ->
           let i = !k in
           if i >= S.length !r then -1
           else begin
             incr k;
             Char.code (S.get !r i)
           end)

let as_string r = S.to_string r

let to_string = function
  | Rope r ->
      let s = as_string !r in
      r := S.empty ;
      s
  | _ -> raise (Misc.fatal "Out.to_string")

let to_chan chan = function
  | Rope r ->
      dump chan !r ;
      r := S.empty
  | _ -> raise (Misc.fatal "Out.to_chan")

let hidden_copy from_rope to_buff = match to_buff with
  | Null -> ()
  | Rope r -> r := S.append !r from_rope
  | Chan chan -> dump chan from_rope


let copy from_buff to_buff = match from_buff with
  | Rope r -> hidden_copy !r to_buff
  | _ -> raise (Misc.fatal "Out.copy")

let as_string = function
  | Rope r ->
      let s = 
        S.print Format.str_formatter !r;
        Format.flush_str_formatter () in
      eprintf "AS STRING: <%s>\n%!" s;
      s
  | _ -> raise (Misc.fatal "Out.as_string")


let copy_fun f from_buff to_buff =
  if !verbose > 2 then begin
    prerr_string "copy fun from buff";
    debug stderr from_buff;
    prerr_newline ();
  end;
  match from_buff with
    | Rope _ as r ->
        put to_buff (f (as_string r))
    | _ -> raise (Misc.fatal "Out.copy_fun")

let copy_no_tag from_buff to_buff =
  if !verbose > 2 then begin
    prerr_string "copy no tag from buff";
    debug stderr from_buff;
    prerr_newline ();
  end;
  match from_buff with
    | Rope from -> begin
      try let i = S.index !from '>' in
          let j =
            if is_empty from_buff then i + 1
            else S.rindex !from '<' in
          hidden_copy (S.sub !from (i+1) (j-i-1)) to_buff ;
          if !verbose > 2 then begin
            prerr_string "copy no tag to_buff";
            debug stderr to_buff;
            prerr_newline ()
          end
      with Not_found -> raise (Misc.fatal "Out.copy_no_tag, no tag found")
    end
    | _ -> raise (Misc.fatal "Out.copy_no_tag")

let close = function
  | Chan c -> close_out c
  | _ -> ()

let is_space c  = match c with
|' '|'\n'|'\r'|'\t' -> true
| _ -> false

let unskip = function
  | Rope r -> r := S.erase !r is_space
  | _ -> ()

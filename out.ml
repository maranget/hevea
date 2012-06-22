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

open Lexing
open Rope

let _header = "$Id: out.ml,v 1.25 2012-06-05 14:55:39 maranget Exp $"
let verbose = ref 0

type t =
  | Rope of S.t ref
  | Chan of out_channel
  | Null

let debug chan = function
  | Rope out ->
      output_char chan '*';
      S.print (Format.formatter_of_out_channel chan) !out;
      output_char chan '*'
  | Chan _ -> output_string chan "*CHAN*"
  | Null -> output_string chan "*NULL*"

(* Here for backward-compatibility of the interface but no longer meaningful *)
let free _ = ()

let create_buff () =
  Rope (ref (S.of_string ""))

let create_chan chan = Chan chan

let create_null () = Null

let is_null = ( = ) Null

and is_empty = function
  | Rope r -> !r = S.empty
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

let put out s =
  match out with
    | Null -> ()
    | Chan chan -> output_string chan s
    | Rope r -> r := S.append !r (S.of_string s)

(* To be used only in a lexer action *)
let blit out lexbuf = match out with
  | Null -> ()
  | Chan chan -> output_string chan (lexeme lexbuf)
  | Rope _ -> put out (lexeme lexbuf)

let put_char out c = match out with
  | Null -> ()
  | Chan chan -> Pervasives.output_char chan c
  | Rope r -> r := S.insert_char !r (S.length !r) c

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
           if i > S.length !r then -1
           else begin
             incr k;
             Char.code (S.get !r i)
           end)

let to_string = function
  | Rope r ->
      S.print Format.str_formatter !r;
      Format.flush_str_formatter ()
  | _ -> raise (Misc.fatal "Out.to_string")

let to_chan chan = function
  | Rope r -> S.print (Format.formatter_of_out_channel chan) !r
  | _ -> raise (Misc.fatal "Out.to_chan")

let hidden_copy from_rope to_buff i l = match to_buff with
  | Null -> ()
  | Rope r -> r := S.append !r (S.sub from_rope i l)
  | Chan chan ->
      S.print (Format.formatter_of_out_channel chan) (S.sub from_rope i l)

let copy from_buff to_buff =
  match from_buff with
    | Rope r -> hidden_copy !r to_buff 0 (S.length !r)
    | _ -> raise (Misc.fatal "Out.copy")

let copy_fun f from_buff to_buff = match from_buff with
  | Rope _ as r ->
      put to_buff (f (to_string r))
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
          hidden_copy !from to_buff (i + 1) (j - i - 1);
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

let is_space x = List.mem x [' '; '\n'; '\r'; '\t']

let unskip = function
  | Rope r ->
      while S.length !r > 0 && is_space (S.get !r (S.length !r - 1)) do
        r := S.delete !r (S.length !r - 1)
      done
  | _ -> ()

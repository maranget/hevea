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

let header = "$Id: out.ml,v 1.4 1998-07-21 11:18:40 maranget Exp $" 
type buff = {
  mutable buff : string;
  mutable bp : int;
}
;;

type t = Buff of buff | Chan of out_channel | Null
;;


let create_buff () = Buff {buff = String.create 128 ; bp = 0}
and create_chan chan = Chan chan
and create_null () = Null
;;

let reset = function
  Buff b -> b.bp <- 0
| _      -> failwith "Out.reset"
;;

let realloc out =
  let new_b = String.create (2*String.length out.buff) in
  String.blit out.buff 0 new_b 0 out.bp ;
  out.buff <- new_b
;;

let rec put out s = match out with
  (Buff out) as b ->
    let l = String.length s in
    if out.bp + l < String.length out.buff then begin
      String.blit s 0 out.buff out.bp l ;
      out.bp <- out.bp + l
    end else begin
      realloc out ;
      put b s
    end
| Chan chan -> output_string chan s
| Null -> ()
;;

let rec put_char out c = match out with
  Buff out as b ->
    if out.bp + 1 < String.length out.buff then begin
      String.set out.buff out.bp c ;
      out.bp <- out.bp + 1
    end else begin
      realloc out ;
      put_char b c
    end
| Chan chan ->
   output_char chan c
| Null -> ()
;;

let flush = function
  Chan chan -> flush chan
| _         -> ()
;;

let to_string out = match out with
  Buff out ->
    let r = String.sub out.buff 0 out.bp in
    out.bp <- 0 ; r
| _ -> failwith "Out.to_string"
;;

let to_chan chan out = match out with
  Buff out ->
    output chan out.buff 0 out.bp ;
    out.bp <- 0
| _  -> failwith "to_chan"
;;

let debug chan out = match out with
  Buff out ->
   output_char chan '*' ;
   output chan out.buff 0 out.bp ;
   output_char chan '*'
| Chan _   ->
   output_string chan "*CHAN*"
| Null ->
   output_string chan "*NULL*"
;;

let copy from_buf to_buf =
(*
  prerr_string "Copy: ";
  debug stderr from_buf ;
  prerr_newline () ;
*)
match from_buf with
  Buff from -> begin match to_buf with
     Chan chan -> output chan from.buff 0 from.bp
   | Buff out   ->
        while out.bp + from.bp >= String.length out.buff do
          realloc out
        done ;
        String.blit from.buff 0 out.buff out.bp from.bp ;
        out.bp <- out.bp + from.bp        
   | Null -> ()
   end
| _ -> failwith "copy"
;;
          
  

let close = function
| Chan c -> close_out c
| _ -> ()
;;

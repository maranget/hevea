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

let verbose = ref 0

module type Config = sig
  val small_length : int
end

module type S = sig
  type t

  val create_buff : unit -> t
  val create_chan : out_channel -> t
  val create_null : unit -> t
  val is_null : t -> bool
  val is_empty : t -> bool

  val reset : t -> unit

  val put : t -> string -> unit
  val blit : t -> Lexing.lexbuf -> unit
  val put_char : t -> char -> unit
  val flush : t -> unit
  val get_pos : t -> int
  val erase_start : int -> t -> unit

  val iter : (char -> unit) -> t -> unit
  val iter_next : (char -> (unit -> int) -> unit) -> t -> unit
  val to_string : t -> string
  val to_list : t -> string list
  val to_chan : out_channel -> t -> unit
  val copy : t -> t -> unit
  val copy_fun : (string -> string) -> t -> t -> unit
  val copy_no_tag : t -> t -> unit
  val close : t -> unit

  val debug : out_channel -> t -> unit
  val unskip : t -> unit
end

module Make(C:Config) = struct

  module S = SimpleRope.Make(C)

(***************************************************)
(* Rope with a front buffer of size C.small_length *)
(***************************************************)

  let max_sz = C.small_length

  type buff =
      { mutable b : string ; mutable p : int ;
        mutable sz : int ; mutable r : S.t; }

  let start_sz = min 16 max_sz

  let alloc_buff () =
    { b = String.create start_sz ; p = 0 ; sz=start_sz; r = S.empty ; }

  let dump_buff chan b =
    S.output chan b.r ;
    output chan b.b 0 b.p

  let reset_buff b = b.p <- 0 ; b.r <- S.empty

  let length_buff b = b.p + S.length b.r

  let to_string_buff b =
    let r = String.create (length_buff b) in
    S.blit b.r r 0  ;
    String.unsafe_blit b.b 0 r (S.length b.r) b.p ;
    r

  let do_flush_buff b =
    let s = String.create b.p in
    String.unsafe_blit b.b 0 s 0 b.p ;
    b.r <- S.append_string b.r s ;
    b.p <- 0

  let flush_buff b = if b.p > 0 then do_flush_buff b

  let realloc b =
    let nsz = 2 * b.sz in
    let nbuff = String.create nsz in
    String.unsafe_blit b.b 0 nbuff 0 b.p ;
    b.b <- nbuff ; b.sz <- nsz

  let rec vput_buff b s pos len =
    if b.p + len < b.sz then begin
      String.unsafe_blit s pos b.b b.p len ;
      b.p <- b.p + len
    end else if b.sz < max_sz then begin
      realloc b ;
      vput_buff b s pos len
    end else if b.p = 0 then
      let bsz = String.create b.sz in
      String.unsafe_blit s pos bsz 0 b.sz ;      
      b.r <- S.append_string b.r bsz ;
      vput_buff b s (pos+b.sz) (len-b.sz)
    else begin
      let tr = b.sz-b.p in
      String.unsafe_blit s pos b.b b.p tr ;
      b.p <- b.p+tr ;
      do_flush_buff b ;
      vput_buff b s (pos+tr) (len-tr)
    end


  let put_buff b s pos len =
    if !verbose > 2 then
      eprintf "PUT '%s'-> '%a'\n%a\n"
        (String.sub s pos len) dump_buff b S.debug b.r ;
    vput_buff b s pos len ;
    if !verbose > 2 then
    eprintf "PUT OVER '%s'-> '%a'\n%a\n"
      (String.sub s pos len) dump_buff b S.debug b.r ;
    ()

  let put_buff_char b c =
    if b.p >= b.sz then begin
      if b.sz < max_sz then realloc b
      else do_flush_buff b
    end ;
    String.unsafe_set b.b b.p c ;
    b.p <- b.p + 1

  let get_buff b k =
    let len = S.length b.r in
    if k < len then S.get b.r k
    else String.unsafe_get b.b (k-len)

  (* Append src at the end of dst *)
  let copy_buff src dst = 
    flush_buff dst ;
    dst.r <- S.append dst.r src.r ;
    put_buff dst src.b 0 src.p

(*******************)

  type t =
    | Rope of buff
    | Chan of out_channel
    | Null

  let debug chan = function
    | Rope out ->
        output_char chan '*';
        dump_buff chan out ;
        output_char chan '*';
        ()
    | Chan _ -> output_string chan "*CHAN*"
    | Null -> output_string chan "*NULL*"

  let create_buff () =  Rope (alloc_buff ())
  let create_chan chan = Chan chan
  let create_null () = Null

  let is_null = ( = ) Null

  and is_empty = function
    | Rope b -> b.p = 0 && S.length b.r = 0
    | _ -> false

  let reset = function
    | Rope b -> b.p <- 0 ; b.r <- S.empty
    | _ -> raise (Misc.fatal "Out.reset")

  let get_pos = function
    | Rope b -> length_buff b
    | _ -> 0

  let erase_start n = function
    | Rope b ->
        flush_buff b ;
        b.r <- S.sub b.r n (S.length b.r - n)
    | _ -> raise (Misc.fatal "Out.erase_start")

  let put out s = match out with
  | Null -> ()
  | Chan chan -> output_string chan s
  | Rope b -> put_buff b s 0 (String.length s)

(* To be used only in a lexer action *)
  let blit out lexbuf =
    let open  Lexing in
    match out with
    | Rope b ->
        let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
        put_buff b lexbuf.lex_buffer lexbuf.lex_start_pos len
    | Chan chan ->
        let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
        output chan lexbuf.lex_buffer lexbuf.lex_start_pos len
    | Null -> ()

  let put_char out c =
    match out with
    | Null -> ()
    | Chan chan -> output_char chan c
    | Rope b -> put_buff_char b c


  let flush = function
    | Chan chan -> flush chan
    | _ -> ()

  let iter f = function
    | Null -> ()
    | Chan _ -> raise (Misc.fatal "Out.iter")
    | Rope  b ->
        S.iter f b.r ;
        let bb = b.b in
        for k = 0 to b.p-1 do f (String.unsafe_get bb k) done ;
        ()

  let iter_next f = function
    | Null -> ()
    | Chan _ -> raise (Misc.fatal "Out.iter_next")
    | Rope b ->

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
            if i >= length_buff b then -1
            else begin
              incr k;
              Char.code (get_buff b i)
            end)

  let to_string = function
    | Rope b ->
        let s = to_string_buff b in
        reset_buff b ;
        s
    | _ -> raise (Misc.fatal "Out.to_string")

  let to_list = function
    | Rope b ->
        let xs =
          if b.p > 0 then [String.sub b.b 0 b.p]
          else [] in
        let xs = S.to_list_append b.r xs in
        if !verbose > 2 then begin
          match xs with
          | []|[_] -> ()
          | _ ->
              eprintf "to_list: %s\n%!"
                (String.concat " "
                   (List.map
                      (fun s -> sprintf "\"%s\"" (String.escaped s))
                      xs))
        end ;
        reset_buff b ;
        xs
    | _ -> raise (Misc.fatal "Out.to_list")

  let to_chan chan = function
    | Rope b ->
        dump_buff chan b ;
        reset_buff b
    | _ -> raise (Misc.fatal "Out.to_chan")

  let hidden_copy from_rope to_buff = match to_buff with
  | Null -> ()
  | Rope b -> copy_buff from_rope b
  | Chan chan -> dump_buff chan from_rope

  let copy from_buff to_buff = match from_buff with
  | Rope b -> hidden_copy b to_buff
  | _ -> raise (Misc.fatal "Out.copy")

  let copy_fun f from_buff to_buff =  match from_buff with
  | Rope b ->
      put to_buff (f (to_string_buff b))
  | _ -> raise (Misc.fatal "Out.copy_fun")

  let copy_no_tag from_buff to_buff =
    if !verbose > 2 then begin
      prerr_string "copy no tag from buff";
      debug stderr from_buff;
      prerr_newline ();
    end;
    match from_buff with
    | Rope src -> begin
        flush_buff src ;
        try
          let i = S.index src.r '>' in
          let j =
            if is_empty from_buff then i + 1
            else S.rindex src.r '<' in
          src.r <- S.sub src.r (i+1) (j-i-1) ;
          hidden_copy src to_buff ;
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
    | Rope b -> 
        flush_buff b ;
        b.r <- S.erase b.r is_space
    | _ -> ()
end

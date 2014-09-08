(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Printf
open Mysys

exception Failed

module type Config = sig
  val pess : bool
  val move : bool
  val small_length : int
end

module Make(C:Config) = struct

let input_protect f name =
  try
    let chan = open_in name in
    try
      let r = f chan in
      begin try close_in chan with _ -> () end ;
      r
    with e ->
      begin try close_in chan with _ -> () end ;
      raise e
  with
  | Sys_error _msg as e -> raise e

let output_protect f name =
  try
    let chan = open_out name in
    try
      let r = f chan in
      begin try close_out chan with _ -> () end ;
      r
    with e ->
      begin try close_out chan with _ -> () end ;
      raise e
  with
  | Sys_error _msg as e -> raise e

let lex_this vdef f name =
   try
    input_protect
      (fun input ->
        let lexbuf = Lexing.from_channel input in
        Location.set name lexbuf ;
        Emisc.reset () ;
        let r = f lexbuf in
        Location.restore () ;
        r)
      name
  with
  | Emisc.LexError s ->
      if !Emisc.verbose > 0 then
        output_char stderr '\n' ;
      Location.print_fullpos () ;
      Printf.fprintf stderr "Lexer error: %s\n" s ;
      Location.restore () ;
      vdef
  | Sys_error _ as e -> raise e
  | e ->
      Location.restore () ;
      raise e

let lex_this_out vdef f name_in name_out =
  try
    input_protect
      (fun input ->
        let lexbuf = Lexing.from_channel input in
        Location.set name_in lexbuf ;
        Emisc.reset () ;
        output_protect
          (fun out ->
            let r = f out lexbuf in
            Location.restore () ;
            r)
          name_out)
      name_in
  with
  | Emisc.LexError s ->
      if !Emisc.verbose > 0 then
        output_char stderr '\n' ;
      Location.print_fullpos () ;
      Printf.fprintf stderr "Lexer error: %s\n" s ;
      Location.restore () ;
      vdef
  | Sys_error _ as e -> raise e
  | e ->
      Location.restore () ;
      raise e
      
module Parse = Htmlparse.Make(C)

let process cls in_name input output =
  let rec do_rec lexbuf = match Parse.main cls lexbuf with
  | [] -> ()
  | ts ->
      if C.pess then
        Pp.trees output (Explode.trees ts)
      else
        Ultra.main output ts ;
      do_rec lexbuf in
  try
    let lexbuf = Lexing.from_channel input in
    Location.set in_name lexbuf ;
    Emisc.reset () ;
    do_rec lexbuf ;
    Location.restore () ;
    true
  with
  | Emisc.LexError s ->
      if !Emisc.verbose > 0 then
        output_char stderr '\n' ;
      Location.print_fullpos () ;
      Printf.fprintf stderr "Lexer error: %s\n" s ;
      Location.restore () ;
      false
  | Htmlparse.Error s ->
      if !Emisc.verbose > 0 then
        output_char stderr '\n' ;
      Location.print_fullpos () ;
      Printf.fprintf stderr "Parser error: %s\n" s ;
      Parse.ptop () ;
      Parse.reset () ;
      Location.restore () ;
      false
  | e ->
      Location.restore () ;
      raise e

let classes in_name input =
  try
    let lexbuf = Lexing.from_channel input in
    Location.set in_name lexbuf ;
    Emisc.reset () ;
    let cls = Parse.classes lexbuf in
    Location.restore () ;
    Some cls
  with
  | Emisc.LexError s ->
      if !Emisc.verbose > 0 then
        output_char stderr '\n' ;
      Location.print_fullpos () ;
      Printf.fprintf stderr "Lexer error: %s\n" s ;
      Location.restore () ;
      None
  | e ->
      Location.restore () ;
      raise e

let chop_extension name =
  try Filename.chop_extension name
  with Invalid_argument _ -> name

let do_mk_out name ext =
   Filename.concat
      (Filename.dirname name)
      (chop_extension (Filename.basename name) ^ ext)
  
let mk_out in_name = do_mk_out  in_name ".tmp"
and mk_esp in_name = do_mk_out  in_name ".esp"

let read_size name = input_protect in_channel_length name
(* Move output file to final destination if optimiser yields some gain *)
let check_output ok in_name out_name =
  let final_name =
    if ok then begin
      let size_in = read_size in_name
      and size_out = read_size out_name in
      let final_name =
        if size_in > size_out then begin
          let dst =
            if C.move then in_name
            else mk_esp in_name in
          rename out_name dst ;
          dst
        end else begin
          remove out_name ;
          in_name
        end in
      if !Emisc.verbose > 0  then begin
        eprintf "Optimized %s: %d -> %d, %.2f%%\n"
          final_name
          size_in size_out
          ((float (size_in-size_out) *. 100.0) /.
           float size_in)
      end ;
      final_name
    end else begin
      remove out_name ;
      in_name
    end in  
  final_name
  
  
let phase1 in_name =
  let out_name = mk_out in_name in
  begin try
    let input = open_in in_name in
    let cls =
      if C.pess then None
      else try classes in_name input
      with e -> close_in input ; raise e in
    close_in input ;
    let input = open_in in_name in
    let out =
      try open_out out_name
      with Sys_error _ as e ->
        close_in input ; raise e in
    let ok =
      try process cls in_name input out
      with e -> close_in input ; close_out out ; raise e in
    close_in input ;
    close_out out ;
    check_output ok in_name out_name
  with
  | Sys_error msg ->
      Printf.fprintf stderr "File error: %s\n" msg ;
      in_name
  | e ->
      remove out_name ;
      raise e
  end


let phase2 name =
  try
    let open Emisc in
    let sts = lex_this StringCount.empty  Lexstyle.get name in
    let m,_ =
      StringCount.fold
        (fun st nocc (m,n as p) ->          
          let withclass = 8 + String.length st + nocc * 4          
          and noclass =  nocc * String.length st in
          if withclass < noclass then
            let cl = sprintf "c%03i" n in
            StringMap.add st cl m,n+1
          else p)            
        sts (StringMap.empty,0) in
    if !Emisc.verbose > 1 then begin
      eprintf "New classes:\n" ;
      StringMap.iter
        (fun st cl -> Emisc.dump_class stderr cl st)
        m ;
      ()
    end ;
    let out_name = mk_out name in
    let ok =
      lex_this_out false
        (fun out lexbuf -> Lexstyle.set m out lexbuf)
        name out_name in
    check_output ok  name out_name    
  with
  | Sys_error msg ->
      Printf.fprintf stderr "File error: %s\n" msg ;
      name

let file name =
  if !Emisc.verbose > 0 then begin
    Printf.fprintf stderr "Optimizing file: %s...\n%!" name    
  end ;
  let name = phase1 name in
  if not C.pess then ignore (phase2 name)
end


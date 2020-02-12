(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

exception Fatal of string
exception NoSupport of string
exception Purposly of string
exception ScanError of string
exception UserError of string
exception EndInput
exception EndDocument
exception Close of string
exception CannotPut
exception EndOfLispComment of int (* QNC *)

let verbose = ref 0
and readverb = ref 0
and displayverb = ref false

let silent = ref false

let column_to_command s = "\\@"^s^"@"


let warning s =
  if not !silent || !verbose > 0 then begin
    Location.print_pos () ;
    prerr_string "Warning: " ;
    prerr_endline s
  end

let print_verb level s =
  if  !verbose > level then begin
    Location.print_pos () ;
    prerr_endline s
  end

let message s =
  if not !silent || !verbose > 0 then prerr_endline s

let fatal s = raise (Fatal s)
let not_supported s = raise (NoSupport s)


let rec rev_iter f = function
  | [] -> ()
  | x::rem -> rev_iter f rem ; f x

let copy_hashtbl from_table to_table =
  Hashtbl.clear to_table ;
  let module OString =
    struct
      type t = string
      let compare = Pervasives.compare
    end in
  let module Strings = Set.Make (OString) in
  let keys = ref Strings.empty in
  Hashtbl.iter 
    (fun key _ -> keys := Strings.add key !keys)
    from_table ;
  Strings.iter
    (fun key ->
      let vals = Hashtbl.find_all from_table key in
      rev_iter (Hashtbl.add to_table key) vals)
    !keys

let copy_int_hashtbl from_table to_table =
  Hashtbl.clear to_table ;
  let module OInt =
    struct
      type t = int
      let compare x y = x-y
    end in
  let module Ints = Set.Make (OInt) in
  let keys = ref Ints.empty in
  Hashtbl.iter 
    (fun key _ -> keys := Ints.add key !keys)
    from_table ;
  Ints.iter
    (fun key ->
      let vals = Hashtbl.find_all from_table key in
      rev_iter (Hashtbl.add to_table key) vals)
    !keys

let start_env env = "\\"^ env
and end_env env = "\\end"^env

type limits = Limits | NoLimits | IntLimits

let image_opt = ref None

let set_image_opt s = image_opt := Some s

let get_image_opt () = match !image_opt with
| None -> ""
| Some s -> s


let dump_index = ref false

type saved = string option

let checkpoint () = !image_opt

and hot_start so = image_opt := so

let next_of_string s =
  let len = String.length s
  and k = ref 0 in
  (fun () ->
    let i = !k in
    if i >= len then -1
    else begin
      incr k ;
      Char.code (String.unsafe_get s i)
    end)

let hexa c = match c with
  | '0'..'9' -> Char.code c - Char.code '0'
  | 'a'..'f' ->  10 + Char.code c - Char.code 'a'
  | 'A'..'F' ->  10 + Char.code c - Char.code 'A'
  | _ -> assert false

let hexa_code c1 c2 = 16 * hexa c1 + hexa c2

(* String utilities *)

let string_map f s =
  let len = String.length s in
  let out = Buffer.create len in
  for k = 0 to len-1 do
    Buffer.add_char out (f (String.get s k))
  done ;
  Buffer.contents out


(* Useful module signature, output of functors called for initialisation *)
module type Rien = sig val rien : unit end

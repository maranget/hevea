(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet MOSCOVA, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2006 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

exception Failed of string

let bad_char c =
  raise
    (Failed
       (Printf.sprintf "Bad character in unicode entity: %c" c))

let rec parse10 len str i r =
  if i < len then
    let c = match str.[i] with
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4
    | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9
    | c -> bad_char c in
    parse10 len str (i+1) (10*r+c)
  else
    r

let rec parse16 len str i r =
  if i < len then
    let c = match str.[i] with
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4
    | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9
    | 'a'|'A' -> 10 | 'b'|'B' -> 11 | 'c'|'C' -> 12
    | 'd'|'D' -> 13 | 'e'|'E' -> 14 | 'f'|'F' -> 15
    | c -> bad_char c in
    parse16 len str (i+1) (16*r+c)
  else
    r

let do_parse str =
  let len = String.length str in
  if len = 0 then begin 
    raise (Failed "Cannot parse unicode entity: empty")
  end else
    match str.[0] with
    | 'X'|'x' -> parse16 len str 1 0
    | '0'..'9' -> parse10 len str 0 0
    | c -> bad_char c
	
let parse str =
  try do_parse str
  with Failed msg ->
    Misc.warning msg ;
    0

exception CannotTranslate

let translate_ascii i =
  if i < 128 then Char.chr i
  else raise CannotTranslate

let translate_latin1 i =
  if i < 256 then Char.chr i
  else raise CannotTranslate

let translate_page p i =
  let page = i lsr 8 in
  if page = 0 then Char.chr i
  else if page = p then Char.chr (i land 0xFF)
  else raise CannotTranslate

let translate_fun = ref translate_ascii

let set_translate key =
  let f = match key with
  | "latin1" -> translate_latin1
  | _ ->
      Misc.warning
	(Printf.sprintf
	   "Unavailable encoding: %s, defaulting to ascii\n"
	   key) ;
      translate_ascii in
  translate_fun := f

let set_translate_page p =
  translate_fun := (fun i -> translate_page p i)
      
let translate i = !translate_fun i

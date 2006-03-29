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

(* Parse unicode chars given the HTML way *)
val parse : string -> int

(* Set translators from table in subdir 'mappings' *)
val set_output_translator : string -> unit
val set_input_translator : string -> unit
val set_translators : string -> unit

(* Translate for output *)
exception CannotTranslate
val translate_in : char -> int
val translate_out : int -> char

(* Diacritical marks *)
val grave : char -> int
val acute : char -> int
val circumflex : char -> int
val tilde : char -> int
val diaeresis : char -> int
val ring : char -> int
val cedilla : char -> int
val stroke : char -> int
val macron : char -> int
val caron : char -> int
val doubleacute : char -> int
val doublegrave : char -> int
val breve : char -> int
val dotabove : char -> int
val dotbelow : char -> int
val linebelow : char -> int
val ringabove : char -> int
val ogonek : char -> int
val circled : char -> int
val doublestruck : char -> int


(* Default rendering *)
val def_default : int -> string -> unit
val get_default : int -> string (* may raise Not_found *)

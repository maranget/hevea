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

type unichar

val show : unichar -> string

(* Parse unicode chars given the HTML way *)
val parse : string -> unichar

(* Set translators from table in subdir 'mappings' *)
val set_output_translator : string -> unit
val set_input_translator : string -> unit
val set_translators : string -> unit

(* Translate for output *)
exception CannotTranslate
val translate_in : char -> (unit -> int) -> unichar
val translate_out : unichar -> (char -> unit) -> unit

(* Diacritical marks *)
val grave : char -> unichar
val acute : char -> unichar
val circumflex : char -> unichar
val tilde : char -> unichar
val diaeresis : char -> unichar
val ring : char -> unichar
val cedilla : char -> unichar
val stroke : char -> unichar
val macron : char -> unichar
val caron : char -> unichar
val doubleacute : char -> unichar
val doublegrave : char -> unichar
val breve : char -> unichar
val dotabove : char -> unichar
val dotbelow : char -> unichar
val linebelow : char -> unichar
val ringabove : char -> unichar
val ogonek : char -> unichar
val circled : char -> unichar
val doublestruck : char -> unichar


(* Default rendering *)
val def_default : unichar -> string -> unit
val get_default : unichar -> string (* may raise Not_found *)

(* Output unicode char as html *)
val html_put : (string -> unit) -> (char -> unit) -> unichar -> unit

(* A few constants *)

val null : unichar
val space : unichar
val nbsp : unichar
val acute_alone : unichar
val grave_alone : unichar
val circum_alone : unichar
val diaeresis_alone : unichar
val cedilla_alone : unichar
val tilde_alone : unichar
val macron_alone : unichar
val doubleacute_alone : unichar
val breve_alone : unichar
val dotabove_alone : unichar
val dotbelow_alone : unichar
val linebelow_alone : unichar
val ogonek_alone : unichar
val ring_alone : unichar
val caron_alone : unichar
val circled_alone : unichar
val eszett : unichar
val iques : unichar
val iexcl : unichar
val minus : unichar
val endash : unichar
val emdash : unichar
val lquot : unichar
val rquot : unichar

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

(***************)
(* Some errors *)
(***************)
exception Fatal of string      (* A bug *)
exception NoSupport of string  (* A feature *)
exception Purposly of string   (* Explicit failure: \@heveafail command *)
exception ScanError of string  (* Error while reading input *)
exception UserError of string  (* Users should correct their code *)
exception Close of string      (* Specific user error: env nesting *)

(*******************)
(* Some non-errors *)
(*******************)
exception EndInput    (* Stop reading that file *)
exception EndDocument (* Document is over *)
exception EndOfLispComment of int (* QNC *)
exception CannotPut   (* Cannot ouput Unicode char (text mode) *)

val verbose : int ref
val readverb : int ref
val displayverb : bool ref
val silent : bool ref
val column_to_command : string -> string
val warning : string -> unit
val print_verb : int -> string -> unit
val message : string -> unit
val fatal : string -> 'a
val not_supported : string -> 'a

(* Copying hash tables, not very nice at present *)

val copy_hashtbl : (string, 'a) Hashtbl.t -> (string, 'a) Hashtbl.t -> unit
val copy_int_hashtbl : (int, 'a) Hashtbl.t -> (int, 'a) Hashtbl.t -> unit


val start_env : string -> string
val end_env : string -> string

type limits = Limits | NoLimits | IntLimits

val set_image_opt : string -> unit
val get_image_opt : unit -> string
val dump_index : bool ref

type saved
val checkpoint : unit -> saved
val hot_start : saved -> unit

(* Some kind of abstract buffer used by unicode in translator *)
val next_of_string : string -> (unit -> int)

(* two digit hexa -> int *)
val hexa_code : char -> char  -> int

(* Some string utilities (backward compatibility) *)
val string_map : (char -> char) -> string -> string

(* Useful module signature, output of functors called for initialisation *)
module type Rien = sig val rien : unit end

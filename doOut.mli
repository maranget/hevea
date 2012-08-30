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

val verbose : int ref

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

module Make(C:Config) : S

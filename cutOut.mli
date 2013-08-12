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

module type Config = sig
  val small_length : int
end

module Make(C:Config) : sig
type t

  val get_name : t -> string

  val create_buff : string -> t
  val create_null : unit -> t
  val create_chan : string -> t
  val close : t -> unit

  val put : t -> string -> unit
  val put_char : t -> char -> unit
  val is_empty : t -> bool
  val to_string : t -> string
  val to_chan : out_channel -> t -> unit
  val copy : t -> t -> unit
  val flush : t -> unit
  val debug : out_channel -> t -> unit
end

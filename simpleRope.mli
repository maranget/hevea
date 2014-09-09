(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Tibault Suzanne, Luc Maranget, projet Moscova, INRIA Rocquencourt  *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Simple rope data structure *)



module type Config = sig
  val small_length : int
end

module Make(C:Config) : sig

type t

(* Basics *)
  val length : t -> int
  val of_string : string -> t
  val singleton : char -> t
  val empty : t

(* Usual strings *)
  val append : t -> t -> t
  val append_string : t -> string -> t
  val append_char : t -> char -> t
  val sub : t -> int -> int -> t
  val get : t -> int -> char
  val iter : (char -> unit) -> t -> unit

(* Translations *)
  val output : out_channel -> t -> unit
  val debug : out_channel -> t -> unit
  val blit : t -> Bytes.t -> int -> unit
  val to_string : t -> string
  val to_list : t -> string list
  val to_list_append : t -> string list -> string list

(* Index function *)
  val index : t -> char -> int
  val rindex : t -> char -> int

(* Hevea specific: erase suffix that validates predicate *)
  val erase : t -> (char -> bool) -> t

end

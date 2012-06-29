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


(* Slight extension over Jean-Christophe Filliatre's Rope module *)

module type STRING = sig
  include Rope.STRING

  val index : t -> char -> int
  val index_from : t -> int -> char -> int

  val rindex : t -> char -> int
  val rindex_from : t -> int -> char -> int
end

(* Copied from Jean-Christophe's rope.mli *)
module type ROPE = sig

  include STRING

  val set : t -> int -> char -> t

  val delete : t -> int -> t

  val insert_char : t -> int -> char -> t

  val insert : t -> int -> t -> t

  module Cursor : sig

    type cursor

    val create : t -> int -> cursor

    val position : cursor -> int

    val to_rope : cursor -> t

    val move_forward : cursor -> int -> cursor

    val move_backward : cursor -> int -> cursor

    val move : cursor -> int -> cursor

    val get : cursor -> char

    val set : cursor -> char -> cursor

    val insert_char : cursor -> char -> cursor

    val insert : cursor -> t -> cursor

(* NYI
    val delete : cursor -> cursor
*)

    val print : Format.formatter -> cursor -> unit

  end
end


module S : sig

  include ROPE with type char = Char.t

  val of_string : string -> t

end


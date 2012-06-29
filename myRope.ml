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

module type CONTROL = Rope.CONTROL

module Make(S : STRING)(C : CONTROL) = struct

  include Rope.Make(S)(C)

  let rec index_from r i c = match r with
    | Str (s, ofs, _) -> S.index_from s (ofs + i) c - ofs
    | App (t1, t2, _, _) ->
        if i < length t1 then index_from t1 i c
        else index_from t2 (i - length t1) c + length t1

  let index r = index_from r 0

  let rec rindex_from r i c = match r with
    | Str (s, ofs, _) -> S.rindex_from s (ofs + i) c - ofs
    | App (t1, t2, _, _) ->
        if i < length t1 then rindex_from t1 i c
        else rindex_from t2 (i - length t1) c + length t1

  let rindex r = rindex_from r (length r - 1)
end

module S = Make(Rope.Str)(Rope.Control)

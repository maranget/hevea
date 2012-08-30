(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Map extension for counting occurrences *)

module type S = sig
  type key
  type count
  val empty : count
  val incr : key -> count -> count
  val fold : (key -> int -> 'b -> 'b) -> count -> 'b -> 'b
end


module Make (Ord : Map.OrderedType) : S with type key = Ord.t
